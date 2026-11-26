#!/usr/bin/env node
//
// crowd.mjs — a few simulated people browsing one Data UI app in real
// Chrome while you browse it too.
//
// One Chrome, one browser context per wanderer (its own profile and
// session). Each walks the real UI the way a person does: type tabs,
// paging, column sorts, opening rows (and the notes under them),
// typing in the search box, clicking chips, Home, Back / Forward, the
// odd reload — with human pauses in between. With --updates a wanderer
// also opens an item, clicks Edit, and puts a fresh "[1234] " in
// front of its title.
//
// Every request is logged with status and latency. The run ends with
// a report (also written under runs/): traffic, latency per endpoint,
// how often requests overlapped, anomalies, a verdict. Exit status 0
// for a clean run, 1 when something went wrong, 2 for a setup error.
//
// Usage, from the repo root
//
//   npm ci --prefix scripts/crowd     # once, for playwright-core
//   scripts/crowd/crowd.mjs --user NAME [options]
//
// The crowd drives this machine's own Google Chrome.
//
// Options
//
//   --users N        wanderers (default 1)
//   --minutes M      run length; 0 = until Ctrl-C (default 5)
//   --speed K        divides every pause: 1 = human, 5 = hurried
//                    (default 1)
//   --updates        include title-prefix updates (the account needs
//                    update rights on items)
//   --update-pct P   chance an action is an update, while possible
//                    (default 15)
//   --headed         show the Chrome windows (default: headless)
//   --base URL       the app (default http://localhost:8081)
//   --user NAME      the account every wanderer shares (required;
//                    "guest" needs no password)
//   --creds FILE     file with a "NAME:password" line for that
//                    account (required for every account but guest;
//                    CROWD_PASSWORD stands in for the file)
//   --seed N         repeat a run (default: random, printed)
//   --out DIR        where runs/ go (default: beside this script)
//   --slow-ms MS     latency that counts as slow (default 3000)
//   --timeout MS     how long an action waits for its requests before
//                    it is a hang (default 30000)
//   --allow-remote   required before --base may leave loopback
//   --hold-on-hang S on a hang, a wanderer sits still for S seconds
//                    before it reloads, so the server can be looked
//                    at while its requests are still open (default 0)
//   --together       everyone arrives at once (default: staggered)
//   --quiet          no per-action lines
//   --help
//
// Sessions
//
// Wanderer 1 logs in through the form once. The others start with its
// stored session, the way a returning visitor does. One account, one
// login: the server keeps one refresh token per account, so a second
// login retires the first. Browse under your own account, never this
// one.
//
// Left behind on the app
//
// Usage-log rows, and — with --updates — prefixed titles. Every update
// is in runs/<run>/updates.jsonl with its before and after.

import { chromium } from 'playwright-core';
import { parseArgs } from 'node:util';
import { setTimeout as sleep } from 'node:timers/promises';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { fileURLToPath } from 'node:url';
import fs from 'node:fs';
import path from 'node:path';

const HERE = path.dirname(fileURLToPath(import.meta.url));
// --help prints the comment block at the top of this file.
const HEADER = fs.readFileSync(fileURLToPath(import.meta.url), 'utf8')
  .split('\n').slice(1);
const BODY_AT = HEADER.findIndex((l) => !l.startsWith('//'));
const USAGE = HEADER.slice(0, BODY_AT)
  .map((l) => l.replace(/^\/\/ ?/, '')).join('\n').replace(/^\n/, '');

let o = {};
try {
  o = parseArgs({
    options: {
      users: { type: 'string' }, minutes: { type: 'string' },
      speed: { type: 'string' }, updates: { type: 'boolean' },
      'update-pct': { type: 'string' }, headed: { type: 'boolean' },
      base: { type: 'string' }, user: { type: 'string' },
      creds: { type: 'string' }, seed: { type: 'string' },
      out: { type: 'string' }, 'slow-ms': { type: 'string' },
      timeout: { type: 'string' }, 'allow-remote': { type: 'boolean' },
      'hold-on-hang': { type: 'string' }, together: { type: 'boolean' },
      quiet: { type: 'boolean' }, help: { type: 'boolean' },
    },
  }).values;
} catch (e) {
  console.error(`crowd: ${String(e.message).split('\n')[0]}`);
  process.exit(2);
}
if (o.help) {
  console.log(USAGE);
  process.exit(0);
}

function fail(msg) {
  console.error(`crowd: ${msg}`);
  process.exit(2);
}

function num(v, def, name, min = 0) {
  if (v === undefined) return def;
  const n = Number(v);
  if (!v.trim() || !Number.isFinite(n) || n < min) {
    fail(`--${name}: expected a number >= ${min}, got "${v}"`);
  }
  return n;
}

const BASE = new URL((o.base ?? 'http://localhost:8081').replace(/\/+$/, ''));
const LOOPBACK = ['localhost', '127.0.0.1', '[::1]'];
if (!LOOPBACK.includes(BASE.hostname) && !o['allow-remote']) {
  fail(`${BASE.origin} is not loopback. Pass --allow-remote to aim the ` +
    'crowd at another host (--updates rewrites titles there).');
}

const CFG = {
  users: Math.max(1, Math.round(num(o.users, 1, 'users', 1))),
  minutes: num(o.minutes, 5, 'minutes'),
  speed: num(o.speed, 1, 'speed', 0.05),
  updates: !!o.updates,
  updatePct: num(o['update-pct'], 15, 'update-pct', 0.01),
  headed: !!o.headed,
  user: o.user,
  creds: o.creds,          // no file given: undefined
  seed: o.seed === undefined
    ? Math.floor(Math.random() * 2 ** 31) : Number(o.seed),
  slowMs: num(o['slow-ms'], 3000, 'slow-ms', 1),
  timeoutMs: num(o.timeout, 30000, 'timeout', 1000),
  holdOnHang: num(o['hold-on-hang'], 0, 'hold-on-hang'),
  together: !!o.together,
  quiet: !!o.quiet,
};
if (o.seed !== undefined && !Number.isFinite(CFG.seed)) {
  fail(`--seed: not a number: "${o.seed}"`);
}
if (o.user === undefined || !o.user.trim()) {
  fail('the crowd needs an account: pass --user NAME ("guest" needs '
    + 'no password)');
}
// The password is found before anything starts, so a missing one is a
// setup error and not a run that fails at its first login.
CFG.password = CFG.user === 'guest' ? null : passwordFor(CFG.user);

const CLICK_MS = Math.min(8000, CFG.timeoutMs);
const T0 = Date.now();
const RUN = path.join(o.out ?? HERE, 'runs',
  `${new Date().toISOString().replace(/[:.]/g, '-')}`
  + `-u${CFG.users}-s${CFG.seed}`);
const SESSION_KEY = 'data-ui-session';
const TITLE_MAX = 120;
// Anomaly kinds that make the verdict PROBLEMS.
const COUNTED = new Set(['hang', 'http', 'netfail', 'dialog',
  'update-failed', 'action-error', 'fatal']);

const state = { stop: false, closing: false, deadline: Infinity };
const health = { ok: 0, fail: 0 };
const users = [];
const reqs = [];          // finished request records
const anomalies = [];
const titleUpdates = [];
let inFlight = 0;
let maxInFlight = 0;
let overlapStarts = 0;    // began while another was in flight
let behind3 = 0;          // began with 3 or more already in flight
let shots = 0;
let dropped = 0;          // requests a new document cut off

const run = promisify(execFile);
const hangCtx = [];       // open hangs, waiting to be described

class Hang extends Error {}
class Fatal extends Error {}

// ---- small helpers ------------------------------------------------------

const out = (s) => {
  if (!CFG.quiet) process.stdout.write(`${s}\n`);
};
const say = (s) => process.stdout.write(`${s}\n`);
const fmt = (n) => n.toLocaleString('en-US');
const secs = () => (Date.now() - T0) / 1000;
const mmss = () => {
  const s = Math.floor(secs());
  return `${Math.floor(s / 60)}:${String(s % 60).padStart(2, '0')}`;
};
const short = (p) => (p.length > 110 ? `${p.slice(0, 107)}...` : p);
const brief = (e) => String(e?.message ?? e).split('\n')[0].slice(0, 160);
const drop = (list, w) => {
  const i = list.indexOf(w);
  if (i >= 0) list.splice(i, 1);
};

function makeRng(seed) {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6D2B79F5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

const pick = (rng, arr) => arr[Math.floor(rng() * arr.length)];

function pickSome(rng, arr, n) {
  const copy = [...arr];
  const got = [];
  while (got.length < n && copy.length) {
    got.push(copy.splice(Math.floor(rng() * copy.length), 1)[0]);
  }
  return got;
}

function weighted(rng, pairs) {
  let x = rng() * pairs.reduce((s, p) => s + p[0], 0);
  for (const [w, v] of pairs) {
    if ((x -= w) < 0) return v;
  }
  return pairs[pairs.length - 1][1];
}

function normal(rng) {
  return Math.sqrt(-2 * Math.log(1 - rng()))
    * Math.cos(2 * Math.PI * rng());
}

// A human pause: lognormal around MEDIAN-MS, this wanderer's pace,
// divided by --speed.
async function pause(u, medianMs, sigma) {
  const ms = medianMs * Math.exp(sigma * normal(u.rng))
    * u.pace / CFG.speed;
  await sleep(Math.min(ms, 30000 / CFG.speed));
}

function hashQuery(page) {
  const h = new URL(page.url()).hash;
  const i = h.indexOf('?');
  return new URLSearchParams(i >= 0 ? h.slice(i + 1) : '');
}

const isList = (r) => r.api && r.p.startsWith('/api/list');
const tabsRow = (u) => u.page.locator('.app-header ~ div').first();
const formLoc = (u) => u.page.locator('form')
  .filter({ has: u.page.locator('h3', { hasText: /^(View|Edit|Add) / }) });
const mainRows = (u, type) => u.page
  .locator(`tr.row-clickable[aria-label^="Open ${type} record"]`);

// The app ignores a row click that lands on a control inside the row
// (a chip filters, a link navigates), so click a row on plain text:
// its first cell without one, or the row itself if it has none.
async function textCell(u, row) {
  const cell = row.locator(
    'td:not(:has(a, button, input, select, textarea, label, '
    + '[data-no-row-click]))').first();
  return (await cell.count()) ? cell : row;
}

// The password for NAME: the environment's, else the "NAME:password"
// line of the creds file. Called once, at startup.
function passwordFor(name) {
  if (process.env.CROWD_PASSWORD) return process.env.CROWD_PASSWORD;
  if (CFG.creds !== undefined) {
    try {
      const line = fs.readFileSync(CFG.creds, 'utf8').split('\n')
        .find((l) => l.startsWith(`${name}:`));
      if (line !== undefined) {
        return line.slice(name.length + 1).replace(/\r$/, '');
      }
    } catch { /* an unreadable file is as good as no line */ }
    fail(`no password for "${name}" in ${CFG.creds} — add a `
      + `"${name}:password" line there, or set CROWD_PASSWORD`);
  }
  fail(`account "${name}" needs a password — pass --creds FILE (a `
    + `"${name}:password" line in it) or set CROWD_PASSWORD`);
}

// ---- what the crowd notices ---------------------------------------------

function anomaly(kind, u, detail) {
  anomalies.push({ at: secs(), kind, user: u ? `u${u.n}` : '-',
    detail });
}

function shot(u, tag) {
  if (!u || shots >= 8 || state.closing) return;
  shots++;
  const file = path.join(RUN, `shot-${String(shots).padStart(2, '0')}`
    + `-${tag}-u${u.n}.png`);
  u.page.screenshot({ path: file }).catch(() => {});
}

// A request that never came back. Everything still in flight is
// marked, so its late failure is not mistaken for a plain abort.
// recover() describes the scene while the requests are still open.
function hang(u, where) {
  const stuck = [...u.live.values()];
  for (const r of stuck) r.stuck = true;
  const a = { at: secs(), kind: 'hang', user: `u${u.n}`,
    detail: `${where}: nothing came back within `
      + `${CFG.timeoutMs / 1000}s` };
  anomalies.push(a);
  hangCtx.push({ a, stuck, page: u.page });
  shot(u, 'hang');
  throw new Hang(`${where}: hang`);
}

// Was each hung request put on the wire? Chrome's own timing knows.
const onTheWire = (r) => (r.req?.timing().sendEnd ?? 0) > 0;

// The server's side of its TCP connections right now (Linux ss):
// how many in each state, and how many hold bytes the client has
// not taken yet.
async function socketSummary() {
  const port = BASE.port || (BASE.protocol === 'https:' ? '443' : '80');
  const { stdout } = await run('ss', ['-tnpH',
    `( sport = :${port} or dport = :${port} )`]);
  const byState = new Map();
  for (const line of stdout.split('\n')) {
    const f = line.trim().split(/\s+/);
    if (f.length < 4 || !f[3].endsWith(`:${port}`)) continue;
    const s = byState.get(f[0]) ?? { n: 0, unsent: 0, most: 0 };
    s.n++;
    if (Number(f[2]) > 0) s.unsent++;
    s.most = Math.max(s.most, Number(f[2]));
    byState.set(f[0], s);
  }
  if (!byState.size) return 'no connection to the server is open';
  return [...byState].map(([state, s]) => `${state} ${s.n}`
    + (s.unsent ? ` (${s.unsent} with data the client has not taken, `
      + `at most ${fmt(s.most)} bytes)` : '')).join(', ');
}

// Say what a hang looked like while it was still hanging.
async function describeHang({ a, stuck, page }) {
  const urls = stuck.map((r) => r.req?.url() ?? '');
  const ended = await page.evaluate((list) => list.map((url) => {
    const e = performance.getEntriesByName(url)[0];
    return e ? Math.round(e.responseEnd) : null;
  }), urls).catch(() => null);
  const items = stuck.map((r, i) => {
    const wire = onTheWire(r) ? 'sent' : 'never sent';
    const state = ended?.[i] > 0
      ? `finished in the page at ${ended[i]} ms` : 'unfinished';
    return `${r.m} ${short(r.p)} (${wire}, ${state}, `
      + `${((Date.now() - r.t0) / 1000).toFixed(1)}s)`;
  });
  a.detail += ` — ${items.join('; ') || 'no request was pending'}`;
  a.sockets = await socketSummary().catch((e) => `ss failed: ${brief(e)}`);
}

// Record an error an action raised. A Hang recorded itself.
function recordError(u, what, e) {
  if (e instanceof Hang) return;
  anomaly(e instanceof Fatal ? 'fatal' : 'action-error', u,
    `${what}: ${brief(e)}`);
}

// ---- request tracking ---------------------------------------------------

// waitFinish / waitStart resolve with the first request record that
// finishes / starts after the call and matches MATCH, or null at the
// timeout. Always arm one before the click that should cause it.
function waiter(list, u, match, ms) {
  let cancel = () => {};
  const promise = new Promise((resolve) => {
    const w = { since: Date.now(), match, resolve, timer: null };
    w.timer = setTimeout(() => {
      drop(u[list], w);
      resolve(null);
    }, ms);
    u[list].push(w);
    cancel = () => {
      clearTimeout(w.timer);
      drop(u[list], w);
    };
  });
  promise.cancel = cancel;
  return promise;
}

const waitFinish = (u, match, ms = CFG.timeoutMs) =>
  waiter('wf', u, match, ms);
const waitStart = (u, match, ms = CFG.timeoutMs) =>
  waiter('ws', u, match, ms);

function onStart(u, r) {
  const url = new URL(r.url());
  if (url.origin !== BASE.origin || url.pathname === '/health') return;
  const rec = {
    u: u.n, api: url.pathname.startsWith('/api/'), m: r.method(),
    p: url.pathname + url.search, type: url.searchParams.get('type'),
    t0: Date.now(), t1: 0, status: 0, err: null, res: null, req: r,
    behind: inFlight, stuck: false, hung: false,
  };
  u.live.set(r, rec);
  u.lastStart = rec.t0;
  if (inFlight++ > 0) overlapStarts++;
  if (inFlight > maxInFlight) maxInFlight = inFlight;
  if (inFlight - 1 >= 3) behind3++;
  for (const w of [...u.ws]) {
    if (w.match(rec)) {
      clearTimeout(w.timer);
      drop(u.ws, w);
      w.resolve(rec);
    }
  }
}

function onFinish(u, r, status, err, res) {
  const rec = u.live.get(r);
  if (!rec) return;
  u.live.delete(r);
  inFlight--;
  rec.t1 = Date.now();
  closeRec(u, rec, status, err, res);
}

// A request's end: wake whoever was waiting for it, then file it.
function closeRec(u, rec, status, err, res) {
  Object.assign(rec, { status, err, res });
  for (const w of [...u.wf]) {
    if (rec.t0 >= w.since && w.match(rec)) {
      clearTimeout(w.timer);
      drop(u.wf, w);
      w.resolve(rec);
    }
  }
  if (state.closing && !rec.stuck) return;
  reqs.push(rec);
  u.done++;
  if (rec.stuck) {
    rec.hung = true;      // already reported, by hang()
    return;
  }
  if (rec.ghost) return;
  const what = `${rec.m} ${short(rec.p)}`;
  if (status === 0) {
    if (err === 'net::ERR_ABORTED') rec.aborted = true;
    else anomaly('netfail', u, `${what}: ${err ?? 'failed'}`);
  } else if (status >= 400) {
    if (status === 401) rec.auth = true;
    else anomaly('http', u, `${status} ${what}`);
  } else if (rec.t1 - rec.t0 >= CFG.slowMs) {
    anomaly('slow', u, `${what} took ${rec.t1 - rec.t0} ms`);
  }
}

// Chrome does not report every request's end (seen after a navigation
// interrupted others). The page's own timing is the second opinion:
// what it says finished, finished.
async function resolveGhosts(u) {
  const old = [...u.live.entries()]
    .filter(([, rec]) => Date.now() - rec.t0 > 2000);
  if (!old.length) return;
  const seen = await u.page.evaluate((urls) => urls.map((url) => {
    const e = performance.getEntriesByName(url)[0];
    return e && e.responseEnd > 0
      ? [Math.round(e.responseEnd - e.startTime), e.responseStatus] : null;
  }), old.map(([r]) => r.url())).catch(() => null);
  if (!seen) return;
  old.forEach(([r, rec], i) => {
    if (!seen[i]) return;
    u.live.delete(r);
    inFlight--;
    rec.t1 = rec.t0 + seen[i][0];
    rec.ghost = true;
    closeRec(u, rec, seen[i][1] || 0, null, null);
  });
}

function attach(u) {
  const page = u.page;
  page.on('request', (r) => onStart(u, r));
  // A new document cuts off whatever the old one had in flight — and
  // Chrome then never reports those. Let them go when it starts.
  page.on('request', (r) => {
    if (!(r.isNavigationRequest() && r.frame() === page.mainFrame())) {
      return;
    }
    for (const [other, rec] of [...u.live]) {
      if (other === r) continue;
      u.live.delete(other);
      inFlight--;
      rec.dropped = true;
      dropped++;
      if (rec.stuck) rec.hung = true;
    }
  });
  page.on('requestfinished', async (r) => {
    const res = await r.response();
    onFinish(u, r, res?.status() ?? 0, null, res);
  });
  page.on('requestfailed', (r) =>
    onFinish(u, r, 0, r.failure()?.errorText ?? 'failed'));
  // A confirm is only ever "Delete Selected": never yes.
  page.on('dialog', (d) => {
    anomaly('dialog', u, `${d.type()}: ${d.message()}`);
    (d.type() === 'confirm' ? d.dismiss() : d.accept())
      .catch(() => {});
  });
  page.on('pageerror', (e) => anomaly('pageerror', u, brief(e)));
  page.on('console', (m) => {
    const text = m.text();
    // A failed response is already recorded with its status.
    if (m.type() === 'error'
      && !/^Failed to load resource: .*status of \d+/.test(text)) {
      anomaly('console', u, text);
    }
  });
}

// ---- waiting for the page -----------------------------------------------

// True once the wanderer's page has nothing in flight and has sent
// nothing for QUIET-MS; false at the timeout (the caller calls hang).
async function settle(u, quietMs = 400) {
  const deadline = Date.now() + CFG.timeoutMs;
  let asked = 0;          // when the page was last asked about strays
  for (;;) {
    if (u.live.size === 0 && Date.now() - u.lastStart >= quietMs) {
      return true;
    }
    if (Date.now() >= deadline) return false;
    if (u.live.size && Date.now() - asked > 1000) {
      asked = Date.now();
      await resolveGhosts(u);
    }
    await sleep(40);
  }
}

async function atRest(u, where, quietMs = 400) {
  if (!(await settle(u, quietMs))) hang(u, `after ${where}`);
}

async function done(u, rec, where) {
  if (!rec.t1) {
    await new Promise((resolve) => {
      const w = { since: 0, match: (x) => x === rec, resolve,
        timer: null };
      w.timer = setTimeout(() => {
        drop(u.wf, w);
        resolve();
      }, CFG.timeoutMs);
      u.wf.push(w);
    });
  }
  if (!rec.t1) hang(u, where);
}

function check(rec, u) {
  if (rec.status >= 400 && rec.status !== 401) {
    anomaly('http', u, `${rec.status} ${rec.m} ${short(rec.p)}`);
  }
}

// Click, and wait for the request it causes.
async function clickFor(u, locator, match, where, ms = CFG.timeoutMs) {
  const w = waitFinish(u, match, ms);
  try {
    await locator.click({ timeout: CLICK_MS });
  } catch (e) {
    w.cancel();
    throw e;
  }
  const rec = await w;
  if (!rec) {
    if (u.live.size) hang(u, where);
    throw new Error(`${where}: nothing was sent to the server`);
  }
  check(rec, u);
  return rec;
}

async function clickQuiet(locator) {
  await locator.click({ timeout: CLICK_MS }).catch(() => {});
}

// ---- sign-in ------------------------------------------------------------

async function login(u) {
  const page = u.page;
  await page.goto(`${BASE.origin}/`,
    { waitUntil: 'load', timeout: CFG.timeoutMs });
  const w = waitFinish(u, (r) => r.p.startsWith('/api/login'));
  try {
    if (CFG.user === 'guest') {
      await page.click('button:has-text("Continue as guest")',
        { timeout: CLICK_MS });
    } else {
      await page.fill('input[name="username"]', CFG.user,
        { timeout: CLICK_MS });
      await page.fill('input[name="password"]', CFG.password,
        { timeout: CLICK_MS });
      await page.click('form button[type="submit"]',
        { timeout: CLICK_MS });
    }
  } catch (e) {
    w.cancel();
    throw e;
  }
  const rec = await w;
  if (!rec) {
    if (u.live.size) hang(u, 'login');
    throw new Fatal(`login as ${CFG.user}: no response from `
      + BASE.origin);
  }
  const json = await rec.res?.json().catch(() => null);
  if (rec.status !== 200 || json?.status !== 'success') {
    throw new Fatal(`login as ${CFG.user} failed on ${BASE.origin} — `
      + `HTTP ${rec.status}`
      + (json?.error ? `, "${json.error}"` : '')
      + '. The account has to exist on this instance (the dev app on '
      + 'its own port keeps its own users).');
  }
  await page.waitForSelector('text=Logout', { timeout: CLICK_MS });
  await atRest(u, 'login');
}

// ---- actions ------------------------------------------------------------

async function clearSearch(u) {
  const btn = u.page.locator('button[title="Clear search"]');
  if (!(await btn.count())) return;
  const w = waitFinish(u, (r) => isList(r) && !r.p.includes('search='));
  await clickQuiet(btn.first());
  const rec = await w;
  if (rec) check(rec, u);
  await atRest(u, 'clear search');
}

async function clearFilters(u) {
  const btn = u.page.locator('button:has-text("Clear filters")').first();
  if (!(await btn.count())) return;
  const w = waitFinish(u, (r) => isList(r) && !r.p.includes('filters='));
  await clickQuiet(btn);
  const rec = await w;
  if (rec) check(rec, u);
  await atRest(u, 'clear filters');
}

async function toItems(u) {
  await clickFor(u,
    tabsRow(u).getByRole('button', { name: 'items', exact: true }),
    (r) => isList(r) && r.type === 'items', 'tab items');
  await atRest(u, 'tab items');
}

async function closeForm(u) {
  await clickQuiet(formLoc(u).first()
    .getByRole('button', { name: /^(Close|Cancel)$/ }).first());
  await atRest(u, 'close form', 800);
}

const rowId = async (row) =>
  ((await row.getAttribute('aria-label')) ?? '').split(' ').pop() ?? '';

const ACTIONS = {};

ACTIONS.tab = async (u) => {
  const cur = hashQuery(u.page).get('type');
  const row = tabsRow(u);
  const names = (await row.locator('button').allInnerTexts())
    .map((s) => s.trim());
  const others = names.filter((n) => n !== cur);
  if (!others.length) return `only ${cur}`;
  const favs = u.favs.filter((n) => others.includes(n));
  const name = (favs.length && u.rng() < 0.6)
    ? pick(u.rng, favs) : pick(u.rng, others);
  await clickFor(u, row.getByRole('button', { name, exact: true }),
    (r) => isList(r) && r.type === name, `tab ${name}`);
  await atRest(u, `tab ${name}`);
  return name;
};

const PAGE_WEIGHT = { '« First': 1, '‹ Prev': 2.5, 'Next ›': 5,
  'Last »': 1.5 };

ACTIONS.page = async (u) => {
  const pager = u.page.locator('.list-footer button:not([disabled])');
  const names = (await pager.allInnerTexts()).map((s) => s.trim());
  if (!names.length) return 'one page only';
  const label = weighted(u.rng,
    names.map((n) => [PAGE_WEIGHT[n] ?? 1, n]));
  await clickFor(u, pager.nth(names.indexOf(label)), isList,
    `page ${label}`);
  await atRest(u, `page ${label}`);
  return `${label} → page ${hashQuery(u.page).get('page') ?? 1}`;
};

// A column header that is not sortable sends nothing, which is not a
// hang: wait briefly for a request to start, and move on if none does.
ACTIONS.sort = async (u) => {
  const th = u.page.locator('table').first().locator('th');
  const cell = th.nth(Math.floor(u.rng() * await th.count()));
  const label = (await cell.innerText()).replace(/[▲▼]/g, '').trim();
  const w = waitStart(u, isList, 2500 / CFG.speed + 800);
  try {
    await cell.click({ timeout: CLICK_MS });
  } catch (e) {
    w.cancel();
    throw e;
  }
  const rec = await w;
  if (!rec) return `${label} (not sortable)`;
  await done(u, rec, `sort ${label}`);
  check(rec, u);
  await atRest(u, `sort ${label}`);
  return label;
};

ACTIONS.open = async (u) => {
  const type = hashQuery(u.page).get('type');
  const rows = mainRows(u, type);
  const n = await rows.count();
  if (!n) return 'no rows';
  const row = rows.nth(Math.floor(u.rng() * n));
  const id = await rowId(row);
  const note = `${type} ${id.slice(0, 8)}`;
  await clickFor(u, await textCell(u, row),
    (r) => r.p.startsWith('/api/item'), `open ${note}`);
  await atRest(u, `open ${note}`, 800);
  // The journal under an item: sometimes open an entry. The page draws
  // it from the list it already has, so nothing has to reach the
  // server: follow a request if one starts, and expect the row to turn
  // into "Collapse" either way.
  const kids = u.page.locator('tr.row-clickable[aria-label^="Expand "]');
  const kidsN = await kids.count();
  let extra = '';
  if (kidsN && u.rng() < 0.6) {
    const kid = kids.nth(Math.floor(u.rng() * Math.min(kidsN, 8)));
    const words = ((await kid.getAttribute('aria-label')) ?? '')
      .split(' ');                     // "Expand <type> record <uuid>"
    const kind = words[1];
    const cell = await textCell(u, kid);
    const w = waitStart(u, (r) => r.p.startsWith('/api/item')
      && r.type === kind, 2500 / CFG.speed + 800);
    try {
      await cell.click({ timeout: CLICK_MS });
    } catch (e) {
      w.cancel();
      throw e;
    }
    const rec = await w;
    if (rec) {
      await done(u, rec, `expand ${kind}`);
      check(rec, u);
    }
    await u.page.locator(
      `tr[aria-label="Collapse ${kind} record ${words[3]}"]`)
      .waitFor({ state: 'attached', timeout: CLICK_MS })
      .catch(() => { throw new Error(`expand ${kind}: did not open`); });
    await atRest(u, `expand ${kind}`, 800);
    await pause(u, 5000, 0.8);
    extra = ` + ${kind}`;
  }
  await pause(u, 9000, 0.9);                 // reading
  await closeForm(u);
  return note + extra;
};

async function pickTerm(u) {
  const cells = await u.page.locator('table').first()
    .locator('tbody td').allInnerTexts();
  const words = [...new Set(cells.join(' ').toLowerCase()
    .match(/[a-z]{4,12}/g) ?? [])];
  if (words.length && u.rng() < 0.7) return pick(u.rng, words);
  return pick(u.rng, ['plan', 'test', 'data', 'model', 'user', 'form',
    'list', 'fix', 'item', 'the']);
}

ACTIONS.search = async (u) => {
  const input = u.page.locator('input[placeholder="Search…"]').first();
  if (await input.inputValue()) await clearSearch(u);
  const term = await pickTerm(u);
  await input.click({ timeout: CLICK_MS });
  const w = waitFinish(u, (r) => isList(r) && r.p.includes('search='),
    CFG.timeoutMs + 20000);
  for (const ch of term) {
    await u.page.keyboard.press(ch);
    await pause(u, 130, 0.5);
    if (u.rng() < 0.07) await pause(u, 700, 0.4);   // a think
  }
  const rec = await w;
  if (!rec) {
    if (u.live.size) hang(u, `search ${term}`);
    throw new Error(`search ${term}: nothing was sent to the server`);
  }
  check(rec, u);
  await atRest(u, `search ${term}`);
  const rows = await mainRows(u, hashQuery(u.page).get('type')).count();
  await pause(u, 5000, 0.8);
  await clearSearch(u);
  return `"${term}" → ${rows} rows`;
};

ACTIONS.filter = async (u) => {
  const chips = u.page.locator('table').first()
    .locator('tbody button.chip[title^="Filter by "]');
  const n = await chips.count();
  if (!n) return 'no chips';
  const chip = chips.nth(Math.floor(u.rng() * n));
  const value = ((await chip.getAttribute('title')) ?? '')
    .replace(/^Filter by /, '');
  await clickFor(u, chip,
    (r) => isList(r) && r.p.includes('filters='), `filter ${value}`);
  await atRest(u, `filter ${value}`);
  const rows = await mainRows(u, hashQuery(u.page).get('type')).count();
  await pause(u, 5000, 0.8);
  await clearFilters(u);
  return `${value} → ${rows} rows`;
};

ACTIONS.home = async (u) => {
  await clickFor(u,
    u.page.locator('.view-switcher button:has-text("Home")'),
    isList, 'home');
  await atRest(u, 'home');
  return hashQuery(u.page).get('type') ?? '';
};

ACTIONS.history = async (u) => {
  const dir = u.rng() < 0.75 ? 'goBack' : 'goForward';
  const before = u.page.url();
  const w = waitStart(u, (r) => r.api, 3000 / CFG.speed + 1000);
  await u.page[dir]({ timeout: CLICK_MS }).catch(() => {});
  const rec = await w;
  if (!rec) {
    const url = u.page.url();
    // Back past the app's first page lands on the empty page the tab
    // began on; the next step brings the wanderer back.
    if (!url.startsWith(`${BASE.origin}/`)) return `${dir}: left the app`;
    return url === before ? `${dir}: nothing there`
      : `${dir} (no request)`;
  }
  await done(u, rec, dir);
  check(rec, u);
  await atRest(u, dir);
  return `${dir} → ${hashQuery(u.page).get('type')}`;
};

ACTIONS.reload = async (u) => {
  await u.page.reload({ waitUntil: 'load', timeout: CFG.timeoutMs });
  await atRest(u, 'reload');
  return hashQuery(u.page).get('type') ?? '';
};

ACTIONS.update = async (u) => {
  if (hashQuery(u.page).get('type') !== 'items') await toItems(u);
  await clearSearch(u);
  await clearFilters(u);
  const rows = mainRows(u, 'items');
  const n = await rows.count();
  if (!n) return 'no items to update';
  const row = rows.nth(Math.floor(u.rng() * n));
  const id = await rowId(row);
  const note = `items ${id.slice(0, 8)}`;
  await clickFor(u, await textCell(u, row),
    (r) => r.p.startsWith('/api/item'), `open ${note}`);
  await atRest(u, `open ${note}`, 800);
  const form = formLoc(u).first();
  const edit = form.getByRole('button', { name: 'Edit', exact: true });
  if (!(await edit.count())) {
    u.noEdit++;
    await closeForm(u);
    if (u.noEdit >= 3) u.canUpdate = false;
    return `${note}: no Edit button `
      + (u.canUpdate ? '(read-only row)'
        : '(this account cannot update; no more updates tried)');
  }
  await clickFor(u, edit,
    (r) => r.p.startsWith('/api/item')
      && r.p.includes('form=update-form'),
    `edit ${note}`);
  const editForm = u.page.locator('form')
    .filter({ has: u.page.locator('h3', { hasText: /^Edit items$/ }) })
    .first();
  await editForm.waitFor({ timeout: CLICK_MS });
  const field = editForm.locator('.field-row')
    .filter({ has: u.page.locator('.field-label label',
      { hasText: /^Title$/ }) })
    .locator('.field-value input').first();
  const before = await field.inputValue();
  const after = `[${1000 + Math.floor(u.rng() * 9000)}] `
    + before.replace(/^\[\d+\] /, '');
  if (after.length > TITLE_MAX) {
    await closeForm(u);
    return `${note}: title would exceed ${TITLE_MAX} characters`;
  }
  await field.fill(after);
  const w = waitFinish(u,
    (r) => r.m === 'POST' && r.p.startsWith('/api/update'));
  try {
    await editForm.getByRole('button', { name: 'Update',
      exact: true }).first().click({ timeout: CLICK_MS });
  } catch (e) {
    w.cancel();
    throw e;
  }
  const rec = await w;
  if (!rec) {
    if (u.live.size) hang(u, `update ${note}`);
    throw new Error(`update ${note}: nothing was sent to the server`);
  }
  const json = await rec.res?.json().catch(() => null);
  if (rec.status !== 200 || json?.status !== 'success') {
    anomaly('update-failed', u, `${note}: HTTP ${rec.status} `
      + `${json?.error ?? ''} — title "${after}"`);
    shot(u, 'update');
    await closeForm(u);
    return `${note}: UPDATE FAILED`;
  }
  titleUpdates.push({ at: new Date().toISOString(), user: `u${u.n}`,
    id, before, after, ms: rec.t1 - rec.t0 });
  u.updates++;
  await atRest(u, `update ${note}`, 800);
  return `${note}: "${before.slice(0, 34)}" → "${after.slice(0, 34)}"`;
};

// Actions that already include their own reading time.
const READING = new Set(['open', 'search', 'filter', 'update']);

async function chooseAction(u) {
  const rng = u.rng;
  if (CFG.updates && u.canUpdate && rng() < CFG.updatePct / 100) {
    return 'update';
  }
  const page = u.page;
  const type = hashQuery(page).get('type');
  const opts = [];
  if (await mainRows(u, type).count()) opts.push([20, 'open']);
  if (await tabsRow(u).locator('button').count() > 1) {
    opts.push([18, 'tab']);
  }
  if (await page.locator('.list-footer button:not([disabled])')
    .count()) {
    opts.push([12, 'page']);
  }
  if (await page.locator('table th').count()) opts.push([9, 'sort']);
  if (await page.locator('input[placeholder="Search…"]').count()) {
    opts.push([7, 'search']);
  }
  if (await page.locator('table tbody button.chip[title^="Filter by "]')
    .count()) {
    opts.push([6, 'filter']);
  }
  opts.push([4, 'home'], [4, 'history'], [3, 'reload']);
  return weighted(rng, opts);
}

// Put the page back the way a person would before the next thing: come
// back if Back left the app, sign in again, leave a failed list, close
// a form left open.
async function tidy(u) {
  const page = u.page;
  if (!page.url().startsWith(`${BASE.origin}/`)) {
    await page.goto(`${BASE.origin}/`,
      { waitUntil: 'load', timeout: CFG.timeoutMs });
    await atRest(u, 'back to the app');
  }
  if (await page.locator('input[name="password"]').count()) {
    anomaly('session-lost', u, 'the login form came back; '
      + 'signing in again');
    await login(u);
  }
  const dismiss = page.locator('[role="alert"] button:has-text("Dismiss")');
  if (await dismiss.count()) await clickQuiet(dismiss.first());
  if (await page.getByText(/^Request failed \(\d+\)$/).count()) {
    await clickQuiet(page.locator('.view-switcher button:has-text("Home")'));
    await settle(u);
  }
  if (await formLoc(u).count()) await closeForm(u);
}

// After a failure: on a hang, first say what it looked like (and sit
// still if asked), then reload the page and carry on.
async function recover(u, onHang = false) {
  if (onHang) {
    const i = hangCtx.findLastIndex((c) => c.page === u.page);
    if (i >= 0) {
      const [ctx] = hangCtx.splice(i, 1);
      await describeHang(ctx);
      if (CFG.holdOnHang > 0) await holdStill(u);
    }
  }
  u.fails++;
  if (u.fails >= 3 || state.stop) return;
  await u.page.reload({ waitUntil: 'load', timeout: CFG.timeoutMs })
    .catch(() => {});
  await settle(u);
}

// Leave the page — and its open requests — alone for a while, so the
// server can be looked at mid-hang.
async function holdStill(u) {
  const until = Date.now() + CFG.holdOnHang * 1000;
  out(`u${u.n} ${mmss().padStart(5)}  sitting still for `
    + `${CFG.holdOnHang}s: its hung requests stay open`);
  while (Date.now() < until && !state.stop) await sleep(500);
}

async function perform(u, act) {
  const t = Date.now();
  const done0 = u.done;
  u.counts[act] = (u.counts[act] ?? 0) + 1;
  let note;
  try {
    note = await ACTIONS[act](u);
    u.fails = 0;
  } catch (e) {
    recordError(u, act, e);
    note = `! ${brief(e)}`;
    if (e instanceof Fatal) {
      u.reported = true;      // recorded here, not again in runUser
      throw e;
    }
    if (!(e instanceof Hang)) shot(u, 'error');
    await recover(u, e instanceof Hang);
  }
  out(`u${u.n} ${mmss().padStart(5)}  ${act.padEnd(8)} ${note}   `
    + `[${u.done - done0} req, ${fmt(Date.now() - t)} ms]`);
}

async function runUser(u) {
  if (!CFG.together) {
    await pause(u, 6000, 0.9);     // people do not all start at once
  }
  try {
    // A crowd arriving together is the busiest moment of the run, so
    // a first page that hangs is recovered from, not given up on.
    await u.page.goto(`${BASE.origin}/`,
      { waitUntil: 'load', timeout: CFG.timeoutMs });
    if (await u.page.locator('input[name="password"]').count()) {
      await login(u);        // the handed-over session did not take
    } else {
      await atRest(u, 'first page load');
    }
    const names = (await tabsRow(u).locator('button').allInnerTexts())
      .map((s) => s.trim());
    if (names.length) {
      u.favs = pickSome(u.rng, names, u.rng() < 0.5 ? 1 : 2);
    }
  } catch (e) {
    recordError(u, 'start', e);
    if (e instanceof Fatal) return;
    await recover(u, e instanceof Hang);
  }
  try {
    while (!state.stop && Date.now() < state.deadline && u.fails < 3) {
      await step(u);
    }
  } catch (e) {
    if (!u.reported) recordError(u, 'stopped', e);
  }
}

// One look around: put the page right, pick something to do, do it.
async function step(u) {
  let act;
  try {
    await tidy(u);
    act = await chooseAction(u);
  } catch (e) {
    recordError(u, 'tidy', e);
    if (e instanceof Fatal) throw e;
    if (!(e instanceof Hang)) shot(u, 'error');
    await recover(u, e instanceof Hang);
    return;
  }
  await perform(u, act);
  if (READING.has(act)) await pause(u, 1500, 0.7);
  else await pause(u, 5500, 0.8);
}

// ---- report -------------------------------------------------------------

function percentile(sorted, p) {
  return sorted.length
    ? sorted[Math.min(sorted.length - 1,
      Math.floor(p / 100 * sorted.length))] : 0;
}

function statLine(label, list) {
  const d = list.map((r) => r.t1 - r.t0).sort((x, y) => x - y);
  return `  ${label.padEnd(34)}${fmt(d.length).padStart(7)}`
    + `${fmt(percentile(d, 50)).padStart(8)}`
    + `${fmt(percentile(d, 95)).padStart(8)}`
    + `${fmt(d[d.length - 1] ?? 0).padStart(8)}`;
}

function endpointKey(r) {
  if (!r.api) return '(page, script, style)';
  const p = r.p.split('?')[0];
  return /\/(list|item)$/.test(p) && r.type
    ? `${r.m} ${p}?type=${r.type}` : `${r.m} ${p}`;
}

function buildReport() {
  const L = [];
  const p = (s = '') => L.push(s);
  const counted = anomalies.filter((a) => COUNTED.has(a.kind));
  const lived = reqs.filter((r) => !r.hung);
  const api = lived.filter((r) => r.api);
  p(`Data UI crowd — ${new Date(T0).toISOString()}`);
  p();
  p('Run');
  p(`  app ${BASE.origin}   account ${CFG.user}   wanderers `
    + `${CFG.users}   ${CFG.headed ? 'headed' : 'headless'}`);
  p(`  asked for ${CFG.minutes ? `${CFG.minutes} min` : 'until Ctrl-C'}, `
    + `ran ${(secs() / 60).toFixed(1)} min   speed ${CFG.speed}   seed `
    + `${CFG.seed}   updates ${CFG.updates ? 'on' : 'off'}`);
  p();
  p('Traffic');
  p(`  requests from the pages: ${fmt(reqs.length)} `
    + `(API ${fmt(api.length)})   health probes ${health.ok} ok, `
    + `${health.fail} failed`);
  const byStatus = new Map();
  for (const r of reqs) {
    const k = r.hung ? 'never returned'
      : r.status ? String(r.status) : (r.err ?? 'failed');
    byStatus.set(k, (byStatus.get(k) ?? 0) + 1);
  }
  p(`  outcome: ${[...byStatus].sort().map(([k, n]) => `${k} ×${fmt(n)}`)
    .join('   ')}`);
  const ghosts = reqs.filter((r) => r.ghost).length;
  if (ghosts) {
    p(`  of those, ${ghosts} Chrome never reported done — the page's `
      + 'own timing gave their time and status');
  }
  if (dropped) p(`  cut off by a reload: ${dropped}`);
  p(`  overlapped: ${fmt(overlapStarts)} `
    + `(${reqs.length ? Math.round(100 * overlapStarts / reqs.length)
      : 0}%) began while another request was in flight — with the `
    + 'gate on, one of each pair waited');
  p(`  most in flight at once: ${maxInFlight}   began behind 3 or `
    + `more: ${fmt(behind3)}`);
  const expired = reqs.filter((r) => r.auth).length;
  if (expired) {
    p(`  401s: ${expired} — an expired access token; the page refreshes `
      + 'and retries');
  }
  p();
  p('Latency, ms');
  p(`  ${''.padEnd(34)}${'n'.padStart(7)}${'p50'.padStart(8)}`
    + `${'p95'.padStart(8)}${'max'.padStart(8)}`);
  p(statLine('every request', lived));
  const groups = new Map();
  for (const r of lived) {
    const k = endpointKey(r);
    if (!groups.has(k)) groups.set(k, []);
    groups.get(k).push(r);
  }
  for (const [k, list] of [...groups]
    .sort((x, y) => y[1].length - x[1].length)) {
    p(statLine(k, list));
  }
  const slow = [...api]
    .sort((x, y) => (y.t1 - y.t0) - (x.t1 - x.t0)).slice(0, 5);
  if (slow.length) {
    p('  slowest:');
    for (const r of slow) {
      p(`    ${fmt(r.t1 - r.t0)} ms  ${r.m} ${short(r.p)}`);
    }
  }
  p();
  p('Wanderers');
  for (const u of users) {
    const acts = Object.entries(u.counts)
      .map(([k, n]) => `${k} ${n}`).join(', ');
    p(`  u${u.n}  ${fmt(u.done)} requests, ${acts || 'nothing'}`
      + (u.fails >= 3 ? '   — gave up after 3 failures in a row' : ''));
  }
  if (CFG.updates) {
    p();
    p(`Updates: ${titleUpdates.length} made `
      + `(${users.map((u) => `u${u.n} ${u.updates}`).join(', ')})`);
    for (const u of users.filter((x) => !x.canUpdate)) {
      p(`  u${u.n}: no Edit button on any item opened — ${CFG.user} `
        + 'cannot update items (it needs a role with update right)');
    }
  }
  p();
  p(`Anomalies: ${anomalies.length ? '' : 'none'}`);
  for (const kind of [...new Set(anomalies.map((a) => a.kind))]) {
    const all = anomalies.filter((a) => a.kind === kind);
    const seen = new Set();
    const uniq = all.filter((a) => {
      const k = `${a.user} ${a.detail}`;
      if (seen.has(k)) return false;
      seen.add(k);
      return true;
    });
    p(`  ${kind} ×${fmt(all.length)}`
      + `${COUNTED.has(kind) ? '' : ' (not counted in the verdict)'}`);
    for (const a of uniq.slice(0, 4)) {
      p(`    ${a.at.toFixed(1)}s ${a.user}  ${a.detail}`);
      if (a.sockets) {
        p(`      the server's connections then: ${a.sockets}`);
      }
    }
    if (uniq.length > 4) p(`    ... ${uniq.length - 4} more`);
  }
  p();
  p(counted.length
    ? `Verdict: PROBLEMS — ${counted.length}; see Anomalies`
    : 'Verdict: CLEAN');
  p(`Files: ${RUN}`);
  return L.join('\n');
}

// ---- main ---------------------------------------------------------------

async function probe() {
  try {
    const r = await fetch(`${BASE.origin}/health`,
      { signal: AbortSignal.timeout(5000) });
    if (r.ok) health.ok++;
    else health.fail++;
  } catch {
    health.fail++;
  }
}

const progress = setInterval(() => {
  if (CFG.quiet || state.closing) return;
  const n = anomalies.filter((a) => COUNTED.has(a.kind)).length;
  say(`  [${mmss()}] requests ${fmt(reqs.length)}   in flight now `
    + `${inFlight}, most ${maxInFlight}   anomalies ${n}   health `
    + `${health.ok} ok / ${health.fail} failed`);
}, 30000);
progress.unref();
const probeTimer = setInterval(probe, 5000);
probeTimer.unref();

function stop(signal) {
  if (state.stop) {
    // Impatience: shut the browser on the way out.
    state.browser?.close().catch(() => {})
      .finally(() => process.exit(130));
    return;
  }
  state.stop = true;
  say(`\n— ${signal}: each wanderer finishes what it is doing `
    + '(again to abandon) —');
}
process.on('SIGINT', () => stop('Ctrl-C'));
process.on('SIGTERM', () => stop('SIGTERM'));

async function main() {
  fs.mkdirSync(RUN, { recursive: true });
  const browser = await chromium.launch({
    channel: 'chrome',
    headless: !CFG.headed,
    // Ctrl-C is ours (see stop): a run ends with its report.
    handleSIGINT: false, handleSIGTERM: false, handleSIGHUP: false,
    // A window hidden behind another must keep rendering and timing.
    args: CFG.headed ? ['--disable-backgrounding-occluded-windows',
      '--disable-renderer-backgrounding',
      '--disable-features=CalculateNativeWinOcclusion'] : [],
  });
  state.browser = browser;
  const giveUp = (msg) => {
    browser.close().catch(() => {});
    fs.rmSync(RUN, { recursive: true, force: true });
    fail(msg);
  };
  const master = makeRng(CFG.seed);
  for (let i = 0; i < CFG.users; i++) {
    const ctx = await browser.newContext(
      { viewport: { width: 1280, height: 900 } });
    const page = await ctx.newPage();
    page.setDefaultTimeout(CLICK_MS);
    page.setDefaultNavigationTimeout(CFG.timeoutMs);
    const u = {
      n: i + 1, ctx, page, rng: makeRng(CFG.seed + 7919 * (i + 1)),
      pace: 0.7 + master() * 0.8, favs: [], ws: [], wf: [],
      live: new Map(), lastStart: 0, done: 0, counts: {}, fails: 0,
      canUpdate: true, noEdit: 0, updates: 0,
    };
    attach(u);
    users.push(u);
  }
  // One sign-in, through the form; the others inherit the session.
  say(`crowd: ${CFG.users} wanderer${CFG.users > 1 ? 's' : ''} as `
    + `${CFG.user} on ${BASE.origin} (seed ${CFG.seed})`);
  try {
    await login(users[0]);
  } catch (e) {
    giveUp(e instanceof Fatal ? brief(e) : `first login: ${brief(e)}`);
  }
  const session = await users[0].page.evaluate((k) =>
    localStorage.getItem(k), SESSION_KEY);
  if (!session) giveUp('the app stored no session after login');
  // The others start with that stored session, the way a returning
  // visitor does — seeded before the app's first load, once. (Loading
  // the page first and reloading with the session makes Chrome stop
  // reporting some requests' ends.)
  for (const u of users.slice(1)) {
    await u.ctx.addInitScript(([k, v, once]) => {
      try {
        if (sessionStorage.getItem(once)) return;
        sessionStorage.setItem(once, '1');
        if (!localStorage.getItem(k)) localStorage.setItem(k, v);
      } catch { /* a page with no storage is not the app */ }
    }, [SESSION_KEY, session, 'crowd-seeded']);
  }
  if (CFG.minutes > 0) {
    state.deadline = Date.now() + CFG.minutes * 60000;
  }
  await probe();
  await Promise.all(users.map(runUser));

  state.closing = true;
  clearInterval(progress);
  clearInterval(probeTimer);
  for (const u of users) await u.ctx.close().catch(() => {});
  await browser.close().catch(() => {});

  const report = buildReport();
  fs.writeFileSync(path.join(RUN, 'report.txt'), `${report}\n`);
  // res (the Playwright Response) is for the actions, not the log.
  const lines = (list) => `${list.map(({ res, req, ...r }) =>
    JSON.stringify(r)).join('\n')}\n`;
  fs.writeFileSync(path.join(RUN, 'requests.jsonl'), lines(reqs));
  fs.writeFileSync(path.join(RUN, 'anomalies.jsonl'), lines(anomalies));
  fs.writeFileSync(path.join(RUN, 'updates.jsonl'),
    lines(titleUpdates));
  say(`\n${report}`);
  process.exit(anomalies.filter((a) => COUNTED.has(a.kind)).length
    ? 1 : 0);
}

main().catch((e) => fail(brief(e)));
