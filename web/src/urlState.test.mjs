// Dependency-free urlState.ts tests: transpile the TS source
// with the installed typescript package (this host's Node 22
// lacks native TS stripping — required until Node >= 23.6),
// import the ES module through a data URL, and run assertions
// with node:assert/strict. Leaves no generated file. Run:
//   npm run test:url-state
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import url from 'node:url'
import ts from 'typescript'

const here = path.dirname(url.fileURLToPath(import.meta.url))
const src = fs.readFileSync(
  path.join(here, 'urlState.ts'), 'utf8')
const out = ts.transpileModule(src, {
  compilerOptions: {
    module: ts.ModuleKind.ESNext,
    target: ts.ScriptTarget.ES2020
  }
}).outputText
const mod = await import(
  'data:text/javascript;base64,'
  + Buffer.from(out).toString('base64'))
const { canonicalize, parseHash, buildUrlState, writeUrl,
        urlStatesEqual, parseScope, scopeToParam } = mod

let passed = 0
function ok(name, fn) {
  fn()
  passed++
  console.log(`ok - ${name}`)
}

// ---- test doubles: mutable window.location.hash + bare
// // globalThis.history (writeUrl calls bare history.pushState)
let liveHash = ''
const pushes = []
const replaces = []
function resetDoubles(hash = '') {
  liveHash = hash
  pushes.length = 0
  replaces.length = 0
}
Object.defineProperty(globalThis, 'window', {
  configurable: true,
  value: { location: {} }
})
Object.defineProperty(globalThis.window, 'location', {
  configurable: true,
  get() { return { get hash() { return liveHash },
                   set hash(v) { liveHash = v } } }
})
globalThis.history = {
  pushState(state, title, url) {
    pushes.push({ state, title, url })
    liveHash = url.slice(url.indexOf('#'))
  },
  replaceState(state, title, url) {
    replaces.push({ state, title, url })
    liveHash = url.slice(url.indexOf('#'))
  }
}

const UUID = 'c0ffee00-1234-5678-9abc-def012345678'

// ---- canonicalize ----
ok('canonicalize: bare view keeps id, drops mode', () => {
  const c = canonicalize(
    { type: 'comments', id: UUID, mode: 'view' })
  assert.deepEqual(c,
    { type: 'comments', id: UUID })
})
ok('canonicalize: edit kept with type + id', () => {
  const c = canonicalize(
    { type: 'comments', id: UUID, mode: 'edit' })
  assert.equal(c.mode, 'edit')
})
ok('canonicalize: id without type dropped', () => {
  const c = canonicalize({ id: UUID })
  assert.equal('id' in c, false)
})
ok('canonicalize: mode without id dropped', () => {
  const c = canonicalize({ type: 'comments', mode: 'edit' })
  assert.equal('mode' in c, false)
})
ok('canonicalize: empty id dropped', () => {
  const c = canonicalize({ type: 'comments', id: '' })
  assert.equal('id' in c, false)
})
ok('canonicalize: uuid lowercased', () => {
  const c = canonicalize(
    { type: 'comments', id: UUID.toUpperCase() })
  assert.equal(c.id, UUID)
})
ok('canonicalize: list state preserved beside record', () => {
  const c = canonicalize({ type: 'comments', id: UUID,
    q: 'x', page: 2 })
  assert.equal(c.q, 'x')
  assert.equal(c.page, 2)
})
ok('canonicalize: unknown mode values are not edit', () => {
  const c = canonicalize({ type: 'c', id: UUID })
  assert.equal(c.mode, undefined)
})

// ---- parseHash round trips ----
ok('parseHash: view permalink', () => {
  resetDoubles(`#/app?type=comments&id=${UUID}`)
  const p = parseHash()
  assert.equal(p.type, 'comments')
  assert.equal(p.id, UUID)
  assert.equal(p.mode, undefined)
})
ok('parseHash: edit permalink', () => {
  resetDoubles(`#/app?type=comments&id=${UUID}&mode=edit`)
  const p = parseHash()
  assert.equal(p.mode, 'edit')
})
ok('parseHash: unknown mode tolerated (no mode)', () => {
  resetDoubles(`#/app?type=comments&id=${UUID}&mode=bogus`)
  const p = parseHash()
  assert.equal(p.mode, undefined)
})
ok('parseHash: id without type parses (canonicalize drops)',
  () => {
    resetDoubles(`#/app?id=${UUID}`)
    const p = parseHash()
    assert.equal(p.id, UUID)
    assert.equal('type' in p, false)
  })
ok('parseHash: mode without id parses (canonicalize drops)',
  () => {
    resetDoubles('#/app?type=comments&mode=edit')
    const p = parseHash()
    assert.equal(p.type, 'comments')
    assert.equal(p.mode, 'edit')
  })
ok('parseHash: empty id ignored', () => {
  resetDoubles('#/app?type=comments&id=')
  const p = parseHash()
  assert.equal('id' in p, false)
})
ok('parseHash: uppercase uuid legal on parse', () => {
  resetDoubles(`#/app?type=c&id=${UUID.toUpperCase()}`)
  assert.equal(parseHash().id, UUID.toUpperCase())
})
ok('parseHash: encoded uuid round trips', () => {
  const s = { type: 'c', id: UUID, mode: 'edit', page: 2 }
  resetDoubles()
  writeUrl(s)
  const p = parseHash()
  assert.equal(p.id, UUID)
  assert.equal(p.mode, 'edit')
  assert.equal(p.page, 2)
  assert.deepEqual(canonicalize(p), canonicalize(s))
})
ok('parseHash: non-app hash returns null', () => {
  resetDoubles('#/other?x=1')
  assert.equal(parseHash(), null)
  resetDoubles('')
  assert.equal(parseHash(), null)
})
ok('parseHash: list params preserved with id', () => {
  resetDoubles(`#/app?type=c&id=${UUID}&q=hi&sort=-name&page=3`)
  const p = parseHash()
  assert.equal(p.q, 'hi')
  assert.equal(p.sort, '-name')
  assert.equal(p.page, 3)
})

// ---- buildUrlState ----
ok('buildUrlState: record target from desired state', () => {
  const b = buildUrlState({ type: 'c', recordId: UUID,
    recordMode: 'view', debouncedSearch: '',
    debouncedNotTerm: '', listFilters: {},
    boolFilters: {}, hideExclusive: true,
    sortField: null, sortDir: 'asc', currentPage: 1,
    scope: { kind: 'none' } })
  assert.equal(b.id, UUID)
  assert.equal(b.mode, 'view')
})
ok('buildUrlState: no record target omits id and mode', () => {
  const b = buildUrlState({ type: 'c', recordId: null,
    recordMode: 'view', debouncedSearch: '',
    debouncedNotTerm: '', listFilters: {},
    boolFilters: {}, hideExclusive: true,
    sortField: null, sortDir: 'asc', currentPage: 1,
    scope: { kind: 'none' } })
  assert.equal(b.id, undefined)
  assert.equal(b.mode, undefined)
})

// ---- post scope (plan 04): parseScope / scopeToParam ----
ok('parseScope: valid pair', () => {
  const s = parseScope('post:' + UUID)
  assert.equal(s.kind, 'valid')
  assert.equal(s.via, 'post')
  assert.equal(s.id, UUID)
})
ok('parseScope: non-UUID id stays structurally valid', () => {
  // Server validation territory — never a frontend reject.
  assert.equal(parseScope('post:garbage').kind, 'valid')
})
ok('parseScope: bare value (no colon) invalid, raw kept', () => {
  const s = parseScope('garbage')
  assert.equal(s.kind, 'invalid')
  assert.equal(s.raw, 'garbage')
})
ok('parseScope: empty via half invalid', () => {
  assert.equal(parseScope(':' + UUID).kind, 'invalid')
})
ok('parseScope: empty id half invalid, raw kept', () => {
  const s = parseScope('post:')
  assert.equal(s.kind, 'invalid')
  assert.equal(s.raw, 'post:')
})
ok('parseScope: extra colon invalid', () => {
  assert.equal(parseScope('post:' + UUID + ':x').kind,
    'invalid')
})
ok('parseScope: empty string invalid with raw retained', () => {
  const s = parseScope('')
  assert.equal(s.kind, 'invalid')
  assert.equal(s.raw, '')
})
ok('scopeToParam: none / valid / invalid round trip', () => {
  assert.equal(scopeToParam({ kind: 'none' }), undefined)
  assert.equal(
    scopeToParam(parseScope('post:' + UUID)),
    'post:' + UUID)
  assert.equal(
    scopeToParam({ kind: 'invalid', raw: 'x' }), 'x')
})

// ---- parseHash fk ----
ok('parseHash: fk beside id', () => {
  resetDoubles(`#/app?type=comments&fk=post:${UUID}&id=${UUID}`)
  const p = parseHash()
  assert.equal(p.fk, 'post:' + UUID)
  assert.equal(p.id, UUID)
})
ok('parseHash: duplicate fk params → empty invalid marker',
  () => {
    resetDoubles(
      `#/app?type=c&fk=post:${UUID}&fk=post:${UUID}`)
    assert.equal(parseHash().fk, '')
  })
ok('parseHash: empty fk value is present', () => {
  resetDoubles('#/app?type=c&fk=')
  assert.equal(parseHash().fk, '')
})
ok('parseHash: absent fk stays absent', () => {
  resetDoubles('#/app?type=c')
  assert.equal('fk' in parseHash(), false)
})
ok('parseHash+canonicalize: fk without type parses then drops',
  () => {
    resetDoubles(`#/app?fk=post:${UUID}`)
    const p = parseHash()
    assert.equal(p.fk, 'post:' + UUID)
    assert.equal('type' in p, false)
    assert.equal('fk' in canonicalize(p), false)
  })

// ---- canonicalize fk ----
ok('canonicalize: valid fk lowercases only the id half', () => {
  const c = canonicalize(
    { type: 'comments', fk: 'POST:' + UUID.toUpperCase() })
  assert.equal(c.fk, 'POST:' + UUID)
})
ok('canonicalize: fk without type dropped', () => {
  const c = canonicalize({ fk: 'post:' + UUID })
  assert.equal('fk' in c, false)
})
ok('canonicalize: invalid fk preserved with raw (incl. empty)',
  () => {
    assert.equal(
      canonicalize({ type: 'c', fk: 'garbage' }).fk, 'garbage')
    assert.equal(canonicalize({ type: 'c', fk: '' }).fk, '')
  })

// ---- buildUrlState scope ----
ok('buildUrlState: valid scope → fk param', () => {
  const b = buildUrlState({ type: 'comments', recordId: null,
    recordMode: 'view', debouncedSearch: '',
    debouncedNotTerm: '', listFilters: {},
    boolFilters: {}, hideExclusive: true,
    sortField: null, sortDir: 'asc', currentPage: 1,
    scope: { kind: 'valid', via: 'post', id: UUID } })
  assert.equal(b.fk, 'post:' + UUID)
})
ok('buildUrlState: invalid scope → raw', () => {
  const b = buildUrlState({ type: 'comments', recordId: null,
    recordMode: 'view', debouncedSearch: '',
    debouncedNotTerm: '', listFilters: {},
    boolFilters: {}, hideExclusive: true,
    sortField: null, sortDir: 'asc', currentPage: 1,
    scope: { kind: 'invalid', raw: 'post:' } })
  assert.equal(b.fk, 'post:')
})

// ---- urlStatesEqual ----
ok('urlStatesEqual: same list, different id not equal', () => {
  const a = canonicalize({ type: 'c', id: UUID })
  const b = canonicalize({ type: 'c', id: UUID.slice(0, -1) + '9' })
  assert.equal(urlStatesEqual(a, b), false)
})
ok('urlStatesEqual: view vs edit not equal', () => {
  const a = canonicalize({ type: 'c', id: UUID })
  const b = canonicalize({ type: 'c', id: UUID, mode: 'edit' })
  assert.equal(urlStatesEqual(a, b), false)
})
ok('urlStatesEqual: case variants equal after canonicalize',
  () => {
    const a = canonicalize({ type: 'c', id: UUID.toUpperCase() })
    const b = canonicalize({ type: 'c', id: UUID })
    assert.equal(urlStatesEqual(a, b), true)
  })
ok('urlStatesEqual: scoped vs unscoped unequal (all else equal)',
  () => {
    const a = canonicalize({ type: 'c', q: 'x', page: 2 })
    const b = canonicalize(
      { type: 'c', q: 'x', page: 2, fk: 'post:' + UUID })
    assert.equal(urlStatesEqual(a, b), false)
  })
ok('urlStatesEqual: same scope equal across id case', () => {
  const a = canonicalize({ type: 'c', fk: 'post:' + UUID })
  const b = canonicalize(
    { type: 'c', fk: 'post:' + UUID.toUpperCase() })
  assert.equal(urlStatesEqual(a, b), true)
})

// ---- writeUrl fk ----
ok('writeUrl: scoped entry serialized (fk after id)', () => {
  resetDoubles('#/app?type=c')
  writeUrl({ type: 'comments', id: UUID, fk: 'post:' + UUID })
  assert.equal(pushes[0].url,
    `#/app?type=comments&id=${UUID}&fk=post:${UUID}`)
})
ok('writeUrl: case-variant fk link equal → no duplicate push',
  () => {
    resetDoubles(
      `#/app?type=comments&fk=post:${UUID.toUpperCase()}`)
    writeUrl({ type: 'comments', fk: 'post:' + UUID })
    assert.equal(pushes.length, 0)
    assert.equal(replaces.length, 0)
  })
ok('writeUrl: malformed fk survives (no unscoped rewrite)',
  () => {
    resetDoubles('#/app?type=comments')
    writeUrl({ type: 'comments', fk: 'garbage' })
    assert.equal(pushes[0].url,
      '#/app?type=comments&fk=garbage')
  })
ok('writeUrl: empty invalid marker serialized (fk=)', () => {
  resetDoubles('#/app?type=comments')
  writeUrl({ type: 'comments', fk: '' })
  assert.equal(pushes[0].url, '#/app?type=comments&fk=')
})
ok('writeUrl: no writer loop on parsed malformed state', () => {
  resetDoubles('#/app?type=comments&fk=garbage')
  const p = parseHash()
  writeUrl({ type: 'comments', fk: p.fk })
  assert.equal(pushes.length, 0)
  assert.equal(replaces.length, 0)
})
ok('writeUrl: duplicate-param URL equals the empty marker',
  () => {
    resetDoubles('#/app?type=c&fk=a:1&fk=b:2')
    writeUrl({ type: 'c', fk: '' })
    assert.equal(pushes.length, 0)
  })
ok('writeUrl: absent scope omitted', () => {
  resetDoubles('#/app?type=c&page=3')
  writeUrl({ type: 'c', page: 2 })
  assert.equal(pushes.length, 1)
  assert.ok(!pushes[0].url.includes('fk='))
})
ok('fk: full round trip write → parse → canonical', () => {
  resetDoubles()
  const s = { type: 'comments', id: UUID, fk: 'post:' + UUID }
  writeUrl(s)
  const p = parseHash()
  assert.equal(p.fk, 'post:' + UUID)
  assert.deepEqual(canonicalize(p), canonicalize(s))
})
ok('fk: malformed raw survives parse → canonicalize → ' +
   'scope → build → write', () => {
  resetDoubles('#/app?type=comments&fk=post:')
  const p = parseHash()
  const c = canonicalize(p)
  const sc = parseScope(c.fk ?? '')
  const fk2 = scopeToParam(sc)
  resetDoubles('#/app?type=comments')
  writeUrl({ type: 'comments', fk: fk2 })
  assert.equal(pushes[0].url,
    '#/app?type=comments&fk=post:')
})
ok('fk: empty marker survives the same chain', () => {
  resetDoubles('#/app?type=comments&fk=')
  const p = parseHash()
  const c = canonicalize(p)
  const sc = parseScope(c.fk ?? '')
  const fk2 = scopeToParam(sc)
  resetDoubles('#/app?type=comments')
  writeUrl({ type: 'comments', fk: fk2 })
  assert.equal(pushes[0].url, '#/app?type=comments&fk=')
})

// ---- writeUrl ----
ok('writeUrl: ordinary path pushes once, state arg null', () => {
  resetDoubles('#/app?type=c')
  writeUrl({ type: 'c', id: UUID })
  assert.equal(pushes.length, 1)
  assert.equal(replaces.length, 0)
  assert.equal(pushes[0].state, null)
  assert.equal(pushes[0].url,
    `#/app?type=c&id=${UUID}`)
})
ok('writeUrl: id serialized after type, mode after id', () => {
  resetDoubles('#/app?type=c')
  writeUrl({ type: 'c', id: UUID, mode: 'edit' })
  assert.equal(pushes[0].url,
    `#/app?type=c&id=${UUID}&mode=edit`)
})
ok('writeUrl: no explicit mode=view in canonical URL', () => {
  resetDoubles('#/app?type=c')
  writeUrl({ type: 'c', id: UUID, mode: 'view' })
  assert.ok(!pushes[0].url.includes('mode='))
})
ok('writeUrl: equality gate — canonically equal live hash is a no-op',
  () => {
    resetDoubles(`#/app?type=c&id=${UUID.toUpperCase()}`)
    writeUrl({ type: 'c', id: UUID })
    assert.equal(pushes.length, 0)
    assert.equal(replaces.length, 0)
  })
ok('writeUrl: duplicate write (same state) is a no-op', () => {
  resetDoubles('')
  writeUrl({ type: 'c', id: UUID })
  writeUrl({ type: 'c', id: UUID })
  assert.equal(pushes.length, 1)
})
ok('writeUrl: replace option fires one replaceState, no push',
  () => {
    resetDoubles('#/app?type=c&page=2')
    writeUrl({ type: 'c', page: 1 }, { replace: true })
    assert.equal(replaces.length, 1)
    assert.equal(pushes.length, 0)
    assert.equal(replaces[0].state, null)
    assert.equal(replaces[0].url, '#/app?type=c')
  })
ok('writeUrl: replace also honors the equality gate', () => {
  resetDoubles(`#/app?type=c&id=${UUID}`)
  writeUrl({ type: 'c', id: UUID }, { replace: true })
  assert.equal(replaces.length, 0)
})

console.log(`\n${passed} tests passed`)
