// Dependency-free chipNav.ts tests — same transpile + data-URL
// import pattern as region.test.mjs (this host's Node 22 lacks
// native TS stripping). Run: npm run test:chip-nav
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import url from 'node:url'
import ts from 'typescript'

const here = path.dirname(url.fileURLToPath(import.meta.url))
const src = fs.readFileSync(
  path.join(here, 'chipNav.ts'), 'utf8')
const out = ts.transpileModule(src, {
  compilerOptions: {
    module: ts.ModuleKind.ESNext,
    target: ts.ScriptTarget.ES2020
  }
}).outputText
const mod = await import(
  'data:text/javascript;base64,'
  + Buffer.from(out).toString('base64'))
const { chipClickAction, refNavTarget, m2mViewNav,
        lookupKey, parseValueIdResult, suppressChipClick,
        CHIP_DBLCLICK_MS } = mod

let passed = 0
function ok(name, fn) {
  fn()
  passed++
  console.log(`ok - ${name}`)
}

const nav = { type: 'characters', id: 'UUID-1' }

// ---- chipClickAction ----
ok('click: unhighlighted, no nav → filter', () => {
  assert.equal(chipClickAction(false, null), 'filter')
})
ok('click: unhighlighted with nav → still filter', () => {
  assert.equal(chipClickAction(false, nav), 'filter')
})
ok('click: highlighted, no nav → unfilter', () => {
  assert.equal(chipClickAction(true, null), 'unfilter')
})
ok('click: highlighted with nav → navigate', () => {
  assert.equal(chipClickAction(true, nav), 'navigate')
})

// ---- refNavTarget ----
ok('ref: happy path → {type, id}', () => {
  assert.deepEqual(
    refNavTarget('characters', 'UUID-1', 'Jenny',
      ['scenes', 'characters']),
    { type: 'characters', id: 'UUID-1' })
})
ok('ref: invisible target → null', () => {
  assert.equal(
    refNavTarget('costumes', 'UUID-1', 'Hat',
      ['scenes', 'characters']),
    null)
})
ok('ref: mixed-case target matched lowercase', () => {
  assert.deepEqual(
    refNavTarget('Characters', 'UUID-1', 'Jenny',
      ['scenes', 'characters']),
    { type: 'Characters', id: 'UUID-1' })
})
ok('ref: missing sibling → null', () => {
  assert.equal(
    refNavTarget('characters', undefined, 'Jenny',
      ['characters']),
    null)
})
ok('ref: empty sibling → null', () => {
  assert.equal(
    refNavTarget('characters', '', 'Jenny', ['characters']),
    null)
})
ok('ref: whitespace sibling → null', () => {
  assert.equal(
    refNavTarget('characters', '   ', 'Jenny',
      ['characters']),
    null)
})
ok('ref: non-string sibling → null', () => {
  assert.equal(
    refNavTarget('characters', 42, 'Jenny', ['characters']),
    null)
})
ok('ref: [] sibling (nil encoding) → null', () => {
  assert.equal(
    refNavTarget('characters', [], 'Jenny', ['characters']),
    null)
})
ok('ref: empty value → null', () => {
  assert.equal(
    refNavTarget('characters', 'UUID-1', '', ['characters']),
    null)
})
ok('ref: non-string target → null', () => {
  assert.equal(
    refNavTarget(undefined, 'UUID-1', 'Jenny',
      ['characters']),
    null)
})

// ---- m2mViewNav (view-form chip navigation) ----
const openFn = () => {}
const m2mMeta = { m2m: true, table: 'scene-characters',
  label: 'Scenes',
  lookup: { type: 'scenes', field: 'label' } }
ok('m2m view: happy path → lookup', () => {
  assert.deepEqual(
    m2mViewNav(m2mMeta, openFn, ['scenes', 'characters']),
    { type: 'scenes', field: 'label' })
})
ok('m2m view: invisible target → null', () => {
  assert.equal(
    m2mViewNav(m2mMeta, openFn, ['characters', 'photos']),
    null)
})
ok('m2m view: mixed-case target matched lowercase', () => {
  assert.deepEqual(
    m2mViewNav({ ...m2mMeta,
      lookup: { type: 'Scenes', field: 'label' } },
      openFn, ['scenes']),
    { type: 'Scenes', field: 'label' })
})
ok('m2m view: no meta (bare :list field) → null', () => {
  assert.equal(
    m2mViewNav(undefined, openFn, ['scenes']), null)
})
ok('m2m view: reference-family meta → null', () => {
  assert.equal(
    m2mViewNav({ m2m: false, table: 'scenes',
      column: 'label', label: 'Scene',
      target: 'scenes' }, openFn, ['scenes']),
    null)
})
ok('m2m view: M2M without lookup facts → null', () => {
  assert.equal(
    m2mViewNav({ m2m: true, table: 't', label: 'L' },
      openFn, ['scenes']),
    null)
})
ok('m2m view: no open handler → null', () => {
  assert.equal(
    m2mViewNav(m2mMeta, null, ['scenes']), null)
})
ok('m2m view: non-function open → null', () => {
  assert.equal(
    m2mViewNav(m2mMeta, 'open', ['scenes']), null)
})

// ---- parseValueIdResult ----
ok('value-id: string result → id', () => {
  assert.equal(
    parseValueIdResult({ status: 'success',
      result: 'UUID-1' }),
    'UUID-1')
})
ok('value-id: [] result (nil encoding) → null', () => {
  assert.equal(
    parseValueIdResult({ status: 'success', result: [] }),
    null)
})
ok('value-id: null result → null', () => {
  assert.equal(
    parseValueIdResult({ status: 'success', result: null }),
    null)
})
ok('value-id: empty object → null', () => {
  assert.equal(parseValueIdResult({}), null)
})
ok('value-id: null body → null', () => {
  assert.equal(parseValueIdResult(null), null)
})
ok('value-id: empty string result → null', () => {
  assert.equal(
    parseValueIdResult({ status: 'success', result: '' }),
    null)
})

// ---- lookupKey ----
ok('key: embedded separator cannot forge a collision', () => {
  // A |-style join of the tuple would let a value containing
  // the separator collide with another entry's key.
  assert.notEqual(
    lookupKey(0, 'tags', 'name', 'red|blue'),
    lookupKey(0, 'tags', 'name', 'red'))
})
ok('key: values differing only mid-string stay distinct', () => {
  assert.notEqual(
    lookupKey(1, 'tags', 'name', 're]d'),
    lookupKey(1, 'tags', 'name', 'red'))
})
ok('key: identical tuples collide (cacheable)', () => {
  assert.equal(
    lookupKey(7, 'tags', 'name', 'red'),
    lookupKey(7, 'tags', 'name', 'red'))
})

// ---- suppressChipClick (double-click guard) ----
ok('guard: detail 0, no prior filter click → allowed', () => {
  assert.equal(suppressChipClick(0, null), false)
})
ok('guard: detail 1, no prior filter click → allowed', () => {
  assert.equal(suppressChipClick(1, null), false)
})
ok('guard: detail 2 (2nd click of a double-click) → suppressed', () => {
  assert.equal(suppressChipClick(2, null), true)
})
ok('guard: detail 0, 100ms since filter → suppressed (double Enter)', () => {
  assert.equal(suppressChipClick(0, 100), true)
})
ok('guard: detail 0, 600ms since filter → allowed (navigates)', () => {
  assert.equal(suppressChipClick(0, 600), false)
})
ok('guard: detail 1, 100ms since filter → suppressed (remount case)', () => {
  // The button can remount when the list re-fetches, which
  // resets the browser's click count — the timestamp guard
  // catches what detail alone would miss.
  assert.equal(suppressChipClick(1, 100), true)
})
ok('guard: detail 1, 600ms since filter → allowed', () => {
  assert.equal(suppressChipClick(1, 600), false)
})
ok('guard: detail 2, 600ms since filter → still suppressed', () => {
  assert.equal(suppressChipClick(2, 600), true)
})
ok('guard: boundary — exactly the window is past it (<, not <=)', () => {
  assert.equal(suppressChipClick(1, CHIP_DBLCLICK_MS), false)
  assert.equal(suppressChipClick(1, CHIP_DBLCLICK_MS - 1), true)
})
ok('guard: the window is 500ms', () => {
  assert.equal(CHIP_DBLCLICK_MS, 500)
})

console.log(`\n${passed} tests passed`)
