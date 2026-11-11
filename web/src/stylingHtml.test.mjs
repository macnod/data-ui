// Dependency-free stylingHtml.ts tests: transpile the TS source
// with the installed typescript package, import the ES module
// through a data URL, and run assertions with node:assert/strict.
// Leaves no generated file. Run:
//   npm run test:styling-html
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import url from 'node:url'
import ts from 'typescript'

const here = path.dirname(url.fileURLToPath(import.meta.url))
const src = fs.readFileSync(
  path.join(here, 'stylingHtml.ts'), 'utf8')
const out = ts.transpileModule(src, {
  compilerOptions: {
    module: ts.ModuleKind.ESNext,
    target: ts.ScriptTarget.ES2020
  }
}).outputText
const mod = await import(
  'data:text/javascript;base64,'
  + Buffer.from(out).toString('base64'))
const { slotFromInfo, headingStyleFromInfo } = mod

let passed = 0
function ok(name, fn) {
  fn()
  passed++
  console.log(`ok - ${name}`)
}

const HTML = '<strong>Site</strong>'

// ---- banner key ----
ok('slotFromInfo: a string with content is the value', () => {
  assert.equal(
    slotFromInfo({ banner: HTML }, 'banner'), HTML)
})
ok('slotFromInfo: empty string is null', () => {
  assert.equal(slotFromInfo({ banner: '' }, 'banner'), null)
})
ok('slotFromInfo: whitespace-only string is null', () => {
  assert.equal(slotFromInfo({ banner: '  \t ' }, 'banner'), null)
})
ok('slotFromInfo: missing key is null', () => {
  assert.equal(slotFromInfo({ title: 'x' }, 'banner'), null)
})
ok('slotFromInfo: null result is null', () => {
  assert.equal(slotFromInfo(null, 'banner'), null)
})
ok('slotFromInfo: undefined result is null', () => {
  assert.equal(slotFromInfo(undefined, 'banner'), null)
})
ok('slotFromInfo: number value is null', () => {
  assert.equal(slotFromInfo({ banner: 42 }, 'banner'), null)
})
ok('slotFromInfo: [] (the plist-to-json nil shape) is null', () => {
  assert.equal(slotFromInfo({ banner: [] }, 'banner'), null)
})
ok('slotFromInfo: object value is null', () => {
  assert.equal(slotFromInfo({ banner: { html: 'x' } }, 'banner'), null)
})

// ---- footer key ----
ok('slotFromInfo: the footer key reads its own value', () => {
  assert.equal(
    slotFromInfo({ footer: HTML, banner: 'other' }, 'footer'), HTML)
})
ok('slotFromInfo: footer missing while banner set is null', () => {
  assert.equal(
    slotFromInfo({ banner: HTML }, 'footer'), null)
})

// ---- headingStyleFromInfo ----
// The canonical heading-slot value, exactly as probed through
// sanitize-styling-html: quoted font stack, negative
// letter-spacing, four declarations.
const CANON =
  '<span style="font-family: Georgia, \'Times New Roman\', serif;' +
  ' font-size: 36px; font-weight: 700; letter-spacing: -0.3px">' +
  '</span>'
const CANON_STYLE = {
  fontFamily: "Georgia, 'Times New Roman', serif",
  fontSize: '36px',
  fontWeight: '700',
  letterSpacing: '-0.3px'
}

ok('headingStyleFromInfo: the canonical probed span lifts all four declarations', () => {
  assert.deepEqual(headingStyleFromInfo(CANON), CANON_STYLE)
})
ok('headingStyleFromInfo: the self-closing spelling lifts the same style (the backend normalizes; the parser is shape-agnostic)', () => {
  assert.deepEqual(
    headingStyleFromInfo(CANON.replace('></span>', '/>')),
    CANON_STYLE)
})
ok('headingStyleFromInfo: no style attribute is null', () => {
  assert.equal(headingStyleFromInfo('<span>t</span>'), null)
})
ok('headingStyleFromInfo: an empty style attribute is null', () => {
  assert.equal(headingStyleFromInfo('<span style=""></span>'), null)
})
ok('headingStyleFromInfo: null input is null', () => {
  assert.equal(headingStyleFromInfo(null), null)
})
ok('headingStyleFromInfo: a non-string input is null', () => {
  assert.equal(headingStyleFromInfo(42), null)
})
ok('headingStyleFromInfo: a malformed declaration is skipped', () => {
  assert.deepEqual(
    headingStyleFromInfo('<span style="font-size: 30px; nonsense"></span>'),
    { fontSize: '30px' })
})
ok('headingStyleFromInfo: all-declarations-malformed is null (a broken row must not take the page down)', () => {
  assert.equal(
    headingStyleFromInfo('<span style="nonsense; also bad"></span>'),
    null)
})
ok('headingStyleFromInfo: two styled spans — the first style wins (the non-canonical pin)', () => {
  assert.deepEqual(
    headingStyleFromInfo(
      '<span style="font-size: 30px"></span>' +
      '<span style="color: red"></span>'),
    { fontSize: '30px' })
})

console.log(`\n${passed} tests passed`)
