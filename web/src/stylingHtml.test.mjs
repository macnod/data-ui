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
const { slotFromInfo } = mod

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

console.log(`\n${passed} tests passed`)
