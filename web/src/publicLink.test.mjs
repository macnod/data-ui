// Dependency-free publicLink.ts tests: transpile the TS source
// with the installed typescript package, import the ES module
// through a data URL, and run assertions with node:assert/strict.
// Leaves no generated file. Run:
//   npm run test:public-link
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import url from 'node:url'
import ts from 'typescript'

const here = path.dirname(url.fileURLToPath(import.meta.url))
const src = fs.readFileSync(
  path.join(here, 'publicLink.ts'), 'utf8')
const out = ts.transpileModule(src, {
  compilerOptions: {
    module: ts.ModuleKind.ESNext,
    target: ts.ScriptTarget.ES2020
  }
}).outputText
const mod = await import(
  'data:text/javascript;base64,'
  + Buffer.from(out).toString('base64'))
const { hashPermalink, bestLinkUrl } = mod

let passed = 0
function ok(name, fn) {
  fn()
  passed++
  console.log(`ok - ${name}`)
}

const UUID = 'c0ffee00-1234-5678-9abc-def012345678'

// ---- hashPermalink ----
ok('hashPermalink: reproduces the exact old string', () => {
  assert.equal(
    hashPermalink('https://app.example', '/', 'posts', UUID),
    `https://app.example/#/app?type=posts&id=${UUID}`)
})
ok('hashPermalink: pathname with segments is preserved', () => {
  assert.equal(
    hashPermalink('https://app.example', '/some/base/', 'posts', UUID),
    `https://app.example/some/base/#/app?type=posts&id=${UUID}`)
})
ok('hashPermalink: type and id are encoded', () => {
  assert.equal(
    hashPermalink('http://x', '/', 'my type', 'a/b'),
    'http://x/#/app?type=my%20type&id=a%2Fb')
})
ok('hashPermalink: no mode and no list params', () => {
  const s = hashPermalink('http://x', '/', 'posts', UUID)
  assert.ok(!s.includes('mode='))
  assert.ok(!s.includes('page='))
  assert.ok(!s.includes('q='))
})

// ---- bestLinkUrl ----
ok('bestLinkUrl: public string wins', () => {
  assert.equal(
    bestLinkUrl('https://blog.example/p/posts/x', 'http://x/#/app'),
    'https://blog.example/p/posts/x')
})
ok('bestLinkUrl: undefined falls back', () => {
  assert.equal(bestLinkUrl(undefined, 'HASH'), 'HASH')
})
ok('bestLinkUrl: null falls back', () => {
  assert.equal(bestLinkUrl(null, 'HASH'), 'HASH')
})
ok('bestLinkUrl: empty string falls back', () => {
  assert.equal(bestLinkUrl('', 'HASH'), 'HASH')
})
ok('bestLinkUrl: [] (the plist-to-json nil shape) falls back', () => {
  assert.equal(bestLinkUrl([], 'HASH'), 'HASH')
})
ok('bestLinkUrl: a number falls back', () => {
  assert.equal(bestLinkUrl(42, 'HASH'), 'HASH')
})
ok('bestLinkUrl: an object falls back', () => {
  assert.equal(bestLinkUrl({ url: 'x' }, 'HASH'), 'HASH')
})

console.log(`\n${passed} tests passed`)
