// Dependency-free columns.ts tests — same transpile + data-URL
// import pattern as chipNav.test.mjs (this host's Node 22 lacks
// native TS stripping). Run: npm run test:columns
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import url from 'node:url'
import ts from 'typescript'

const here = path.dirname(url.fileURLToPath(import.meta.url))
const src = fs.readFileSync(
  path.join(here, 'columns.ts'), 'utf8')
const out = ts.transpileModule(src, {
  compilerOptions: {
    module: ts.ModuleKind.ESNext,
    target: ts.ScriptTarget.ES2020
  }
}).outputText
const mod = await import(
  'data:text/javascript;base64,'
  + Buffer.from(out).toString('base64'))
const { columnsStorageKey, deleteColumnStorageKey,
        parseHiddenColumns, visibleListFields,
        supportsColumnVisibility } = mod

let passed = 0
function ok(name, fn) {
  fn()
  passed++
  console.log(`ok - ${name}`)
}

// ---- key builders ----
ok('key: hidden-columns key shape', () => {
  assert.equal(
    columnsStorageKey('scenebook', 'scenes'),
    'data-ui-hidden-columns:scenebook:scenes')
})
ok('key: delete-column key shape', () => {
  assert.equal(
    deleteColumnStorageKey('todos', 'todo'),
    'data-ui-delete-column:todos:todo')
})
ok('key: hidden and delete keys differ for one type', () => {
  assert.notEqual(
    columnsStorageKey('m', 't'),
    deleteColumnStorageKey('m', 't'))
})

// ---- parseHiddenColumns ----
ok('parse: absent (null) → []', () => {
  assert.deepEqual(parseHiddenColumns(null), [])
})
ok('parse: empty string → []', () => {
  assert.deepEqual(parseHiddenColumns(''), [])
})
ok('parse: malformed JSON → []', () => {
  assert.deepEqual(parseHiddenColumns('["created",'), [])
})
ok('parse: non-array value → []', () => {
  assert.deepEqual(parseHiddenColumns('{"created":1}'), [])
})
ok('parse: non-string members filtered out', () => {
  assert.deepEqual(
    parseHiddenColumns('["created", 42, null, true, "name"]'),
    ['created', 'name'])
})
ok('parse: happy path', () => {
  assert.deepEqual(
    parseHiddenColumns('["created","updated"]'),
    ['created', 'updated'])
})
ok('parse: duplicates pass through', () => {
  // The render's Set makes them harmless; the parse does not
  // de-duplicate (a write computed from the parsed array would
  // otherwise silently drop what storage claimed to hold).
  assert.deepEqual(
    parseHiddenColumns('["a","a","b"]'),
    ['a', 'a', 'b'])
})

// ---- visibleListFields ----
ok('visible: no hidden → all, order kept', () => {
  assert.deepEqual(
    visibleListFields(['name', 'created', 'updated'], []),
    ['name', 'created', 'updated'])
})
ok('visible: subset hidden', () => {
  assert.deepEqual(
    visibleListFields(['name', 'created', 'updated'],
      ['created', 'updated']),
    ['name'])
})
ok('visible: unknown stored names ignored (schema drift)', () => {
  assert.deepEqual(
    visibleListFields(['name', 'created'], ['gone', 'name']),
    ['created'])
})
ok('visible: all hidden → []', () => {
  assert.deepEqual(
    visibleListFields(['name', 'created'], ['name', 'created']),
    [])
})
ok('visible: duplicates in hidden are harmless', () => {
  assert.deepEqual(
    visibleListFields(['a', 'b'], ['a', 'a']),
    ['b'])
})

// ---- supportsColumnVisibility ----
const formsResponse = {
  'list-form': { name: {} },
  'add-form': { name: {} },
  'update-form': { name: {} },
  'view-form': { name: {} },
  records: []
}
ok('support: ordinary response with forms → true', () => {
  assert.equal(supportsColumnVisibility(formsResponse), true)
})
ok('support: view-form alone → true', () => {
  assert.equal(supportsColumnVisibility(
    { 'list-form': {}, 'view-form': {}, records: [] }), true)
})
ok('support: list-form-only → false (rollup AND form-less ordinary)', () => {
  // The rollup shape and an ordinary type declaring no forms
  // are wire-identical — both are excluded, the accepted v1
  // scope (the escape is declaring a :view-form). No response
  // key distinguishes them; the test documents the collision
  // rather than pretending to tell them apart.
  assert.equal(supportsColumnVisibility(
    { 'list-form': {}, records: [] }), false)
})
ok('support: undefined → false', () => {
  assert.equal(supportsColumnVisibility(undefined), false)
})
ok('support: empty object → false', () => {
  assert.equal(supportsColumnVisibility({}), false)
})
ok('support: forms-bearing page shape → true (unreachable on the list path)', () => {
  // A page response carries view-form but has no list-form,
  // so the dead-shell return renders it before the list path
  // ever consults this predicate — documented, not guarded.
  assert.equal(supportsColumnVisibility(
    { 'view-form': {}, records: [] }), true)
})

console.log(`\n${passed} tests passed`)
