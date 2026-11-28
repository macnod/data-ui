// Dependency-free savedViews.ts tests — same transpile + data-URL
// import pattern as chipNav.test.mjs (this host's Node 22 lacks
// native TS stripping). Run: npm run test:saved-views
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import url from 'node:url'
import ts from 'typescript'

const here = path.dirname(url.fileURLToPath(import.meta.url))
const src = fs.readFileSync(
  path.join(here, 'savedViews.ts'), 'utf8')
const out = ts.transpileModule(src, {
  compilerOptions: {
    module: ts.ModuleKind.ESNext,
    target: ts.ScriptTarget.ES2020
  }
}).outputText
const mod = await import(
  'data:text/javascript;base64,'
  + Buffer.from(out).toString('base64'))
const { viewFromRecord, viewsForType, normalizeViewName,
        planSave } = mod

let passed = 0
function ok(name, fn) {
  fn()
  passed++
  console.log(`ok - ${name}`)
}

// The module must stay import-free: the data-URL import above only
// works while it is, and the URL format belongs to urlState.ts.
ok('module: imports nothing', () => {
  assert.equal(/^\s*import\s/m.test(src), false)
})

// ---- viewFromRecord ----
const wire = { id: 'UUID-1', name: 'Working',
  'list-type': 'todos', query: 'type=todos&sort=-name' }
ok('record: a full wire record → a SavedView', () => {
  assert.deepEqual(viewFromRecord(wire),
    { id: 'UUID-1', name: 'Working', listType: 'todos',
      query: 'type=todos&sort=-name' })
})
ok('record: null / undefined / non-object → null', () => {
  assert.equal(viewFromRecord(null), null)
  assert.equal(viewFromRecord(undefined), null)
  assert.equal(viewFromRecord('x'), null)
  assert.equal(viewFromRecord(42), null)
})
ok('record: each missing key → null', () => {
  for (const key of ['id', 'name', 'list-type', 'query']) {
    const rec = { ...wire }
    delete rec[key]
    assert.equal(viewFromRecord(rec), null, 'missing ' + key)
  }
})
ok('record: each empty-string key → null', () => {
  for (const key of ['id', 'name', 'list-type', 'query']) {
    assert.equal(viewFromRecord({ ...wire, [key]: '' }), null,
      'empty ' + key)
  }
})
ok('record: each non-string key → null', () => {
  for (const key of ['id', 'name', 'list-type', 'query']) {
    for (const bad of [42, true, null, [], {}]) {
      assert.equal(viewFromRecord({ ...wire, [key]: bad }), null,
        `${key}: ${JSON.stringify(bad)}`)
    }
  }
})
ok('record: extra wire keys are dropped', () => {
  assert.deepEqual(
    viewFromRecord({ ...wire, user: 'alice', 'created-at': 'x' }),
    viewFromRecord(wire))
})
ok('record: a camelCase listType is not the wire key', () => {
  // the REST edge sends hyphenated keys
  const { 'list-type': _drop, ...camel } = wire
  assert.equal(viewFromRecord({ ...camel, listType: 'todos' }), null)
})

// ---- viewsForType ----
const v = (id, name, listType) =>
  ({ id, name, listType, query: 'type=' + listType })
const all = [
  v('1', 'Working', 'todos'), v('2', 'Active', 'todos'),
  v('3', 'Zed', 'tags'), v('4', 'Mine', 'tags'),
  v('5', 'Beta', 'scenes'), v('6', 'alpha', 'scenes'),
  v('7', 'Working', 'Todos')
]
ok('viewsForType: keeps only the type, sorted by name', () => {
  assert.deepEqual(
    viewsForType(all, 'todos').map(x => x.id), ['2', '1'])
  assert.deepEqual(
    viewsForType(all, 'tags').map(x => x.id), ['4', '3'])
  assert.deepEqual(
    viewsForType(all, 'scenes').map(x => x.id), ['5', '6'])
})
ok('viewsForType: case-sensitive on both the type and the sort', () => {
  // 'Todos' is another type entirely
  assert.deepEqual(viewsForType(all, 'Todos').map(x => x.id), ['7'])
  // code-unit order: an uppercase name sorts before a lowercase one
  assert.deepEqual(
    viewsForType(all, 'scenes').map(x => x.name),
    ['Beta', 'alpha'])
})
ok('viewsForType: no match → []', () => {
  assert.deepEqual(viewsForType(all, 'nope'), [])
  assert.deepEqual(viewsForType([], 'todos'), [])
})
ok('viewsForType: never mutates or reorders its input', () => {
  const input = [v('1', 'b', 't'), v('2', 'a', 't')]
  const snapshot = JSON.stringify(input)
  const out = viewsForType(input, 't')
  assert.deepEqual(out.map(x => x.id), ['2', '1'])
  assert.equal(JSON.stringify(input), snapshot)
  assert.notEqual(out, input)
})

// ---- normalizeViewName ----
ok('normalizeViewName: trims both ends only', () => {
  assert.equal(normalizeViewName('  Working  '), 'Working')
  assert.equal(normalizeViewName('\tWorking\n'), 'Working')
  assert.equal(normalizeViewName('My  View'), 'My  View')
  assert.equal(normalizeViewName('working'), 'working')
  assert.equal(normalizeViewName(''), '')
  assert.equal(normalizeViewName('   '), '')
})

// ---- planSave ----
const todos = viewsForType(all, 'todos')
ok('planSave: empty and whitespace names → none', () => {
  assert.deepEqual(planSave(todos, ''), { kind: 'none' })
  assert.deepEqual(planSave(todos, '   '), { kind: 'none' })
  assert.deepEqual(planSave(todos, '\t\n'), { kind: 'none' })
})
ok('planSave: a new name → insert, trimmed', () => {
  assert.deepEqual(planSave(todos, 'Fresh'),
    { kind: 'insert', name: 'Fresh' })
  assert.deepEqual(planSave(todos, '  Fresh '),
    { kind: 'insert', name: 'Fresh' })
})
ok('planSave: an exact match → update with that id', () => {
  assert.deepEqual(planSave(todos, 'Working'),
    { kind: 'update', id: '1', name: 'Working' })
  assert.deepEqual(planSave(todos, ' Active '),
    { kind: 'update', id: '2', name: 'Active' })
})
ok('planSave: the name is trimmed before the match', () => {
  // an untrimmed match would insert a look-alike row
  assert.equal(planSave(todos, 'Working ').kind, 'update')
})
ok('planSave: a case-different name is a new name', () => {
  assert.deepEqual(planSave(todos, 'working'),
    { kind: 'insert', name: 'working' })
  assert.deepEqual(planSave(todos, 'WORKING'),
    { kind: 'insert', name: 'WORKING' })
})
ok('planSave: matches only the views it is given (the current type)',
  () => {
    // 'Working' exists on todos and on Todos; on tags it is new
    assert.equal(planSave(viewsForType(all, 'tags'), 'Working').kind,
      'insert')
    assert.deepEqual(planSave(viewsForType(all, 'Todos'), 'Working'),
      { kind: 'update', id: '7', name: 'Working' })
    assert.equal(planSave([], 'Working').kind, 'insert')
  })

console.log(`\n${passed} tests passed`)
