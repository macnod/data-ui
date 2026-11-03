// Dependency-free region.ts tests — same transpile + data-URL
// import pattern as urlState.test.mjs (this host's Node 22
// lacks native TS stripping). Run: npm run test:region
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import url from 'node:url'
import ts from 'typescript'

const here = path.dirname(url.fileURLToPath(import.meta.url))
const src = fs.readFileSync(
  path.join(here, 'region.ts'), 'utf8')
const out = ts.transpileModule(src, {
  compilerOptions: {
    module: ts.ModuleKind.ESNext,
    target: ts.ScriptTarget.ES2020
  }
}).outputText
const mod = await import(
  'data:text/javascript;base64,'
  + Buffer.from(out).toString('base64'))
const { composeReply, normalizeRoles, lastOffset,
        regionPayloadData, insertOutcome, separateQuotes } = mod

let passed = 0
function ok(name, fn) {
  fn()
  passed++
  console.log(`ok - ${name}`)
}

// ---- composeReply: quote depth ----
ok('quote: plain text gains one level', () => {
  assert.equal(composeReply('alice', 'text'),
    '> **alice** wrote:\n>\n> text\n\n')
})
ok('quote: one-level source gains a second level', () => {
  assert.equal(composeReply('alice', '> text'),
    '> **alice** wrote:\n>\n> > text\n\n')
})
ok('quote: two-level source gains a third level', () => {
  assert.equal(composeReply('alice', '> > deeper'),
    '> **alice** wrote:\n>\n> > > deeper\n\n')
})
ok('quote: three-level source gains a fourth level', () => {
  assert.equal(composeReply('alice', '> > > deeper'),
    '> **alice** wrote:\n>\n> > > > deeper\n\n')
})
ok('quote: no space after marker also gains a level', () => {
  assert.equal(composeReply('alice', '>text'),
    '> **alice** wrote:\n>\n> > text\n\n')
})
ok('quote: multiline, each line one level deeper', () => {
  assert.equal(composeReply('alice', 'first\n> second\n> > third'),
    '> **alice** wrote:\n>\n> first\n> > second\n> > > third\n\n')
})
ok('quote: source text beyond the marker is preserved', () => {
  assert.equal(composeReply('alice', '>  spaced'),
    '> **alice** wrote:\n>\n> >  spaced\n\n')
})
ok('separator: bare ">" line follows the attribution', () => {
  assert.equal(composeReply('alice', 'text').split('\n')[1], '>')
})
ok('preset ends with a blank line (lazy-continuation guard)', () => {
  // Without it, the first line typed below the quote extends the
  // quote's last paragraph (CommonMark lazy continuation).
  const preset = composeReply('alice', 'text')
  assert.ok(preset.endsWith('\n\n'))
  assert.equal(preset.split('\n').at(-1), '')
  assert.equal(preset.split('\n').at(-2), '')
})
ok('quote: blank source line → bare marker, paragraphs survive', () => {
  assert.equal(composeReply('alice', 'a\n\nb'),
    '> **alice** wrote:\n>\n> a\n>\n> b\n\n')
})
ok('quote: blank quoted line re-quotes one level deeper', () => {
  assert.equal(composeReply('alice', '>\n> text'),
    '> **alice** wrote:\n>\n> >\n> > text\n\n')
})
ok('user: missing (undefined) → empty preset', () => {
  assert.equal(composeReply(undefined, 'body'), '')
})
ok('user: null → empty preset', () => {
  assert.equal(composeReply(null, 'body'), '')
})
ok('user: [] → empty preset', () => {
  assert.equal(composeReply([], 'body'), '')
})
ok('user: empty string → empty preset', () => {
  assert.equal(composeReply('', 'body'), '')
})
ok('user: whitespace-only → empty preset', () => {
  assert.equal(composeReply('   ', 'body'), '')
})
ok('forged attribution in body stays quoted body text', () => {
  const preset = composeReply('alice',
    '**bob** wrote:\nhi')
  assert.ok(preset.includes('> **bob** wrote:'))
  assert.equal(preset.split('\n')[0], '> **alice** wrote:')
})
ok('depth grows one level per reply generation (transcript)', () => {
  // alice posts 'hello'; bob replies (quotes her body at depth 1);
  // carol replies to bob (bob's whole comment re-quoted, so the
  // attribution lands at depth 2 and alice's body at depth 2).
  // Trailing blank line trimmed for the inner-quote simulation.
  const bob = composeReply('bob', 'hello').trimEnd()
  const carol = composeReply('carol', bob)
  assert.ok(carol.includes('\n> > **bob** wrote:'))
  assert.ok(carol.includes('\n> > hello'))
})

// ---- separateQuotes (lazy-join guard at submit) ----
ok('separate: text after quote run gains a blank line', () => {
  assert.equal(separateQuotes('> a\n> b\nuser'),
    '> a\n> b\n\nuser')
})
ok('separate: already-separated body unchanged', () => {
  assert.equal(separateQuotes('> a\n\nuser'),
    '> a\n\nuser')
})
ok('separate: body ending on the quote unchanged', () => {
  assert.equal(separateQuotes('user\n> a'), 'user\n> a')
})
ok('separate: plain text unchanged', () => {
  assert.equal(separateQuotes('hello\nworld'),
    'hello\nworld')
})
ok('separate: interleaved runs separated too', () => {
  assert.equal(separateQuotes('> a\nmid\n> b\nend'),
    '> a\n\nmid\n> b\n\nend')
})
ok('separate: indented quote line counts as quoted', () => {
  assert.equal(separateQuotes('  > a\nuser'),
    '  > a\n\nuser')
})
ok('separate: the caret-in-quote trap fixed end-to-end', () => {
  // User clicks into the last quoted line and types there; the
  // preset's trailing blank was consumed by the edit. Submit
  // must still separate the typed text from the quote.
  const typed = '> **alice** wrote:\n>\n> hi\ntyped here'
  assert.equal(separateQuotes(typed),
    '> **alice** wrote:\n>\n> hi\n\ntyped here')
})

// ---- normalizeRoles ----
ok('roles: flat strings retained', () => {
  assert.deepEqual(
    normalizeRoles(['public', 'comments-user']),
    ['public', 'comments-user'])
})
ok('roles: nested arrays take element zero', () => {
  assert.deepEqual(
    normalizeRoles([
      'public',
      ['comments-user', 'create', 'read']
    ]),
    ['public', 'comments-user'])
})
ok('roles: malformed entries dropped', () => {
  assert.deepEqual(
    normalizeRoles(['public', 42, null, {}, []]),
    ['public'])
})
ok('roles: empty strings dropped', () => {
  assert.deepEqual(normalizeRoles(['', 'public']), ['public'])
})
ok('roles: non-array input → []', () => {
  assert.deepEqual(normalizeRoles(undefined), [])
  assert.deepEqual(normalizeRoles('public'), [])
  assert.deepEqual(normalizeRoles(null), [])
})

// ---- lastOffset ----
ok('lastOffset: total 0 → 0', () => {
  assert.equal(lastOffset(0, 20), 0)
})
ok('lastOffset: total 1 → 0', () => {
  assert.equal(lastOffset(1, 20), 0)
})
ok('lastOffset: total 20 (full page) → 0', () => {
  assert.equal(lastOffset(20, 20), 0)
})
ok('lastOffset: total 21 → page 2', () => {
  assert.equal(lastOffset(21, 20), 20)
})
ok('lastOffset: total 40 → page 2', () => {
  assert.equal(lastOffset(40, 20), 20)
})
ok('lastOffset: total 41 → page 3', () => {
  assert.equal(lastOffset(41, 20), 40)
})

// ---- regionPayloadData ----
ok('payload: membership = add-form keys ∪ via, minus roles',
  () => {
    const data = regionPayloadData(
      ['body', 'user', 'post'], 'post',
      { body: 'hi', user: 'alice', post: 'UUID-1' })
    assert.deepEqual(data,
      { body: 'hi', user: 'alice', post: 'UUID-1' })
  })
ok('payload: roles never in data', () => {
  const data = regionPayloadData(
    ['body', 'roles'], 'post',
    { body: 'hi', roles: ['public'] })
    assert.equal('roles' in data, false)
})
ok('payload: blank string omitted', () => {
  const data = regionPayloadData(
    ['body', 'user'], 'post',
    { body: 'hi', user: '   ' })
    assert.equal('user' in data, false)
})
ok('payload: untouched user absent (autofill)', () => {
  const data = regionPayloadData(
    ['body', 'user'], 'post', { body: 'hi' })
  assert.equal('user' in data, false)
  assert.equal(data.user, undefined)
})
ok('payload: explicit null preserved', () => {
  const data = regionPayloadData(
    ['body', 'user'], 'post',
    { body: 'hi', user: null })
  assert.equal('user' in data, true)
  assert.equal(data.user, null)
})
ok('payload: nonblank string not trimmed', () => {
  const data = regionPayloadData(
    ['body'], 'post', { body: '  padded  ' })
  assert.equal(data.body, '  padded  ')
})
ok('payload: hidden via preset survives absence from add-form',
  () => {
    const data = regionPayloadData(
      ['body', 'user'], 'post',
      { body: 'hi', post: 'UUID-2' })
    assert.equal(data.post, 'UUID-2')
  })
ok('payload: sparse — only presets initialize', () => {
  const data = regionPayloadData(
    ['body', 'user', 'post'], 'post',
    { post: 'UUID-3' })
  assert.deepEqual(data, { post: 'UUID-3' })
})

// ---- insertOutcome ----
ok('outcome: full success shape → inserted', () => {
  assert.equal(insertOutcome(true,
    { status: 'success',
      result: { inserted: true, id: 'UUID' } }),
    'inserted')
})
ok('outcome: inserted false → noop', () => {
  assert.equal(insertOutcome(true,
    { status: 'success',
      result: { inserted: false } }),
    'noop')
})
ok('outcome: non-OK → invalid', () => {
  assert.equal(insertOutcome(false,
    { status: 'success',
      result: { inserted: true, id: 'UUID' } }),
    'invalid')
})
ok('outcome: empty id → invalid', () => {
  assert.equal(insertOutcome(true,
    { status: 'success',
      result: { inserted: true, id: '' } }),
    'invalid')
})
ok('outcome: unparseable body → invalid', () => {
  assert.equal(insertOutcome(true, null), 'invalid')
})

console.log(`\n${passed} tests passed`)
