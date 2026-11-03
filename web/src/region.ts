// Region renderer pure helpers (plan 03 — the "Region renderer"
// child of "Embedded child region"). The Region component in
// App.tsx consumes these; they are dependency-free and
// unit-tested by region.test.mjs (npm run test:region). No React,
// no fetch, no App state — anything that touches those lives in
// the component.

// Re-quote one source line one nesting level deeper:
//   "text"       → "> text"
//   "> text"     → "> > text"
//   "> > deeper" → "> > > deeper"
// Depth is speaker identity: each reply generation adds exactly
// one marker, so an N-deep thread renders as a nested transcript
// (email style) instead of a flattened soup of attributions. Each
// leading ">" plus one optional following space is one level; a
// blank line re-quotes to a bare marker at that depth (">", "> >")
// so paragraph structure inside the quote survives re-quoting.
function quoteLine(line: string): string {
  let rest = line
  let depth = 0
  while (rest.startsWith('>')) {
    depth++
    rest = rest.slice(1)
    if (rest.startsWith(' ')) rest = rest.slice(1)
  }
  const marker = '> '.repeat(depth + 1)
  return rest === '' ? marker.trimEnd() : marker + rest
}

// Compose a Reply preset from a quoted record. The attribution
// line comes only from a nonblank string `user` field: undefined,
// null, [], empty, and whitespace-only users are missing (a
// missing user produces an empty preset — no attribution and no
// quote, never a quote of unknown provenance). Attribution is
// never lifted from the quoted body: a forged attribution inside
// the body remains body text. A bare ">" line follows the
// attribution so it renders as its own paragraph — consecutive
// plain lines would merge into one soft-break paragraph and the
// quote's first body line would run into "X wrote:". Nesting
// depth grows one level per reply generation (quoteLine). The
// preset ends with a blank line: without it the first line the
// user types below the quote lazy-continues the quote's last
// paragraph (CommonMark: an unmarked, nonblank line extends a
// quoted paragraph), swallowing the reply into the quote.
export function composeReply(
  user: unknown, body: string
): string {
  const attributed =
    typeof user === 'string' && user.trim() !== ''
  if (!attributed) return ''
  const lines = String(body).split('\n')
  return ['> **' + user + '** wrote:', '>',
    ...lines.map(quoteLine), '', ''].join('\n')
}

// Ensure reply text never lazy-joins a quote (CommonMark: a
// nonblank, unmarked line after a quoted line continues the
// quote's paragraph — the user's text would render inside the
// quote). After every run of quote lines, insert a blank line
// when the next line is nonblank and not itself a quote line;
// already-separated text passes through unchanged, and a body
// that ends on the quote keeps its shape (nothing follows to
// separate). Applied at submit (submitPanel), so the stored
// text is normalized regardless of where the caret was.
// The insertion is semantics-preserving for intentional text:
// blocks that can interrupt a paragraph (lists, headings)
// render identically with or without the blank.
export function separateQuotes(body: string): string {
  const isQuote = (s: string) => /^\s*>/.test(s)
  const lines = String(body).split('\n')
  const out: string[] = []
  for (let i = 0; i < lines.length; i++) {
    out.push(lines[i])
    const next = lines[i + 1]
    if (next !== undefined && isQuote(lines[i])
        && next.trim() !== '' && !isQuote(next)) {
      out.push('')
    }
  }
  return out.join('\n')
}

// Normalize the wire's type-roles entries for an insert payload:
// each string maps to itself, each array to its element zero
// (02's nested :type-roles spelling — legal model syntax that
// rides list-result's verbatim :type-roles — would otherwise be
// sent as-is and make the backend's be-id lookup treat
// permission names as role names). Retain only nonempty strings.
// Never put roles inside data; omitted roles means private.
export function normalizeRoles(
  typeRoles: unknown
): string[] {
  if (!Array.isArray(typeRoles)) return []
  const out: string[] = []
  for (const entry of typeRoles) {
    const role = typeof entry === 'string'
      ? entry
      : Array.isArray(entry) && entry.length > 0
        ? entry[0] : null
    if (typeof role === 'string' && role !== '') {
      out.push(role)
    }
  }
  return out
}

// Last-page offset from a fresh total and the limit actually
// sent: offset = (ceil(total / limit) - 1) * limit. total 0
// uses offset 0 before any ceil arithmetic.
export function lastOffset(total: number, limit: number): number {
  if (total <= 0) return 0
  return (Math.ceil(total / limit) - 1) * limit
}

// Build the region insert payload's `data` object. Membership:
// the child's emitted add-form keys union the preset FK
// (spec.via) — the backend may omit a backend-hidden FK from the
// emitted add-form, and the preset must still be sent — minus
// `roles` (which rides top-level). Values preserve their JSON
// type: present nulls survive serialization, undefined
// properties disappear, nonblank strings are NOT trimmed, and
// blank/whitespace-only strings are omitted exactly as the
// normal submitForm path does. The hidden preset FK is assigned
// unconditionally after filtering, so it survives even when the
// backend omits its schema. An untouched `user` stays absent
// (not null, not '') so the fixture's :autofill :user can supply
// the current username — a present blank or null user would
// suppress autofill.
export interface RegionValues {
  [key: string]: unknown
}

export function regionPayloadData(
  addFormKeys: string[],
  via: string,
  values: RegionValues
): Record<string, unknown> {
  const allowed = new Set<string>(addFormKeys)
  allowed.add(via)
  allowed.delete('roles')
  const out: Record<string, unknown> = {}
  for (const key of allowed) {
    const v = values[key]
    if (typeof v === 'string' && v.trim() === '') continue
    if (v === undefined) continue
    out[key] = v
  }
  out[via] = values[via]
  return out
}

// The insert verdict from a POST /api/insert response: a new
// insertion requires HTTP OK, status === 'success',
// result.inserted === true, and a nonempty string result.id.
// inserted:false (the duplicate no-op — the duplicate test is
// all filled local fields, not identity keys) is 'noop';
// anything else is 'invalid'.
export type InsertOutcome =
  'inserted' | 'noop' | 'invalid'

export function insertOutcome(
  ok: boolean, body: any
): InsertOutcome {
  if (!ok) return 'invalid'
  if (body?.status !== 'success') return 'invalid'
  if (body?.result?.inserted === true
      && typeof body?.result?.id === 'string'
      && body.result.id !== '') {
    return 'inserted'
  }
  if (body?.result?.inserted === false) return 'noop'
  return 'invalid'
}
