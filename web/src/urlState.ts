// App state in the URL: serialize / parse the app's query state
// to/from the location hash. Format (decided 2026-10-07 /
// 2026-10-16): hash params, prefix "#/app?", e.g.
//   #/app?type=todo&q=x&notq=y&sort=-name&page=2
//     &filters=<JSON>&bool=<JSON>&hide-exclusive=false
// Not encoded: view mode (derived from the type's category),
// modals / lightbox, row selection, the un-trimmed input text.
// No routing library — the frontend stays minimal and
// schema-driven. This module is the single home of the format;
// writeUrl is the only hash writer in the app.

export interface UrlState {
  type?: string
  q?: string        // trimmed debounced search commit
  notq?: string     // trimmed debounced Not… commit
  filters?: Record<string, string[]>   // chips
  bool?: Record<string, 'yes' | 'no'>
  hideExclusive?: boolean
  sort?: string     // '-field' desc, bare 'field' asc
  page?: number     // 1-based
}

// The query half of App state, passed in by the callers — type,
// both debounced terms, listFilters, boolFilters, hideExclusive,
// sortField, sortDir, currentPage. No App import: urlState.ts
// stays free of App types.
export interface QueryFields {
  type: string
  debouncedSearch: string
  debouncedNotTerm: string
  listFilters: Record<string, string[]>
  boolFilters: Record<string, 'yes' | 'no'>
  hideExclusive: boolean
  sortField: string | null
  sortDir: 'asc' | 'desc'
  currentPage: number
}

const PREFIX = '#/app'

// Values-based filters emptiness: a record whose every values
// array is empty is empty ({roles: []} is what toggleChipFilter
// leaves behind after clearing the last chip). Key count alone
// would keep a dead param on the canonical URL — same emptiness
// rule the Clear button uses.
function filtersEmpty(
  f: Record<string, string[]> | undefined
): boolean {
  if (!f) return true
  return Object.values(f).every(v => !Array.isArray(v)
    || v.length === 0)
}

// canonicalize is the single home of every omission rule. Its
// contract is behavioral (plan pass 7): a key is dropped when it
// is absent OR present with its default value. The distinction
// is load-bearing — one of the two objects this function
// receives (buildUrlState's output) carries every key present
// with its live value (page: 1, q: '', filters: {}, bool: {},
// sort: null, raw hideExclusive). A weaker "copy defined keys,
// skip empties" implementation leaves page: 1 and
// hideExclusive: true in the object, and the Step-4 listener's
// equality gate then compares {type} against
// {type, page: 1, hideExclusive: true} and misses on every
// default the build carries.
export function canonicalize(s: UrlState): UrlState {
  const c: UrlState = {}
  if (typeof s.type === 'string' && s.type !== '') {
    c.type = s.type
  }
  if (typeof s.q === 'string' && s.q !== '') c.q = s.q
  if (typeof s.notq === 'string' && s.notq !== '') c.notq = s.notq
  if (!filtersEmpty(s.filters)) {
    // Values arrays sorted and de-duplicated (pass 6): chip
    // order is cosmetic, so a hand-edited reorder compares
    // equal in the gate instead of closing the open form.
    c.filters = Object.fromEntries(
      Object.entries(s.filters!)
        .filter(([, v]) => Array.isArray(v) && v.length > 0)
        .map(([k, v]) =>
          [k, [...new Set(v)].sort()])
    )
  }
  if (s.bool && typeof s.bool === 'object'
      && Object.keys(s.bool).length > 0) {
    c.bool = s.bool
  }
  // hideExclusive: kept only when false AND the type is roles
  // (constraint 11 / pass 5 finding 3). The default is on; the
  // fetch consults it only for the roles type. The rule lives
  // here alone — never in buildUrlState — so both consumers of
  // the canonical form cannot drift.
  if (s.hideExclusive === false && s.type === 'roles') {
    c.hideExclusive = false
  }
  if (s.sort != null) c.sort = s.sort
  if (typeof s.page === 'number' && s.page !== 1) {
    c.page = s.page
  }
  return c
}

// buildUrlState is the writer's raw object build: every field
// present with its live value (page: 1, q: '', filters: {},
// hideExclusive the raw boolean). It must NOT canonicalize
// internally (pass 5 finding 1): writeUrl routes the build
// through canonicalize afterward, and if the build started
// omitting defaults the writer's second pass would be a no-op
// and "one canonicalize, both sides" would degrade from an
// enforced invariant to a comment.
export function buildUrlState(q: QueryFields): UrlState {
  return {
    type: q.type,
    q: q.debouncedSearch,
    notq: q.debouncedNotTerm,
    filters: q.listFilters,
    bool: q.boolFilters,
    hideExclusive: q.hideExclusive,
    sort: q.sortField == null
      ? undefined
      : (q.sortDir === 'desc' ? '-' : '') + q.sortField,
    page: q.currentPage
  }
}

// Parse a JSON object param (filters / bool). Returns undefined
// on anything malformed — a malformed value drops just that
// param, never the whole parse. A corrupt param on a *shared*
// link is worth hearing about, unlike corrupt localStorage which
// is local, so it warns.
function parseJsonParam<T>(
  raw: string | null,
  valid: (v: any) => v is T,
  name: string
): T | undefined {
  if (raw == null) return undefined
  let parsed: any
  try {
    // URLSearchParams.get singly-decodes; when the writer's
    // double-encoded form survives the browser's hash handling
    // (Safari decodes location.hash once more), one decode
    // pass may still see %7B…%7D — decode before JSON.parse.
    const decoded = raw.startsWith('%')
      ? decodeURIComponent(raw) : raw
    parsed = JSON.parse(decoded)
  } catch {
    console.warn(`app-state-url: malformed ${name} param`)
    return undefined
  }
  if (valid(parsed)) return parsed
  console.warn(`app-state-url: malformed ${name} param`)
  return undefined
}

const isFilters = (v: any): v is Record<string, string[]> =>
  typeof v === 'object' && v !== null && !Array.isArray(v)
  && Object.values(v).every(
    x => Array.isArray(x)
      && x.every(s => typeof s === 'string'))

const isBool = (v: any): v is Record<string, 'yes' | 'no'> =>
  typeof v === 'object' && v !== null && !Array.isArray(v)
  && Object.values(v).every(
    x => x === 'yes' || x === 'no')

// parseHash: read the live hash. Returns null for any hash that
// is not ours (including '' and '#') — "not ours, no seed".
// Tolerate a missing '?'. Unknown params are ignored (a future
// reader never crashes on an old link; a new link on an old
// build just seeds what it knows). view is deliberately not in
// the format: both consumers derive the view mode from the
// type's category, so old view=… links fall to the
// unknown-param tolerance.
export function parseHash(): UrlState | null {
  const hash = window.location.hash
  if (!hash.startsWith(PREFIX)) return null
  const q = hash.slice(PREFIX.length)
  const query = q.startsWith('?') ? q.slice(1) : q
  const params = new URLSearchParams(query)
  const s: UrlState = {}
  const t = params.get('type')
  if (typeof t === 'string' && t !== '') s.type = t
  const qv = params.get('q')
  if (typeof qv === 'string' && qv !== '') s.q = qv
  const nv = params.get('notq')
  if (typeof nv === 'string' && nv !== '') s.notq = nv
  const f = parseJsonParam(params.get('filters'), isFilters,
                           'filters')
  if (f) s.filters = f
  const b = parseJsonParam(params.get('bool'), isBool, 'bool')
  if (b) s.bool = b
  const h = params.get('hide-exclusive')
  if (h === 'true') s.hideExclusive = true
  else if (h === 'false') s.hideExclusive = false
  const so = params.get('sort')
  if (so != null && /^-?[A-Za-z0-9_-]+$/.test(so)) s.sort = so
  const p = params.get('page')
  if (p != null && /^\d+$/.test(p) && parseInt(p, 10) >= 1) {
    s.page = parseInt(p, 10)
  }
  return s
}

// Deep equality for two UrlStates (of any shape — the callers
// pass canonical forms). Key order, array order, and empty-array
// shape are canonicalized away by canonicalize; this compares
// the canonical results. The type compare stays case-sensitive
// for both callers: the writer's no-op compare leans on it, and
// the listener handles case-variant arrivals by resolving the
// parsed type through target.name before the gate (a different
// compare), never by loosening this one.
export function urlStatesEqual(a: UrlState, b: UrlState): boolean {
  const keys = new Set(
    [...Object.keys(a), ...Object.keys(b)])
  for (const k of keys) {
    const av = (a as any)[k]
    const bv = (b as any)[k]
    if (av === bv) continue
    if (typeof av === 'object' && typeof bv === 'object'
        && av !== null && bv !== null) {
      const ak = Object.keys(av)
      const bk = Object.keys(bv)
      if (ak.length !== bk.length) return false
      for (const kk of ak) {
        if (!Object.prototype.hasOwnProperty.call(bv, kk)) {
          return false
        }
        if (JSON.stringify(av[kk])
            !== JSON.stringify(bv[kk])) return false
      }
      continue
    }
    return false
  }
  return true
}

// writeUrl: the only hash writer. pushState on committed
// changes only (search writes on the debounce commit, not per
// keystroke). The no-op compare is canonical-to-canonical,
// never raw strings: browsers disagree on percent-decoding
// location.hash and URLSearchParams encoding need not
// round-trip byte-for-byte through it (filters JSON, spaces) —
// a raw compare would push a duplicate entry on the first login
// of every shared link. It also absorbs a hand-edited but
// semantically equal hash (an explicit page=1) — no-op, no
// duplicate entry. If the current hash is not ours
// (parseHash null), always write.
export function writeUrl(s: UrlState): void {
  const c = canonicalize(s)
  const params = new URLSearchParams()
  if (c.type != null) params.set('type', c.type)
  if (c.q != null) params.set('q', c.q)
  if (c.notq != null) params.set('notq', c.notq)
  if (c.filters != null) {
    params.set('filters', JSON.stringify(c.filters))
  }
  if (c.bool != null) {
    params.set('bool', JSON.stringify(c.bool))
  }
  if (c.hideExclusive === false) {
    params.set('hide-exclusive', 'false')
  }
  if (c.sort != null) params.set('sort', c.sort)
  if (c.page != null) params.set('page', String(c.page))
  let qs = params.toString()
  if (qs !== '') qs = '?' + qs
  const next = PREFIX + qs
  const live = parseHash()
  if (live != null
      && urlStatesEqual(canonicalize(live), c)) {
    return  // URL already equals state — no duplicate entry
  }
  try {
    history.pushState(null, '', next)
  } catch {
    // pushState can throw (security errors in exotic embeds);
    // URL state is best-effort, never a crash on navigation.
  }
}
