import { useEffect, useRef, useState } from 'react'
import { apiFetch, setTokens, clearTokens, getAccessToken,
         onAuthFailure } from './api'
import StarRating from './StarRating'
import CheckboxList from './CheckboxList'

// Apply CSS-relevant settings values to the document.
// Currently only dark-mode (boolean → toggle .dark class on body).
// Future fields just add one line each here.
function applyCssVariables(vars: Record<string, unknown>) {
  const body = document.body
  if (vars['dark-mode']) {
    body.classList.add('dark')
  } else {
    body.classList.remove('dark')
  }
}

// Fetch CSS variables from the backend and apply them.
async function fetchAndApplyCssVariables() {
  try {
    const res = await apiFetch('/api/css-variables')
    if (res.ok) {
      const json = await res.json()
      if (json?.result) {
        applyCssVariables(json.result)
      }
    }
  } catch {
    /* non-fatal — default light theme stays */
  }
}

// Extract the backend's error message from a failed response, falling back
// to a generic message if the body can't be parsed.
async function errorMessage(res: Response, fallback: string): Promise<string> {
  try {
    const body = await res.json()
    if (body?.error) return body.error
  } catch {
    /* keep fallback */
  }
  return fallback
}

interface Field {
  label: string
  'widget': string
  path?: boolean
  table?: string
  precision?: number
  'read-only'?: boolean
  sortable?: boolean
  searchable?: boolean
}

interface ListResponse {
  status: string
  result: {
    'type-key': string
    'list-form': Record<string, Field>
    // Rollups emit only list-form (the compiler forbids add/update
    // forms on :rollup types), so both are optional on the wire.
    'add-form'?: Record<string, Field>
    'update-form'?: Record<string, Field>
    records: any[]
    total?: number
    // Plan 09: echoed user-facing effective sort. Null when the
    // request sent none and the type has no ranking default (base
    // types). Never the id tiebreaker.
    sort?: { field: string, dir: 'asc' | 'desc' } | null
    'allowed-values'?: Record<string, string[]>
    'type-roles'?: string[]
    create?: boolean
    delete?: boolean
    update?: boolean
  }
}

interface TypeInfo {
  name: string
  category: 'user' | 'system' | 'settings'
}

type ViewMode = 'app' | 'admin' | 'settings'

// Page size for list pagination. Matches the frozen list contract:
// be-list and /api/list both default limit to 20. The FE always
// sends limit=, so the server default is unused, but the numbers
// stay aligned.
const PAGE_SIZE = 20

// Format a number according to the field's :precision UI hint.
// Returns the original value untouched if it's not a number or
// no precision is specified.
function formatNumber(
  val: any, field: Field
): string {
  if (typeof val !== 'number' || field.precision == null)
    return val != null ? String(val) : ''
  return val.toFixed(field.precision)
}

// --- Widget dispatch ---
//
// Each function handles one rendering context (list cell vs form).
// Dispatch is on `widget` only.

// Plain-text projection of a cell value: arrays join with ", ",
// numbers get :precision formatting. Feeds the default cell
// rendering, the code widget, and the one-line clamp tooltip.
function cellText(val: any, field: Field): string {
  return Array.isArray(val) ? val.join(', ')
    : formatNumber(val, field)
}

function renderCellValue(
  val: any, field: Field
): React.ReactNode {
  const widget = field['widget'] || ''
  const text = cellText(val, field)

  if (widget === 'code') {
    return (
      <pre style={{
        margin: 0,
        maxHeight: '4.5em',
        overflow: 'hidden',
        whiteSpace: 'pre-wrap',
        fontSize: '0.85em',
        fontFamily: 'monospace'
      }}>
        {text}
      </pre>
    )
  }

  if (widget === 'image-list') {
    const paths: string[] = Array.isArray(val) ? val : []
    if (paths.length === 0) return text || ''
    // List cells show only the first path alphabetically (the
    // cover shot); the lightbox still pages through all of them.
    const sorted =
      [...paths].sort((a, b) => a.localeCompare(b))
    return (
      <ThumbnailGrid
        type={field.table || ''} paths={sorted} size={40} max={1}
      />
    )
  }

  if (widget === 'image') {
    const path = typeof val === 'string' ? val : ''
    if (!path) return text || ''
    return (
      <ThumbnailGrid
        type={field.table || ''} paths={[path]} size={40}
      />
    )
  }

  if (widget === 'stars') {
    const num = typeof val === 'number' ? val
      : val ? Number(val) : null
    if (num == null || isNaN(num)) return ''
    return <StarRating value={num} />
  }

  if (widget === 'checkbox') {
    const isTrue = val === true || val === 'true'
    return isTrue
      ? <span style={{ fontSize: '1.1em' }}>&#10003;</span>
      : <span />
  }

  return text
}

function ImagePreview({
  type, path
}: {
  type: string
  path: string
}) {
  const [open, setOpen] = useState(false)
  const name = path.split('/').pop() || path
  const src = fileUrl(type, path)

  return (
    <>
      <img
        src={src}
        alt={name}
        onClick={() => setOpen(true)}
        onError={e => {
          (e.target as HTMLImageElement).style
            .display = 'none'
        }}
        style={{
          width: '60px',
          height: '60px',
          objectFit: 'cover',
          flexShrink: 0,
          cursor: 'pointer'
        }}
      />
      {open && (
        <ImageModal
          images={[{ src, filename: name }]}
          index={0}
          onClose={() => setOpen(false)}
          onNavigate={() => {}}
        />
      )}
    </>
  )
}

function renderFormField(
  field: Field, value: any,
  onChange: (v: string) => void
): React.ReactNode {
  const widget = field['widget'] || ''

  if (widget === 'stars') {
    const num = value ? Number(value) : null
    return (
      <StarRating
        value={num}
        interactive={true}
        onChange={onChange}
      />
    )
  }

  return (
    <input
      type="text"
      value={value || ''}
      onChange={e => onChange(e.target.value)}
    />
  )
}

// --- Image helpers ---

function fileUrl(type: string, path: string): string {
  const token = getAccessToken()
  const base = `/api/file?type=${encodeURIComponent(type)}`
    + `&path=${encodeURIComponent(path)}`
  return token ? `${base}&token=${encodeURIComponent(token)}` : base
}

function ImageModal({
  images, index, onClose, onNavigate
}: {
  images: { src: string; filename: string }[]
  index: number
  onClose: () => void
  onNavigate: (index: number) => void
}) {
  const current = images[index]
  const hasPrev = images.length > 1
  const hasNext = images.length > 1

  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      if (e.key === 'ArrowLeft')
        onNavigate(
          (index - 1 + images.length) % images.length
        )
      else if (e.key === 'ArrowRight')
        onNavigate((index + 1) % images.length)
      else if (e.key === 'Escape')
        onClose()
    }
    window.addEventListener('keydown', handler)
    return () => window.removeEventListener(
      'keydown', handler
    )
  }, [index, images.length, onNavigate, onClose])

  return (
    <div
      onClick={onClose}
      style={{
        position: 'fixed',
        inset: 0,
        background: 'var(--overlay-bg)',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        justifyContent: 'center',
        zIndex: 1000,
        cursor: 'pointer'
      }}
    >
      <div style={{
        display: 'flex',
        gap: '1rem',
        marginBottom: '1rem',
        alignItems: 'center',
        cursor: 'default'
      }}>
        <button
          type="button"
          onClick={e => {
            e.stopPropagation()
            onNavigate(
              (index - 1 + images.length) % images.length
            )
          }}
          disabled={!hasPrev}
        >
          ‹ Prev
        </button>
        <span style={{ color: 'var(--muted)', fontSize: '0.85rem' }}>
          {index + 1} / {images.length}
        </span>
        <button
          type="button"
          onClick={e => {
            e.stopPropagation()
            onNavigate((index + 1) % images.length)
          }}
          disabled={!hasNext}
        >
          Next ›
        </button>
        <span style={{ width: '1rem' }} />
        <a
          href={current.src}
          download={current.filename}
          onClick={e => e.stopPropagation()}
          style={modalLinkStyle}
        >
          Download
        </a>
        <a
          href={current.src}
          target="_blank"
          rel="noopener noreferrer"
          onClick={e => e.stopPropagation()}
          style={modalLinkStyle}
        >
          Open in new tab
        </a>
        <button type="button" onClick={onClose}>Close</button>
      </div>
      <img
        src={current.src}
        alt={current.filename}
        onClick={e => e.stopPropagation()}
        style={{
          maxWidth: '90vw',
          maxHeight: '80vh',
          objectFit: 'contain',
          cursor: 'default'
        }}
      />
    </div>
  )
}

const modalLinkStyle: React.CSSProperties = {
  color: 'var(--link)',
  textDecoration: 'underline',
  cursor: 'pointer'
}

function ThumbnailGrid({
  type, paths, size, max
}: {
  type: string
  paths: string[]
  size?: number
  // Render at most `max` thumbnails; the lightbox still pages
  // through the full `paths` list (indices align because the
  // shown thumbnails are a prefix slice).
  max?: number
}) {
  const [modalIndex, setModalIndex] = useState<number | null>(
    null
  )
  const thumbSize = size || 80

  if (!paths || paths.length === 0)
    return <span style={{ color: 'var(--muted-2)' }}>—</span>

  const modalImages = paths.map(p => ({
    src: fileUrl(type, p),
    filename: p.split('/').pop() || p
  }))

  const shown = max != null ? paths.slice(0, max) : paths

  return (
    <>
      <div style={{
        display: 'flex',
        flexWrap: 'wrap',
        gap: '0.5rem'
      }}>
        {shown.map((p, i) => {
          const name = p.split('/').pop() || p
          return (
            <div
              key={p}
              onClick={() => setModalIndex(i)}
              style={{
                cursor: 'pointer',
                textAlign: 'center',
                width: `${thumbSize}px`
              }}
            >
              <img
                src={fileUrl(type, p)}
                alt={name}
                style={{
                  width: `${thumbSize}px`,
                  height: `${thumbSize}px`,
                  objectFit: 'cover',
                  display: 'block'
                }}
              />
              <div style={{
                fontSize: '0.7rem',
                overflow: 'hidden',
                textOverflow: 'ellipsis',
                whiteSpace: 'nowrap'
              }}>
                {name}
              </div>
            </div>
          )
        })}
      </div>
      {modalIndex !== null && (
        <ImageModal
          images={modalImages}
          index={modalIndex}
          onClose={() => setModalIndex(null)}
          onNavigate={setModalIndex}
        />
      )}
    </>
  )
}

// --- Read-only field rendering ---

function renderReadOnlyField(
  field: Field,
  value: any
): React.ReactNode {
  const widget = field['widget'] || ''

  if (widget === 'image-list') {
    const paths: string[] = Array.isArray(value) ? value : []
    return (
      <ThumbnailGrid
        type={field.table || ''} paths={paths}
      />
    )
  }

  if (widget === 'image') {
    const path = typeof value === 'string' ? value : ''
    if (!path) {
      return <div style={{ color: 'var(--muted-2)' }}>—</div>
    }
    return (
      <ImagePreview
        type={field.table || ''} path={path}
      />
    )
  }

  if (widget === 'stars') {
    const num = typeof value === 'number' ? value
      : value ? Number(value) : null
    if (num == null || isNaN(num))
      return <div style={{ color: 'var(--muted-2)' }}>—</div>
    return <StarRating value={num} />
  }

  if (widget === 'checkbox') {
    const isTrue = value === true || value === 'true'
    return (
      <div style={{ padding: '0.3rem 0' }}>
        {isTrue ? '\u2713' : ''}
      </div>
    )
  }

  // Default read-only: plain text display
  const text = Array.isArray(value) ? value.join(', ')
    : formatNumber(value, field)
  return (
    <div style={{
      padding: '0.3rem 0',
      color: 'var(--label)',
      minHeight: '1.2em'
    }}>
      {text || '—'}
    </div>
  )
}

function App() {
  const [data, setData] = useState<ListResponse | null>(null)
  const [types, setTypes] = useState<TypeInfo[]>([])
  // True once the login-time /api/types fetch has settled (even to
  // an empty list). Distinguishes "still resolving" from "this user
  // can see no types at all" for the header label.
  const [typesLoaded, setTypesLoaded] = useState(false)
  const [viewMode, setViewMode] = useState<ViewMode>('app')
  const [type, setType] = useState('__init__')
  const [showAddForm, setShowAddForm] = useState(false)
  const [formValues, setFormValues] = useState<Record<string, any>>({})
  // Confirmation box for :widget :password fields. Keyed by field
  // name so multi-password forms stay independent; cleared with the
  // rest of the form state.
  const [passwordConfirm, setPasswordConfirm] =
    useState<Record<string, string>>({})
  const [selectedIds, setSelectedIds] = useState<string[]>([])
  const [editRecord, setEditRecord] = useState<any>(null)
  const [listError, setListError] = useState<string | null>(null)
  const [title, setTitle] = useState('Data UI')
  const [pendingActions, setPendingActions] = useState<Set<string>>(new Set())
  const [elapsed, setElapsed] = useState<number | null>(null)
  const runningStartRef = useRef<number | null>(null)
  const [sortField, setSortField] = useState<string | null>(null)
  const [sortDir, setSortDir] = useState<'asc' | 'desc'>('asc')
  // Plan 09: the type's default ranking column (backend-echoed), or
  // null. Set only from responses to requests that sent no sort;
  // cleared on type change alongside the sort state.
  const defaultSortRef = useRef<string | null>(null)
  const [searchTerm, setSearchTerm] = useState('')
  const [debouncedSearch, setDebouncedSearch] = useState('')
  const [currentPage, setCurrentPage] = useState(1)
  const searchTimer = useRef<ReturnType<typeof setTimeout> | null>(null)
  // Chip filters (clickable list-value chips): fieldKey -> selected
  // values. Within a field, values union (has-any, sent as one :in
  // row); across fields, rows AND. Empty arrays are omitted from
  // the request (in () is invalid SQL / a 400).
  const [listFilters, setListFilters] = useState<
    Record<string, string[]>
  >({})
  // Phase 1 (hide exclusive roles): checked = append one
  // not-like row to /api/list filters when the listed type is
  // roles. Default on — the curated default view excludes the
  // per-user noise roles (machine-written :exclusive suffix).
  const [hideExclusive, setHideExclusive] = useState(true)
  // Phase 2 (negative search): the "Not…" term, complement of the
  // search box over the :searchable fields. Debounced exactly like
  // searchTerm; blank sends no rows (requests stay byte-identical).
  const [notTerm, setNotTerm] = useState('')
  const [debouncedNotTerm, setDebouncedNotTerm] = useState('')
  const notTimer = useRef<ReturnType<typeof setTimeout> | null>(null)

  // Auth state
  const [username, setUsername] = useState('')
  const [password, setPassword] = useState('')
  const [loginError, setLoginError] = useState('')
  const [loggedIn, setLoggedIn] = useState(false)
  const [loggedInUser, setLoggedInUser] = useState('')
  const [guestAllowed, setGuestAllowed] = useState(false)

  // When token refresh fails, force return to login screen
  useEffect(() => {
    onAuthFailure(() => {
      setLoggedIn(false)
      setLoggedInUser('')
      resetSessionState()
      document.body.classList.remove('dark')
    })
  }, [])

  // Shared post-login path: the Login button, "Continue as guest",
  // and auto-guest all land here.
  const completeLogin = (user: string, access: string, refresh: string) => {
    setTokens(access, refresh)
    setLoggedIn(true)
    setLoggedInUser(user)
    setUsername('')
    setPassword('')
    fetchAndApplyCssVariables()
  }

  const loginAsGuest = async (): Promise<boolean> => {
    try {
      const res = await fetch('/api/login', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ username: 'guest', password: '' })
      })
      const json = await res.json()
      if (json.status === 'success' && json.result) {
        const access = json.result['access-token']
        const refresh = json.result['refresh-token']
        if (access && refresh) {
          completeLogin('guest', access, refresh)
          return true
        }
      }
    } catch {
      /* fall through — leave the login form up */
    }
    return false
  }

  // Fetch the app title + guest flag on mount (pre-login, no auth
  // required). When the model allows guest login, transparently sign
  // in as guest unless this tab opted out via the skip flag (set on
  // guest logout, so the login form stays reachable for an admin).
  useEffect(() => {
    let cancelled = false
    fetch('/api/public-info')
      .then(r => r.ok ? r.json() : null)
      .then(async json => {
        if (cancelled || !json?.result) return
        if (json.result['title']) {
          setTitle(String(json.result['title']))
        }
        // Strict true: plist-to-json emits [] for a bare nil, which
        // must not count as allowed.
        if (json.result['guest-allowed'] === true) {
          setGuestAllowed(true)
          // FR-6: auto-guest only when guest-allowed === true AND
          // guest-auto !== false (strict, same [] pitfall).
          // "Continue as guest" stays gated on guest-allowed alone.
          const guestAuto = json.result['guest-auto']
          if (guestAuto !== false
              && !sessionStorage.getItem('data-ui-skip-auto-guest')) {
            await loginAsGuest()
          }
        }
      })
      .catch(() => {})
    return () => { cancelled = true }
  }, [])

  // Keep the browser title bar in sync with the app title (the
  // model's name), not the static "Data UI" from index.html.
  useEffect(() => {
    document.title = title
  }, [title])

  const handleContinueAsGuest = async () => {
    sessionStorage.removeItem('data-ui-skip-auto-guest')
    await loginAsGuest()
  }

  const isEditMode = !!editRecord

  // Total comes from the last successful fetch envelope; do not
  // derive it from records.length (wrong on any page past 1).
  const totalRecords = data?.result?.total ?? 0
  const totalPages = Math.max(1, Math.ceil(totalRecords / PAGE_SIZE))

  const userTypes = types.filter(t => t.category === 'user')
  const systemTypes = types.filter(t => t.category === 'system')
  const settingsTypes = types.filter(t => t.category === 'settings')

  const activeTypes = viewMode === 'admin' ? systemTypes
    : viewMode === 'settings' ? settingsTypes
    : userTypes

  // Header shows the selected type name, except before any type is
  // selected: blank while the type list is still resolving on login,
  // "No Access" once it has resolved and the user can see no types
  // (e.g. guest). Never render the __init__ sentinel.
  const headerLabel = type !== '__init__' ? type
    : typesLoaded ? 'No Access'
    : ''

  const fetchList = async (): Promise<ListResponse | null> => {
    setListError(null)
    const offset = (currentPage - 1) * PAGE_SIZE
    let url = `/api/list?type=${type}&limit=${PAGE_SIZE}&offset=${offset}`
    if (sortField) {
      url += `&sort=${sortField}:${sortDir}`
    }
    if (debouncedSearch) {
      url += `&search=${encodeURIComponent(debouncedSearch)}`
    }
    // Chip filters: one [table, column, "in", values] row per field
    // with a non-empty selection. The table comes from list-form
    // field meta (field.table), NOT the state key — chores
    // :completed-by must send "users", and the state key would 404
    // in parse-type. The column is the M2M display column ("name"),
    // which is what parse-field resolves in the compiled model.
    {
      const form = data?.result?.['list-form'] || {}
      const rows = Object.entries(listFilters)
        .filter(([, values]) => values.length > 0)
        .map(([fieldKey, values]) => [
          form[fieldKey]?.table || fieldKey, 'name', 'in', values
        ])
      // Phase 1: hide the per-user exclusive roles (name suffix
      // :exclusive, trigger-enforced — same signal the backend's
      // exclusive-role-p reads). Case-sensitive not-like is exact
      // here: the suffix is machine-written lowercase.
      if (type === 'roles' && hideExclusive) {
        rows.push(['roles', 'name', 'not-like', '%:exclusive'])
      }
      // Phase 2: one not-ilike row per searchable field — the
      // complement of search's OR-ILIKE group (rows AND together).
      // Same list-form meta guard as the chips: field.table is not
      // consulted (searchable fields are the type's own base
      // columns), and the resetListQuery clears on type change so
      // the stale-form window sends nothing.
      if (debouncedNotTerm) {
        for (const [fieldKey, f] of Object.entries(form)) {
          if (f.searchable === true) {
            rows.push([type, fieldKey, 'not-ilike',
                       `%${debouncedNotTerm}%`])
          }
        }
      }
      if (rows.length > 0) {
        url += `&filters=${encodeURIComponent(JSON.stringify(rows))}`
      }
    }
    try {
      const res = await apiFetch(url)
      if (!res.ok) {
        throw new Error(`Request failed (${res.status})`)
      }
      const json: ListResponse = await res.json()
      setData(json)
      // Plan 09: paint sortField/sortDir from the echoed effective
      // sort on every successful fetch (including null — base types
      // with no sort echo unsorted). The backend owns the ranking
      // default (first sortable measure DESC on rollups), so the
      // header indicator always matches the actual order.
      const echoed = json?.result?.sort ?? null
      if (echoed && typeof echoed.field === 'string') {
        setSortField(echoed.field)
        setSortDir(echoed.dir === 'desc' ? 'desc' : 'asc')
      } else {
        setSortField(null)
        setSortDir('asc')
      }
      // Cache the type's default ranking column, but only when this
      // request sent no sort: an echo from a sorted request is the
      // echoed request, not the default, and caching it would attach
      // the toggle-only exception to the wrong column.
      if (!sortField) {
        defaultSortRef.current = echoed && typeof echoed.field === 'string'
          ? echoed.field : null
      }
      // Clamp the page if the result set shrank under us (e.g.
      // deletions). total lives at json.result.total (the REST
      // layer wraps every payload in a result envelope); using
      // records.length would report the page size as the total.
      const total = json?.result?.total ?? 0
      const maxPage = Math.max(1, Math.ceil(total / PAGE_SIZE))
      if (currentPage > maxPage) setCurrentPage(maxPage)
      return json
    } catch (err: any) {
      setData(null)
      setListError(err.message || 'Failed to load data')
      return null
    }
  }

  // fetchList closes over page/sort/search; keep a stable reference
  // for the running-status poll interval so it always calls the
  // current fetchList without restarting the timer.
  const fetchListRef = useRef(fetchList)
  fetchListRef.current = fetchList

  // Reset all list-query state (sort, search, page, selection) when
  // the list identity changes. Reused by changeType, switchViewMode,
  // and returnToLanding so the new type's first fetch lands on page 1
  // with clean params (no stale sort/search carried over).
  const resetListQuery = () => {
    setSortField(null)
    setSortDir('asc')
    defaultSortRef.current = null
    setSearchTerm('')
    setDebouncedSearch('')
    setNotTerm('')
    setDebouncedNotTerm('')
    setListFilters({})
    if (searchTimer.current) clearTimeout(searchTimer.current)
    if (notTimer.current) clearTimeout(notTimer.current)
    setCurrentPage(1)
    setSelectedIds([])
  }

  // Reset all session-scoped UI state so the next login starts
  // clean. Without this, the next session inherits the previous
  // one's view mode (admin tabs under a regular user), type,
  // page, and half-open forms.
  const resetSessionState = () => {
    setViewMode('app')
    setType('__init__')
    setTypes([])
    setTypesLoaded(false)
    setData(null)
    setShowAddForm(false)
    setEditRecord(null)
    setFormValues({})
    setPasswordConfirm({})
    setListError(null)
    resetListQuery()
  }

  const changeType = (newType: string) => {
    setType(newType)
    setShowAddForm(false)
    setEditRecord(null)
    setFormValues({})
    resetListQuery()
  }

  const handleListSearch = (query: string) => {
    setSearchTerm(query)
    if (searchTimer.current) clearTimeout(searchTimer.current)
    searchTimer.current = setTimeout(() => {
      setDebouncedSearch(query.trim())
      setCurrentPage(1)
      setSelectedIds([])
    }, 300)
  }

  // Phase 2: "Not…" input handler — a copy of handleListSearch on
  // the not-term state. Same 300ms debounce, trim, page 1, clear
  // selection contract (the result set changes).
  const handleListNot = (query: string) => {
    setNotTerm(query)
    if (notTimer.current) clearTimeout(notTimer.current)
    notTimer.current = setTimeout(() => {
      setDebouncedNotTerm(query.trim())
      setCurrentPage(1)
      setSelectedIds([])
    }, 300)
  }

  // Toggle one chip value in the field's selection (union within
  // the field; refetch flows through the useEffect dep on
  // listFilters). Any toggle restarts the result set: page 1,
  // selection cleared — same contract as search and sort changes.
  const toggleChipFilter = (fieldKey: string, value: string) => {
    setListFilters(prev => {
      const cur = prev[fieldKey] || []
      const next = cur.includes(value)
        ? cur.filter(v => v !== value)
        : [...cur, value]
      return { ...prev, [fieldKey]: next }
    })
    setCurrentPage(1)
    setSelectedIds([])
  }

  const switchViewMode = (mode: ViewMode) => {
    setViewMode(mode)
    setShowAddForm(false)
    setEditRecord(null)
    setFormValues({})
    resetListQuery()
    // Select first type in the new mode
    if (mode === 'app') {
      const first = userTypes[0]
      if (first) setType(first.name)
    } else if (mode === 'admin') {
      const first = systemTypes[0]
      if (first) setType(first.name)
    } else if (mode === 'settings') {
      const first = settingsTypes[0]
      if (first) setType(first.name)
    }
  }

  const openEditForm = async (record: any) => {
    // Fetch the full record from /api/item with update-form so that
    // fields absent from list-form (e.g. deploy-status, my-rating)
    // are populated correctly in the edit form.
    let fullRecord = record
    try {
      const res = await apiFetch(
        `/api/item?type=${type}&id=${record.id}&form=update-form`
      )
      if (res.ok) {
        const json = await res.json()
        fullRecord = json?.result?.record || record
      }
    } catch {
      // Fall back to list record on error
    }
    setEditRecord(fullRecord)
    // Filter out the current user's own exclusive role from the
    // roles — it's injected automatically by the backend and should
    // never appear as a manual checkbox or be sent back.
    const myExclusive = `${loggedInUser}:exclusive`
    const cleanRoles = (fullRecord.roles || []).filter(
      (r: string) => r !== myExclusive
    )
    setFormValues({ ...fullRecord, roles: cleanRoles })
    setShowAddForm(false)
  }

  const closeForm = () => {
    setEditRecord(null)
    setShowAddForm(false)
    setFormValues({})
    setPasswordConfirm({})
  }

  // Return to the landing page (used by settings Submit/Cancel).
  const returnToLanding = () => {
    closeForm()
    fetchAndApplyCssVariables()
    apiFetch('/api/info')
      .then(r => r.json())
      .then(info => {
        const t = info.result?.['title']
        if (t) setTitle(String(t))
        const lp = info.result?.['landing-page']
        if (lp) {
          setViewMode('app')
          // Reset query state in the same batch as the type change
          // so one fetch lands on page 1 of the landing type.
          resetListQuery()
          setType(String(lp))
        }
      })
      .catch(() => {
        // Fallback: first user type
        setViewMode('app')
        resetListQuery()
        const first = userTypes[0]
        if (first) setType(first.name)
      })
  }

  // Cancel button handler: returns to landing in settings mode,
  // otherwise just closes the form.
  const handleCancel = () => {
    if (viewMode === 'settings') {
      returnToLanding()
    } else {
      closeForm()
    }
  }

  const handleLogin = async () => {
    setLoginError('')
    try {
      const res = await fetch('/api/login', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ username, password })
      })
      const json = await res.json()
      if (json.status === 'success' && json.result) {
        const access = json.result['access-token']
        const refresh = json.result['refresh-token']
        if (access && refresh) {
          completeLogin(username, access, refresh)
        } else {
          setLoginError('Invalid response from server')
        }
      } else {
        setLoginError(json.error || 'Login failed')
      }
    } catch (e) {
      setLoginError('Network error')
    }
  }

  const handleLogout = () => {
    // Leaving guest mode in this tab: opt out of auto-guest until the
    // tab closes, so the login form stays up for a real sign-in.
    if (loggedInUser === 'guest') {
      sessionStorage.setItem('data-ui-skip-auto-guest', '1')
    }
    clearTokens()
    setLoggedIn(false)
    setLoggedInUser('')
    resetSessionState()
    document.body.classList.remove('dark')
  }

  const handleAction = async (fieldKey: string) => {
    if (!editRecord) return
    const id = editRecord.id
    setPendingActions(prev => new Set(prev).add(fieldKey))
    try {
      const res = await apiFetch('/api/actions', {
        method: 'POST',
        body: JSON.stringify({
          type,
          id,
          field: fieldKey
        })
      })
      if (!res.ok) {
        alert(await errorMessage(res, 'Action failed'))
      }
      // Reload the list with the current page/sort/search, then
      // re-open the edit form by id: an action that changes a sort
      // key can move the row off the current page, so the row must
      // not be looked up in the current page's records.
      await fetchList()
      openEditForm({ id })
    } catch {
      alert('Network error during action')
    } finally {
      setPendingActions(prev => {
        const next = new Set(prev)
        next.delete(fieldKey)
        return next
      })
    }
  }

  const submitForm = async () => {
    // Find file field if present. Rollup types emit no add/update
    // forms (read-only), so the form lookup is guarded.
    const formDef = isEditMode
      ? data!.result['update-form'] || {}
      : data!.result['add-form'] || {}

    // Password confirmation gate: every :widget :password field on
    // the form must match its confirmation box before anything is
    // sent (covers both the JSON and file-upload paths below).
    for (const f of Object.keys(formDef)) {
      if (formDef[f]['widget'] !== 'password') continue
      const pw = formValues[f] || ''
      const confirm = passwordConfirm[f] || ''
      if (pw === '' && confirm === '') continue
      if (pw !== confirm) {
        alert(`Password entries for "${formDef[f].label}" do not match`)
        return
      }
    }

    const fileField = Object.keys(formDef).find(f => formDef[f]['widget'] === 'file')
    const fileValue = fileField ? formValues[fileField] : null

    if (fileField && fileValue instanceof File) {
      // Two-phase upload.
      //
      // POST 1: multipart/form-data to /api/upload with all add-form
      // fields, including the file field. The browser sets the
      // multipart boundary automatically (see apiFetch).
      const formData = new FormData()
      formData.append('type', type)
      for (const f of Object.keys(formDef)) {
        const value = formValues[f]
        if (value === undefined || value === null) continue
        if (f === fileField) {
          formData.append(f, value)
        } else if (Array.isArray(value)) {
          value.forEach(v => formData.append(f, v))
        } else {
          formData.append(f, value)
        }
      }
      if (formValues.roles) {
        // roles may be array or single value; send as-is
        const roles = formValues.roles
        if (Array.isArray(roles)) {
          roles.forEach(r => formData.append('roles', r))
        } else {
          formData.append('roles', roles)
        }
      }

      const uploadRes = await apiFetch('/api/upload', {
        method: 'POST',
        body: formData
      })

      if (!uploadRes.ok) {
        let detail = `${uploadRes.status} ${uploadRes.statusText}`.trim()
        const errText = await uploadRes.text().catch(() => '')
        if (errText) {
          try {
            const errJson = JSON.parse(errText)
            detail =
              errJson?.result?.message ||
              errJson?.message ||
              errJson?.error ||
              detail
          } catch {
            // Body wasn't JSON; use the raw text.
            detail = errText
          }
        }
        alert(`File upload failed: ${detail}`)
        return
      }

      // Extract the file-token returned by the upload.
      const uploadJson = await uploadRes.json()
      const fileToken =
        uploadJson?.['file-token'] ?? uploadJson?.result?.['file-token']

      if (!fileToken) {
        alert('File upload did not return a file-token')
        return
      }

      // POST 2: JSON to /api/insert with the add-form fields plus a
      // top-level file-token (sibling of type). The file field itself
      // is omitted from the metadata payload.
      const { roles, [fileField]: _omit, ...rest } = formValues
      const filteredRest = Object.fromEntries(
        Object.entries(rest).filter(([, v]) => typeof v !== 'string' || v.trim() !== '')
      )
      const payload: any = { type, 'file-token': fileToken, data: filteredRest }
      if (roles) payload.roles = Array.isArray(roles) ? roles : [roles]

      const res = await apiFetch('/api/insert', {
        method: 'POST',
        body: JSON.stringify(payload)
      })

      if (res.ok) {
        closeForm()
        fetchList()
      } else {
        alert(await errorMessage(res, 'Failed to insert'))
      }
      return
    }

    // Normal (no file) path
    const { roles, ...rest } = formValues
    const filteredRest = Object.fromEntries(
      Object.entries(rest).filter(([, v]) => typeof v !== 'string' || v.trim() !== '')
    )
    const payload: any = { type, data: filteredRest }
    if (roles) payload.roles = roles

    let res
    if (isEditMode) {
      payload.filters = editRecord.id
      res = await apiFetch('/api/update', {
        method: 'POST',
        body: JSON.stringify(payload)
      })
    } else {
      res = await apiFetch('/api/insert', {
        method: 'POST',
        body: JSON.stringify(payload)
      })
    }

    if (res.ok) {
      if (viewMode === 'settings') {
        returnToLanding()
      } else {
        closeForm()
        fetchList()
      }
    } else {
      alert(await errorMessage(res, isEditMode ? 'Failed to update' : 'Failed to insert'))
    }
  }

  const toggleSelect = (id: string) => {
    if (selectedIds.includes(id)) {
      setSelectedIds(selectedIds.filter(x => x !== id))
    } else {
      setSelectedIds([...selectedIds, id])
    }
  }

  const deleteSelected = async () => {
    if (selectedIds.length === 0) return

    const toDelete = records.filter((r: any) => selectedIds.includes(r.id))
    const nameField = listFields.includes('name') ? 'name' : listFields[0]
    const names = toDelete.map((r: any) => r[nameField] || r.id).join(', ')

    if (!confirm(`Delete ${names}?`)) return

    for (const id of selectedIds) {
      const payload = { type, filters: id }
      await apiFetch('/api/delete', {
        method: 'POST',
        body: JSON.stringify(payload)
      })
    }
    setSelectedIds([])
    fetchList()
  }

  useEffect(() => {
    if (!loggedIn) return
    // Fetch landing page + types in parallel on login
    Promise.all([
      apiFetch('/api/info').then(r => r.json()),
      apiFetch('/api/types').then(r => r.json())
    ]).then(([info, typesJson]) => {
      const typeInfos: TypeInfo[] =
        (typesJson.result || []).map((t: any) => ({
          name: typeof t === 'string' ? t : t.name,
          category: typeof t === 'string'
            ? 'user' : t.category
        }))
      setTypes(typeInfos)
      setTypesLoaded(true)
      const t = info.result?.['title']
      if (t) setTitle(String(t))
      const lp = info.result?.['landing-page']
      let resolved: string | null = null
      if (lp) {
        resolved = String(lp)
      } else {
        const firstUser = typeInfos.find(
          t => t.category === 'user'
        )
        if (firstUser) resolved = firstUser.name
        else if (typeInfos.length > 0)
          resolved = typeInfos[0].name
      }
      if (resolved) {
        const target = resolved
        setType(target)
        // Align viewMode with the resolved type's category so a
        // fresh login never inherits the previous session's mode
        // (e.g. logging out of Admin used to leave the next user
        // on the admin tab strip).
        const cat = typeInfos.find(
          t => t.name.toLowerCase() === target.toLowerCase()
        )?.category
        setViewMode(
          cat === 'system' ? 'admin'
          : cat === 'settings' ? 'settings'
          : 'app'
        )
      }
      // else: no types at all; leave __init__ (empty app)
    }).catch(() => {
      setTypes([])
      setTypesLoaded(true)
    })
  }, [loggedIn])

  useEffect(() => {
    if (!loggedIn || type === '__init__') return
    fetchList()
  }, [loggedIn, type, sortField, sortDir, debouncedSearch,
      debouncedNotTerm, currentPage, listFilters, hideExclusive])

  // Clear the row selection whenever the page changes (button
  // navigation or the post-delete clamp inside fetchList), so
  // Delete Selected never targets off-page rows. Same-page filter
  // changes are cleared explicitly in their handlers.
  useEffect(() => { setSelectedIds([]) }, [currentPage])

  // Poll for status updates when a button field's status starts
  // with "running". Re-fetches the record via /api/item and
  // updates editRecord + formValues so the status text updates
  // live in the open form. Prefix match on "running" means
  // intermediate statuses like "running: building image" keep
  // polling automatically.
  //
  // Also tracks elapsed time since running started, with a 1s
  // ticker for the display.
  const editRecordId = editRecord?.id
  const hasRunningStatus = Object.keys(editRecord || {}).some(
    k => k.endsWith('-status') &&
         String(editRecord?.[k]).startsWith('running')
  )

  // Manage the running start timestamp
  useEffect(() => {
    if (hasRunningStatus && runningStartRef.current === null) {
      runningStartRef.current = Date.now()
      setElapsed(0)
    } else if (!hasRunningStatus) {
      runningStartRef.current = null
      setElapsed(null)
    }
  }, [hasRunningStatus])

  // 1s ticker for elapsed display
  useEffect(() => {
    if (!hasRunningStatus) return
    const ticker = setInterval(() => {
      if (runningStartRef.current !== null) {
        setElapsed(Math.floor(
          (Date.now() - runningStartRef.current) / 1000
        ))
      }
    }, 1000)
    return () => clearInterval(ticker)
  }, [hasRunningStatus])

  // 3s poll for record updates
  useEffect(() => {
    if (!editRecordId || !type || !hasRunningStatus) return

    const interval = setInterval(async () => {
      try {
        const res = await apiFetch(
          `/api/item?type=${type}&id=${editRecordId}` +
          `&form=update-form`
        )
        if (!res.ok) return
        const json = await res.json()
        const updated = json?.result?.record
        if (!updated) return
        setEditRecord(updated)
        // Merge all fields from the polled record into formValues.
        // This keeps the form synced with server state so fields
        // written by async workers (e.g. :model from Generate)
        // appear without a page reload.  Safe because buttons are
        // disabled while status is "running".
        setFormValues(prev => {
          const next = { ...prev }
          for (const k of Object.keys(updated)) {
            next[k] = updated[k]
          }
          return next
        })
        // Also refresh the list so list-view statuses stay fresh.
        // Call through the ref so the poll always uses the current
        // page/sort/search (not a stale closure from this render).
        fetchListRef.current()
      } catch {
        // Network errors during polling are non-fatal
      }
    }, 3000)

    return () => clearInterval(interval)
  }, [editRecordId, hasRunningStatus, type])

  // In settings mode, auto-enter edit mode with the single record.
  // Depends on `type` too: when switching to settings, viewMode changes
  // first (firing this effect with stale data), then type changes and
  // fetchList delivers the correct record. Without `type` in the deps,
  // the stale-data call sets isEditMode=true and the guard prevents the
  // correct record from ever loading.
  useEffect(() => {
    if (viewMode !== 'settings') return
    if (!data?.result?.records?.length) return
    if (data.result['type-key'] !== type) return
    if (isEditMode) return
    // Only auto-open the edit form for single-record settings types
    // (e.g. user preferences). Multi-record types like secrets should
    // display as a normal list.
    if (data.result.records.length > 1) return
    openEditForm(data.result.records[0])
  }, [data, viewMode, type])

  if (!loggedIn) {
    return (
      <div style={{ maxWidth: 320, margin: '100px auto', padding: 20 }}>
        <h1>{title}</h1>
        <h2>Login</h2>
        <form onSubmit={e => { e.preventDefault(); handleLogin() }}>
          <input
            type="text"
            name="username"
            autoComplete="username"
            placeholder="Username"
            value={username}
            onChange={e => setUsername(e.target.value)}
            style={{ width: '100%', marginBottom: 8 }}
          />
          <input
            type="password"
            name="password"
            autoComplete="current-password"
            placeholder="Password"
            value={password}
            onChange={e => setPassword(e.target.value)}
            style={{ width: '100%', marginBottom: 12 }}
          />
          <button type="submit" style={{ width: '100%' }}>Login</button>
        </form>
        {guestAllowed && (
          <button
            onClick={handleContinueAsGuest}
            style={{ width: '100%', marginTop: 8 }}
          >
            Continue as guest
          </button>
        )}
        {loginError && <p style={{ color: 'var(--error-bright)' }}>{loginError}</p>}
      </div>
    )
  }

  if (!data || !data.result || Array.isArray(data.result) || !data.result['list-form']) {
    return (
      <div>
        <div style={{ position: 'relative' }}>
          <h1 style={{ margin: 0 }}>
            <a href="#" onClick={(e) => {
              e.preventDefault()
              returnToLanding()
            }} style={{ textDecoration: 'none', color: 'inherit' }}>
              {title}
            </a>
          </h1>
          <div style={{
            position: 'absolute',
            left: '50%',
            top: '50%',
            transform: 'translate(-50%, -50%)',
            fontSize: '1.1rem',
            fontWeight: 'bold'
          }}>
            {headerLabel}
          </div>
          <div style={{ position: 'absolute', right: 0, top: '50%', transform: 'translateY(-50%)', display: 'flex', alignItems: 'center', gap: '0.5rem' }}>
            <button
              onClick={() => returnToLanding()}
              style={{
                fontWeight: viewMode === 'app' ? 'bold' : 'normal',
                background: viewMode === 'app' ? 'var(--hover-bg)' : ''
              }}
            >
              🏠 Home
            </button>
            {settingsTypes.length > 0 && (
              <button
                onClick={() => switchViewMode('settings')}
                style={{
                  fontWeight: viewMode === 'settings' ? 'bold' : 'normal',
                  background: viewMode === 'settings' ? 'var(--hover-bg)' : ''
                }}
              >
                ⚙ Settings
              </button>
            )}
            {systemTypes.length > 0 && (
              <button
                onClick={() => switchViewMode('admin')}
                style={{
                  fontWeight: viewMode === 'admin' ? 'bold' : 'normal',
                  background: viewMode === 'admin' ? 'var(--hover-bg)' : ''
                }}
              >
                🔧 Admin
              </button>
            )}
            <span style={{ fontSize: '0.9rem' }}>{loggedInUser}</span>
            <button
              onClick={handleLogout}
              style={{}}
            >
              Logout
            </button>
          </div>
        </div>
        <div style={{ marginBottom: '1rem', display: 'flex', gap: '0.25rem', borderBottom: '2px solid var(--border)' }}>
          {activeTypes.map(t => (
            <button
              key={t.name}
              onClick={() => changeType(t.name)}
              style={{
                padding: '0.5rem 1rem',
                border: 'none',
                background: t.name === type ? 'var(--tab-active-bg)' : 'var(--tab-inactive-bg)',
                borderBottom: t.name === type ? '2px solid var(--tab-border)' : 'none',
                fontWeight: t.name === type ? 'bold' : 'normal',
                cursor: 'pointer'
              }}
            >
              {t.name}
            </button>
          ))}
        </div>
        <p style={{ color: listError ? 'var(--error)' : undefined }}>
          {listError || 'No records'}
        </p>
      </div>
    )
  }

  const listFields = Object.keys(data.result['list-form'])
  const addFields: string[] =
    data.result['add-form'] ? Object.keys(data.result['add-form']) : []
  const records = data.result.records

  // Chip eligibility: checkbox-list fields whose source table differs
  // from the listed type. fe-fields always sets `table` (falls back
  // to the type key), so the synthetic Roles column on non-base types
  // carries the listed type's key and is excluded here; users.roles
  // (table "roles" ≠ "users") is a real join and qualifies.
  const chipFieldMeta: Record<string, { table: string, label: string }> = {}
  for (const f of listFields) {
    const fld = data.result['list-form'][f]
    if (fld['widget'] === 'checkbox-list'
        && fld.table && fld.table !== type) {
      chipFieldMeta[f] = { table: fld.table, label: fld.label }
    }
  }
  const hasChipFields = Object.keys(chipFieldMeta).length > 0
  const hasSearchable =
    Object.values(data.result['list-form'])
      .some(f => f.searchable === true)
  // "Any filter active" is some values array with length > 0, not
  // Object.keys(listFilters).length — clearing the last chip leaves
  // { field: [] } in state, which the key count would still report.
  const anyFilterActive =
    Object.values(listFilters).some(vals => vals.length > 0)

  return (
    <div>
      <div style={{ position: 'relative' }}>
        <h1 style={{ margin: 0 }}>
          <a href="#" onClick={(e) => {
            e.preventDefault()
            returnToLanding()
          }} style={{ textDecoration: 'none', color: 'inherit' }}>
            {title}
          </a>
        </h1>
        <div style={{
          position: 'absolute',
          left: '50%',
          top: '50%',
          transform: 'translate(-50%, -50%)',
          fontSize: '1.1rem',
          fontWeight: 'bold'
        }}>
          {headerLabel}
        </div>
        <div style={{ position: 'absolute', right: 0, top: '50%', transform: 'translateY(-50%)', display: 'flex', alignItems: 'center', gap: '0.5rem' }}>
          <button
            onClick={() => returnToLanding()}
            style={{
              fontWeight: viewMode === 'app' ? 'bold' : 'normal',
              background: viewMode === 'app' ? 'var(--hover-bg)' : ''
            }}
          >
            🏠 Home
          </button>
          {settingsTypes.length > 0 && (
            <button
              onClick={() => switchViewMode('settings')}
              style={{
                fontWeight: viewMode === 'settings' ? 'bold' : 'normal',
                background: viewMode === 'settings' ? 'var(--hover-bg)' : ''
              }}
            >
              ⚙ Settings
            </button>
          )}
          {systemTypes.length > 0 && (
            <button
              onClick={() => switchViewMode('admin')}
              style={{
                fontWeight: viewMode === 'admin' ? 'bold' : 'normal',
                background: viewMode === 'admin' ? 'var(--hover-bg)' : ''
              }}
            >
              🔧 Admin
            </button>
          )}
          <span style={{ fontSize: '0.9rem' }}>{loggedInUser}</span>
          <button
            onClick={handleLogout}
            style={{}}
          >
            Logout
          </button>
        </div>
      </div>

      <div style={{ marginBottom: '1rem', display: 'flex', gap: '0.25rem', borderBottom: '2px solid var(--border)' }}>
        {activeTypes.map(t => (
          <button
            key={t.name}
            onClick={() => changeType(t.name)}
            style={{
              padding: '0.5rem 1rem',
              border: 'none',
              background: t.name === type ? 'var(--tab-active-bg)' : 'var(--tab-inactive-bg)',
              borderBottom: t.name === type ? '2px solid var(--tab-border)' : 'none',
              fontWeight: t.name === type ? 'bold' : 'normal',
              cursor: 'pointer'
            }}
          >
            {t.name}
          </button>
        ))}
      </div>

      {!(showAddForm || isEditMode) && (
        <div style={{ marginBottom: '0.5rem' }}>
          {data.result.create && (
            <button onClick={() => {
              setShowAddForm(true)
              setEditRecord(null)
              const typeRoles = data.result['type-roles'] || []
              if (typeRoles.length > 0) {
                setFormValues({ roles: [...typeRoles] })
              } else {
                setFormValues({})
              }
            }}>
              Add
            </button>
          )}
          {data.result.delete && (
            <button onClick={deleteSelected} style={{ marginLeft: '0.5rem' }}>
              Delete Selected
            </button>
          )}
        </div>
      )}

      {!(showAddForm || isEditMode) && (hasSearchable || hasChipFields) && (
        <div style={{ marginBottom: '0.75rem', display: 'flex', gap: '0.5rem', alignItems: 'center', flexWrap: 'wrap' }}>
          {hasSearchable && (
            <>
              <input
                type="text"
                placeholder="Search…"
                value={searchTerm}
                onChange={e => handleListSearch(e.target.value)}
                style={{ flex: '0 1 20rem', padding: '0.35rem 0.5rem' }}
              />
              {searchTerm && (
                <button
                  type="button"
                  onClick={() => handleListSearch('')}
                  title="Clear search"
                  style={{ padding: '0.25rem 0.5rem' }}
                >
                  ×
                </button>
              )}
              {/* Phase 2: negative search. Placeholder documents the
                  wire truth: filter like/ilike values are raw SQL
                  patterns (% and _ act as wildcards; the operator
                  path has no ESCAPE clause). */}
              <input
                type="text"
                placeholder="Not… (wildcards: % and _)"
                value={notTerm}
                onChange={e => handleListNot(e.target.value)}
                style={{ flex: '0 1 20rem', padding: '0.35rem 0.5rem' }}
              />
              {notTerm && (
                <button
                  type="button"
                  onClick={() => handleListNot('')}
                  title="Clear not-search"
                  style={{ padding: '0.25rem 0.5rem' }}
                >
                  ×
                </button>
              )}
            </>
          )}
          {/* Phase 1: hide-exclusive view preference, roles list
              only. Inline with the search box, ahead of the chips —
              chips are data filters the user built; this curates
              the default view. */}
          {type === 'roles' && (
            <label style={{ whiteSpace: 'nowrap' }}>
              <input
                type="checkbox"
                checked={hideExclusive}
                onChange={e => {
                  setHideExclusive(e.target.checked)
                  setCurrentPage(1)
                  setSelectedIds([])
                }}
              />
              {' '}Hide exclusive
            </label>
          )}
          {Object.entries(listFilters)
            .filter(([, values]) => values.length > 0)
            .map(([fieldKey, values]) =>
              values.map(v => (
                <span key={`${fieldKey}:${v}`} className="chip chip-active">
                  {chipFieldMeta[fieldKey]
                    ? `${chipFieldMeta[fieldKey].label}: ` : ''}{v}
                  <button
                    type="button"
                    className="chip-x"
                    title="Remove filter"
                    onClick={() => toggleChipFilter(fieldKey, v)}
                  >×</button>
                </span>
              ))
            )}
          {anyFilterActive && (
            <button
              type="button"
              onClick={() => {
                setListFilters({})
                setCurrentPage(1)
                setSelectedIds([])
              }}
              style={{ padding: '0.25rem 0.5rem' }}
            >
              Clear filters
            </button>
          )}
        </div>
      )}

      {(showAddForm || isEditMode) && (
        <form style={{ marginTop: '1rem', marginLeft: '1.5rem' }}>
          <h3>{isEditMode ? 'Edit' : 'Add'} {data.result['type-key']}</h3>

          <div style={{ marginBottom: '1rem' }}>
            <button type="button" onClick={submitForm}>
              {isEditMode ? 'Update' : 'Submit'}
            </button>
            <button type="button" onClick={handleCancel} style={{ marginLeft: '0.5rem' }}>
              Cancel
            </button>
          </div>

          {(isEditMode
            ? Object.keys(data.result['update-form'] || {})
            : addFields).map(f => {
            const fieldMeta = isEditMode
              ? (data.result['update-form'] || {})[f]
              : (data.result['add-form'] || {})[f]
            const allowed = data.result['allowed-values']?.[f] || []
            const isCheckboxList = fieldMeta['widget'] === 'checkbox-list'
            const isCheckbox = fieldMeta['widget'] === 'checkbox'

            if (isCheckboxList) {
              const selected = formValues[f] || []
              // Safety net: the backend palette already excludes the
              // viewer's own exclusive role (auto-injected at write
              // time).
              const isRolesField = f === 'roles'
              const myExclusive = `${loggedInUser}:exclusive`
              const options = isRolesField
                ? allowed.filter((r: string) => r !== myExclusive)
                : allowed

              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>{fieldMeta.label}</label><br />
                  <CheckboxList
                    options={options}
                    selected={selected}
                    onToggle={(val, checked) => {
                      const next = checked
                        ? [...selected, val]
                        : selected.filter((v: string) => v !== val)
                      setFormValues({ ...formValues, [f]: next })
                    }}
                  />
                </div>
              )
            }

            if (isCheckbox) {
              const checked = !!formValues[f]
              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>
                    <input
                      type="checkbox"
                      checked={checked}
                      onChange={e => setFormValues({ ...formValues, [f]: e.target.checked })}
                    />
                    {' '}{fieldMeta.label}
                  </label>
                </div>
              )
            }

            if (fieldMeta['widget'] === 'select') {
              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>{fieldMeta.label}</label><br />
                  <select
                    value={formValues[f] || ''}
                    onChange={e =>
                      setFormValues({ ...formValues, [f]: e.target.value })
                    }
                  >
                    <option value="" disabled>Select...</option>
                    {allowed.map((val: string) => (
                      <option key={val} value={val}>{val}</option>
                    ))}
                  </select>
                </div>
              )
            }

            if (fieldMeta['widget'] === 'file') {
              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>{fieldMeta.label}</label><br />
                  <input
                    type="file"
                    onChange={e => {
                      const file = e.target.files?.[0] || null
                      setFormValues({ ...formValues, [f]: file })
                    }}
                  />
                </div>
              )
            }

            if (fieldMeta['widget'] === 'password') {
              const pw = formValues[f] || ''
              const confirm = passwordConfirm[f] || ''
              const touched =
                pw !== '' || confirm !== ''
              const mismatched =
                touched && pw !== confirm
              const matchHint = !touched ? '' : mismatched
                ? ' — entries do not match'
                : ' — entries match'
              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>{fieldMeta.label}</label><br />
                  <input
                    type="password"
                    autoComplete="new-password"
                    placeholder={isEditMode
                      ? 'Leave blank to keep current password'
                      : ''}
                    value={pw}
                    onChange={e =>
                      setFormValues({ ...formValues, [f]: e.target.value })
                    }
                  /><br />
                  <input
                    type="password"
                    autoComplete="new-password"
                    placeholder="Confirm password"
                    style={{
                      marginTop: '0.25rem',
                      ...(touched
                        ? { borderColor: mismatched
                            ? 'var(--error, red)'
                            : 'var(--success, green)' }
                        : {})
                    }}
                    value={confirm}
                    onChange={e =>
                      setPasswordConfirm({
                        ...passwordConfirm, [f]: e.target.value
                      })
                    }
                  />
                  <span style={{
                    marginLeft: '0.5rem',
                    fontSize: '0.85em',
                    color: mismatched
                      ? 'var(--error, red)'
                      : 'var(--muted, gray)'
                  }}>
                    {matchHint}
                  </span>
                </div>
              )
            }

            if (fieldMeta['widget'] === 'hidden') {
              return null
            }

            if (fieldMeta['read-only'] === true) {
              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>{fieldMeta.label}</label><br />
                  {renderReadOnlyField(
                    fieldMeta, formValues[f]
                  )}
                </div>
              )
            }

            if (fieldMeta['widget'] === 'textarea') {
              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>{fieldMeta.label}</label><br />
                  <textarea
                    value={formValues[f] || ''}
                    onChange={e =>
                      setFormValues({ ...formValues, [f]: e.target.value })
                    }
                    rows={8}
                    style={{ width: '100%', resize: 'vertical' }}
                  />
                </div>
              )
            }

            if (fieldMeta['widget'] === 'code') {
              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>{fieldMeta.label}</label><br />
                  <textarea
                    value={formValues[f] || ''}
                    onChange={e =>
                      setFormValues({ ...formValues, [f]: e.target.value })
                    }
                    rows={12}
                    style={{
                      width: '100%',
                      resize: 'vertical',
                      fontFamily: 'monospace',
                      fontSize: '0.95em'
                    }}
                  />
                </div>
              )
            }

            if (fieldMeta['widget'] === 'button') {
              const statusKey = `${f}-status`
              const statusVal = editRecord?.[statusKey] || ''
              const isRunning = statusVal.startsWith('running')
              const isPending = pendingActions.has(f)
              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>{fieldMeta.label}</label><br />
                  <button
                    type="button"
                    disabled={isRunning || isPending}
                    onClick={() => handleAction(f)}
                  >
                    {isPending ? 'Working...' : fieldMeta.label}
                  </button>
                  {isRunning && (
                    <span style={{
                      marginLeft: '0.5rem',
                      fontSize: '0.85em',
                      display: 'inline-flex',
                      alignItems: 'center',
                      gap: '0.4rem',
                      color: 'var(--muted)'
                    }}>
                      <span className="du-spinner" />
                      {elapsed !== null && (
                        <span>{elapsed}s</span>
                      )}
                      {statusVal !== 'running' && (
                        <span>({statusVal})</span>
                      )}
                    </span>
                  )}
                  {!isRunning && statusVal && (
                    <span style={{
                      marginLeft: '0.5rem',
                      fontSize: '0.85em',
                      color: statusVal === 'complete'
                        ? 'var(--success, green)'
                        : statusVal.startsWith('failed')
                          ? 'var(--error)'
                          : 'var(--muted)'
                    }}>
                      {statusVal}
                    </span>
                  )}
                </div>
              )
            }

            return (
              <div key={f} style={{ marginBottom: '0.5rem' }}>
                <label>{fieldMeta.label}</label><br />
                {renderFormField(fieldMeta, formValues[f], v =>
                  setFormValues({ ...formValues, [f]: v })
                )}
              </div>
            )
          })}

          <button type="button" onClick={submitForm}>
            {isEditMode ? 'Update' : 'Submit'}
          </button>
          <button type="button" onClick={handleCancel} style={{ marginLeft: '0.5rem' }}>
            Cancel
          </button>
        </form>
      )}

      <table>
        <thead>
          <tr>
            {data.result.delete && (
              <th style={{ width: '40px', textAlign: 'center', color: 'var(--error-bright)' }}>✕</th>
            )}
            {data.result.update && (
              <th style={{ width: '60px' }}></th>
            )}
            {listFields.map(f => {
              const field = data.result['list-form'][f]
              if (field.sortable === true) {
                const isActive = sortField === f
                const indicator = isActive
                  ? (sortDir === 'asc' ? ' ▲' : ' ▼')
                  : ''
                return (
                  <th
                    key={f}
                    onClick={() => {
                      // Any sort change (field or direction) restarts
                      // the result set, so drop back to page 1 and
                      // clear the row selection.
                      setCurrentPage(1)
                      setSelectedIds([])
                      if (isActive) {
                        if (defaultSortRef.current === f) {
                          // The ranking default column only toggles
                          // desc <-> asc; clearing would re-echo the
                          // same default, so the third (unsorted)
                          // state is a no-op that would stick the
                          // board on desc.
                          setSortDir(sortDir === 'asc' ? 'desc' : 'asc')
                        } else if (sortDir === 'asc') {
                          setSortDir('desc')
                        } else {
                          setSortField(null)
                          setSortDir('asc')
                        }
                      } else {
                        setSortField(f)
                        setSortDir('asc')
                      }
                    }}
                    style={{
                      cursor: 'pointer',
                      userSelect: 'none',
                    }}
                  >
                    {field.label}{indicator}
                  </th>
                )
              }
              return <th key={f}>{field.label}</th>
            })}
          </tr>
        </thead>
        <tbody>
          {records.map((rec, idx) => (
            <tr key={idx}>
              {data.result.delete && (
                <td style={{ textAlign: 'center' }}>
                  <input
                    type="checkbox"
                    checked={selectedIds.includes(rec.id)}
                    onChange={() => toggleSelect(rec.id)}
                  />
                </td>
              )}
              {data.result.update && (
                <td>
                  <button onClick={() => openEditForm(rec)}>Edit</button>
                </td>
              )}
              {listFields.map(f => {
                const field = data.result['list-form'][f]
                // Global one-line rule: text cells clamp to one
                // line with an ellipsis; the full value rides
                // along as the hover tooltip. Media cells and chip
                // cells keep their own layout (chips wrap).
                const widget = field['widget'] || ''
                const isMedia =
                  widget === 'image' || widget === 'image-list'
                const isChipCell = f in chipFieldMeta
                const clamp = !(isMedia || isChipCell)
                const cellStyle: React.CSSProperties | undefined =
                  clamp ? {
                    maxWidth: '22rem',
                    overflow: 'hidden',
                    textOverflow: 'ellipsis',
                    whiteSpace: 'nowrap'
                  } : undefined
                const tip =
                  clamp ? cellText(rec[f], field) : ''
                return (
                  <td
                    key={f} style={cellStyle}
                    title={tip || undefined}
                  >
                    {isChipCell && Array.isArray(rec[f])
                      ? (
                        <span className="chip-cell">
                          {(rec[f] as string[]).map(v => {
                            const active =
                              (listFilters[f] || []).includes(v)
                            return (
                              <button
                                key={v}
                                type="button"
                                className={active
                                  ? 'chip chip-active' : 'chip'}
                                title={active
                                  ? `Remove filter: ${v}`
                                  : `Filter by ${v}`}
                                onClick={() =>
                                  toggleChipFilter(f, String(v))}
                              >{String(v)}</button>
                            )
                          })}
                        </span>
                      )
                      : renderCellValue(rec[f], field)}
                  </td>
                )
              })}
            </tr>
          ))}
        </tbody>
      </table>

      <div className="list-footer">
        {totalPages > 1 && (
          <div className="pagination">
            <button
              onClick={() => setCurrentPage(1)}
              disabled={currentPage === 1}
            >
              « First
            </button>
            <button
              onClick={() => setCurrentPage(p => p - 1)}
              disabled={currentPage === 1}
            >
              ‹ Prev
            </button>
            <span>Page {currentPage} of {totalPages}</span>
            <button
              onClick={() => setCurrentPage(p => p + 1)}
              disabled={currentPage === totalPages}
            >
              Next ›
            </button>
            <button
              onClick={() => setCurrentPage(totalPages)}
              disabled={currentPage === totalPages}
            >
              Last »
            </button>
          </div>
        )}
        <span className="record-count">{totalRecords} records</span>
      </div>
    </div>
  )
}

export default App