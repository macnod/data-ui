import { useEffect, useRef, useState } from 'react'
import { apiFetch, setTokens, clearTokens, getAccessToken,
         onAuthFailure, onTokenRefreshed, restoreSession }
  from './api'
import StarRating from './StarRating'
import CheckboxList from './CheckboxList'
import ChipPicker from './ChipPicker'
import FilePath from './FilePath'
import SearchSelect from './SearchSelect'
import { parseHash, canonicalize, buildUrlState,
         urlStatesEqual, writeUrl } from './urlState'
import { Markdown } from './Markdown'
import { PageDescription } from './PageDescription'
import { FieldHelp } from './FieldHelp'

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
  'hide-label'?: boolean
  // :help wire value (fe-fields passes :ui through verbatim):
  // markdown string; absent / empty / non-string render nothing.
  help?: string
  sortable?: boolean
  searchable?: boolean
  // :not-null wire value (fe-fields): JSON boolean via the
  // registered boolean key (true only when the field declares
  // :not-null t). search-select gates its clear ✕ on this.
  'not-null'?: boolean
  // :directory-type wire value (:widget :file-path only): the
  // type whose rows fill the folder select. Keyword → downcased
  // string (plist-to-json), e.g. "directories".
  'directory-type'?: string
  // :filter-with wire value: "boolean" or "reference" (bare sugar)
  // or the kind/default plist form {"kind":"boolean","default":true}
  // (the reference kind has no default). The reference picker emits
  // one target-keyed :in row per field — [target-type, target
  // identity column, "in", values] — union within the field.
  'filter-with'?: string | { kind?: string, 'default'?: boolean }
  // :column wire value (fe-fields, :target fields only): the target
  // type's identity display column, the second element of the
  // reference chip picker's :in row. Never a bare nil (plist-to-json
  // would emit []).
  column?: string
  // :enabled-when wire value (button-only, dual-consumed): AND-
  // composed [field, op, literal] rows. Keywords serialize as
  // downcased strings (plist-to-json), so ops arrive as "ne" etc.
  // Absent key means enabled.
  'enabled-when'?: [string, string, any][]
}

// Evaluate one :enabled-when row against a record (Lisp
// eval-condition-row's mirror; rules pinned in the shared
// fixture tests/fixtures/enabled-when-cases.json). Fail closed:
// absent/undefined field or null value fails the row — except
// an empty-literal eq/ne, where the empty class (null / "")
// answers directly (the :eq "" mirror of the :ne "" rule).
// Type mismatch fails ordering rows and :not-in. Never throws.
const evalEnabledRow = (
  row: [string, string, any],
  record: Record<string, any>
): boolean => {
  const [field, op, literal] = row
  const value = record?.[field]
  const empty = (v: any) =>
    v === null || v === undefined || v === ''
  if ((op === 'eq' || op === 'ne') && empty(literal)) {
    return op === 'eq' ? empty(value) : !empty(value)
  }
  if (value === null || value === undefined) return false
  switch (op) {
    case 'eq': return isEquality(value, literal)
    case 'ne': return !isEquality(value, literal)
    case 'gt': return typeof value === typeof literal && value > literal
    case 'lt': return typeof value === typeof literal && value < literal
    case 'gte': return typeof value === typeof literal && value >= literal
    case 'lte': return typeof value === typeof literal && value <= literal
    case 'in': return Array.isArray(literal)
      && literal.some(x => isEquality(value, x))
    case 'not-in': return Array.isArray(literal)
      && literal.some(x => typeof x === typeof value)
      && !literal.some(x => isEquality(value, x))
    default: return false
  }
}

// Equality under the Lisp evaluator's equal: strict for
// primitives (the wire never mixes 3 and "3").
const isEquality = (a: any, b: any): boolean =>
  typeof a === typeof b && a === b

// Evaluate an :enabled-when gate: every row must hold (AND).
// No gate (undefined) means enabled — fail-closed governs rows
// inside a present gate, never the gate's absence. THE shared
// evaluator for every site (edit form today; the list cell and
// role-gating OR at the call site when those land).
const evalEnabledWhen = (
  rows: Field['enabled-when'],
  record: Record<string, any> | null | undefined
): boolean => {
  if (!rows || rows.length === 0) return true
  if (!record) return false
  return rows.every(r => evalEnabledRow(r, record))
}

// Normalize the :filter-with wire value to its kind, or null when
// the field does not carry one. Boolean and reference by design;
// unknown kinds collapse to null (no toolbar). A future kind must
// extend this helper, not rely on the null fallthrough.
const filterWithKind = (
  f: Field['filter-with']
): string | null =>
  f === 'boolean' || f === 'reference'
    ? f
    : typeof f === 'object' && f !== null
      && (f.kind === 'boolean' || f.kind === 'reference')
      ? f.kind
      : null

// Authored initial state for a boolean filter select: 'yes' /
// 'no' from the plist default, null when absent (Any) or the
// bare sugar form. Kind-guarded so a future kind whose plist
// carried a default never seeds a boolean select.
const filterWithDefault = (
  f: Field['filter-with']
): 'yes' | 'no' | null =>
  filterWithKind(f) !== 'boolean'
    ? null
    : typeof f === 'object' && f !== null
      ? f['default'] === true ? 'yes'
        : f['default'] === false ? 'no'
        : null
      : null

// Chip-eligible field metadata for both picker families: M2M
// (checkbox-list whose source table differs from the listed type)
// and reference (:filter-with :reference on a field whose table is
// the :target and carries the wire :column). One membership rule,
// two call sites — the render-site memo and fetchList's in-fetch
// rebuild — so the pickers shown and the rows emitted can never
// diverge. Discovery walks the whole form key set, the boolean
// precedent (listFields is the rendered column set and can diverge
// from the form the query covers).
const buildChipFieldMeta = (
  form: Record<string, Field>, type: string
): Record<string, { table: string,
                    column?: string,
                    label: string, m2m: boolean }> => {
  const meta: Record<string, { table: string,
                               column?: string,
                               label: string, m2m: boolean }> = {}
  for (const f of formFieldKeys(form)) {
    const fld = form[f]
    if (fld['widget'] === 'checkbox-list'
        && fld.table && fld.table !== type) {
      meta[f] = { table: fld.table, label: fld.label, m2m: true }
    } else if (filterWithKind(fld['filter-with']) === 'reference'
               && fld.table && fld.table !== type
               && fld.column) {
      meta[f] = { table: fld.table, column: fld.column,
                  label: fld.label, m2m: false }
    }
  }
  return meta
}

// Authored initial state for every boolean filter field on a
// list form (fieldKey -> 'yes' | 'no'; fields with no default
// omitted). Shared by the seeding effect and the Clear filters
// handler — module-level so the effect can live with the other
// effects, before the render body computes its own copy.
const boolDefaultsOf = (
  form: Record<string, Field> | undefined
): Record<string, 'yes' | 'no'> =>
  Object.fromEntries(
    Object.entries(form || {})
      .map(([fieldKey, f]) =>
        [fieldKey, filterWithDefault(f['filter-with'])])
      .filter(([, d]) => d !== null)
  ) as Record<string, 'yes' | 'no'>

// Wire form objects carry one non-field key: 'hide-list' (a
// form-level declaration the backend appends beside the field
// entries). Field walks must not see it. The parameter is
// Record<string, any> — the ListResponse forms are declared
// Record<string, Field>, and reading the flag off a Field-typed
// value does not compile (no overlap between Field and boolean);
// structural typing makes the wire value assignable here. Do not
// add 'hide-list' to Field (it is not a field).
const formFieldKeys = (
  form: Record<string, any> | undefined
): string[] =>
  Object.keys(form || {}).filter(k => k !== 'hide-list')

interface ListResponse {
  status: string
  result: {
    'type-key': string
    'list-form': Record<string, Field>
    // Rollups emit only list-form (the compiler forbids add/update
    // view forms on :rollup types), so all three are optional on
    // the wire. view-form presence is the declaration bit: absent
    // means the type has no view surface (no fallback).
    'add-form'?: Record<string, Field>
    'update-form'?: Record<string, Field>
    'view-form'?: Record<string, Field>
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

// Per-record capability flags on every /api/list record and the
// /api/item record (row-level — the same gate the write
// endpoints enforce). Strict === true everywhere: plist-to-json
// emits [] for a bare nil, which must not count as permitted.
interface RecordCaps {
  update?: boolean
  delete?: boolean
}

interface TypeInfo {
  name: string
  category: 'user' | 'system' | 'settings'
  // True only on :user-setting types (e.g. settings) — one record
  // per user by nature. Multi-record settings types (secrets)
  // leave it unset.
  userSetting?: boolean
  // :page-description — markdown intro block on the list page.
  // pageDescriptionOpen is presence-preserving: undefined means
  // the author declared nothing (collapsed default); false means
  // an explicit :page-description-open nil.
  pageDescription?: string
  pageDescriptionOpen?: boolean
}

type ViewMode = 'app' | 'admin' | 'settings'

// Page size for list pagination. Matches the frozen list contract:
// be-list and /api/list both default limit to 20. The FE always
// sends limit=, so the server default is unused, but the numbers
// stay aligned.
const PAGE_SIZE = 20

// Interactive descendants disarm the row click (and the row's
// Enter handler): native controls by tag, div-based ones (the
// lightbox thumbnails' data-no-row-click marker) by attribute.
const INTERACTIVE_SELECTOR =
  'a, button, input, select, textarea, ' +
  'label, [data-no-row-click]'

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

// Markdown source → plain text for the one-line cell preview:
// images keep their alt, links keep their text, emphasis
// markers and heading / quote / list sigils drop. Flattening
// follows markdown semantics: a blank line (paragraph break)
// becomes a middle dot so a title and its body don't run
// together; a single newline is a soft break and renders as a
// space, exactly as rendered markdown would. Not a renderer —
// View mode (the reading surface) still gets the full
// <Markdown>. Feeds the cell and its tooltip via cellText.
function stripMarkdown(src: string): string {
  return src
    .replace(/!\[([^\]]*)\]\([^)]*\)/g, '$1')   // images → alt
    .replace(/\[([^\]]+)\]\([^)]*\)/g, '$1')    // links → text
    .replace(/(\*\*|__)(.+?)\1/g, '$2')         // bold
    .replace(/~~(.+?)~~/g, '$1')                // strikethrough
    .replace(/\*(.+?)\*/g, '$1')                // italic
    .replace(/`+([^`]*)`+/g, '$1')              // inline code
    .replace(/^\s{0,3}#{1,6}\s+/gm, '')         // headings
    .replace(/^\s{0,3}>+\s?/gm, '')             // blockquotes
    .replace(/^\s*(?:[-*+]|\d+\.)\s+/gm, '')    // list markers
    .replace(/\s*\n\s*\n\s*/g, ' · ')           // paragraphs → dot
    .replace(/\s*\n\s*/g, ' ')                  // soft breaks → space
    .trim()
}

// Plain-text projection of a cell value: arrays join with ", ",
// numbers get :precision formatting, markdown cells strip their
// markup so the preview and tooltip read as text. Feeds the
// default cell rendering, the code widget, and the one-line
// clamp tooltip.
function cellText(val: any, field: Field): string {
  const text = Array.isArray(val) ? val.join(', ')
    : formatNumber(val, field)
  return field['widget'] === 'markdown'
    ? stripMarkdown(String(text)) : text
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

  // markdown needs no branch here: cellText strips its markup,
  // so the default text path renders the standard one-line
  // clamp (<td> ellipsis + raw-text tooltip). View mode is the
  // reading surface.

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
  const name = basename(path)
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
        // The inline arrows' identity churn is load-bearing: it
        // is what keeps a future React.memo on the lightbox
        // path from skipping the render that rebuilds image
        // URLs after a token rotation. Do not stabilize them.
        <ImageModal
          type={type}
          paths={[path]}
          index={0}
          onClose={() => setOpen(false)}
          onNavigate={() => {}}
        />
      )}
    </>
  )
}

// Mirrors *widgets* in lisp/model.lisp. The compiler rejects
// unknown widgets and injects :widget :textbox when :widget is
// missing, so a widget string outside this set means
// frontend/backend vocabulary drift — fail loudly here instead
// of silently rendering a text input. '' stays a text input
// (the compiler's own default rule for a missing :widget).
const WIDGET_VOCABULARY = new Set([
  'textbox', 'textarea', 'code', 'markdown', 'stars', 'checkbox',
  'checkbox-list', 'select', 'search-select', 'file', 'file-path',
  'password-new', 'password-read', 'button', 'hidden', 'image',
  'image-list'
])

// Two-column property-sheet row for every field in the View/Edit
// overlay form: label right-aligned in a fixed left column
// (.field-label), value in the remainder (.field-value). Markdown
// bodies made label-on-top bury the next field's name mid-page;
// the gutter keeps the label->value pairing scannable. :hide-label
// (strict === true, so absent, false, and a legacy [] all mean
// shown) keeps the label column empty so the gutter and value
// alignment hold. Row spacing lives in .field-row.
function FieldRow({
  field, children
}: { field: Field; children: React.ReactNode }) {
  // :help renders its "?" toggle beside the label only when the
  // label is shown (help + :hide-label t is a compile error, and
  // the === true guard is the defensive ignore for a stale wire).
  // Absent / empty / non-string help renders nothing.
  const help = typeof field.help === 'string' && field.help.length > 0
    ? field.help : null
  return (
    <div className="field-row">
      <div className="field-label">
        {field['hide-label'] === true
          ? null
          : <>
              <label>{field.label}</label>
              {help && field.label
                ? <FieldHelp text={help} label={field.label} /> : null}
            </>}
      </div>
      <div className="field-value">{children}</div>
    </div>
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

  if (widget === 'textbox' || widget === '') {
    return (
      <input
        type="text"
        value={value || ''}
        onChange={e => onChange(e.target.value)}
        style={{ width: '80ch', maxWidth: '100%' }}
      />
    )
  }

  // Widget present but no branch above caught it. Known =
  // routing bug (branch added to the form, not here); unknown =
  // backend vocabulary grew without the frontend.
  const known = WIDGET_VOCABULARY.has(widget)
  console.error(
    known
      ? `renderFormField: no branch for known widget "${widget}"`
      : `renderFormField: unknown widget "${widget}" ` +
        `(not in the compiled vocabulary)`)
  return (
    <span style={{ color: 'var(--error-bright)' }}>
      {known ? 'Unhandled' : 'Unknown'} widget: {widget}
    </span>
  )
}

// --- Image helpers ---

// Stored image values are full paths; the basename is the
// display/download name (alt text, download suggestion), the full
// path is the lightbox caption.
function basename(path: string): string {
  return path.split('/').pop() || path
}

function fileUrl(type: string, path: string): string {
  const token = getAccessToken()
  const base = `/api/file?type=${encodeURIComponent(type)}`
    + `&path=${encodeURIComponent(path)}`
  return token ? `${base}&token=${encodeURIComponent(token)}` : base
}

function ImageModal({
  type, paths, index, onClose, onNavigate
}: {
  type: string
  paths: string[]
  index: number
  onClose: () => void
  onNavigate: (index: number) => void
}) {
  // Live values through refs (the fetchListRef pattern): the
  // keyboard handler must not close over `paths` or `index` —
  // a same-length array swap would not re-subscribe, and the
  // handler would navigate the array from when the lightbox
  // opened. Reading both at event time closes that class.
  const pathsRef = useRef(paths)
  const indexRef = useRef(index)
  pathsRef.current = paths
  indexRef.current = index

  // Shrink guard: modalIndex outlives a refetch, so `paths`
  // can shrink under an open modal (delete, filter, the
  // post-action fetchList). Clamp instead of letting
  // paths[index] go undefined and throw in the render below.
  if (!paths || paths.length === 0) return null
  const i = Math.min(index, paths.length - 1)
  // Render-time fileUrl: an open lightbox must rebuild its URL
  // (and Download / Open-in-new-tab hrefs) when the access
  // token rotates mid-session — a pre-baked src would hold the
  // dead token for the life of this open.
  const currentPath = paths[i]
  const current = {
    src: fileUrl(type, currentPath),
    filename: basename(currentPath),
    caption: currentPath
  }

  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      if (e.key === 'Escape') {
        onClose()
        return
      }
      const n = pathsRef.current.length
      if (n === 0) return
      const cur = Math.min(indexRef.current, n - 1)
      if (e.key === 'ArrowLeft')
        onNavigate((cur - 1 + n) % n)
      else if (e.key === 'ArrowRight')
        onNavigate((cur + 1) % n)
    }
    window.addEventListener('keydown', handler)
    return () => window.removeEventListener(
      'keydown', handler
    )
  }, [onNavigate, onClose])

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
      {/* Pinned near the top of the overlay so its position is
          independent of the image height. */}
      <div style={{
        position: 'absolute',
        top: '1rem',
        left: '50%',
        transform: 'translateX(-50%)',
        display: 'flex',
        gap: '1rem',
        alignItems: 'center',
        cursor: 'default'
      }}>
        {paths.length > 1 && (
          <button
            type="button"
            onClick={e => {
              e.stopPropagation()
              onNavigate(
                (i - 1 + paths.length) % paths.length
              )
            }}
          >
            ‹ Prev
          </button>
        )}
        <span style={{ color: 'var(--muted)', fontSize: '0.85rem' }}>
          {i + 1} / {paths.length}
        </span>
        {paths.length > 1 && (
          <>
            <button
              type="button"
              onClick={e => {
                e.stopPropagation()
                onNavigate((i + 1) % paths.length)
              }}
            >
              Next ›
            </button>
            <span style={{ width: '1rem' }} />
          </>
        )}
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
      {/* Caption strip: the image's full stored path (the
          disambiguator for same-named files). `filename` stays the
          basename for download/alt; this slot is the hook point for
          the Post MVP authored-caption layer. */}
      {current.caption && (
        <div
          onClick={e => e.stopPropagation()}
          style={{
            marginTop: '0.5rem',
            color: 'var(--muted)',
            fontSize: '0.9rem',
            cursor: 'default',
            textAlign: 'center',
            maxWidth: '90vw',
            overflowWrap: 'anywhere'
          }}
        >
          {current.caption}
        </div>
      )}
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

  // The lightbox pages through the full `paths` list (indices
  // align: the shown thumbnails are a prefix slice). It builds
  // its own URLs at render time — no pre-baked array here.
  const shown = max != null ? paths.slice(0, max) : paths

  return (
    <>
      <div style={{
        display: 'flex',
        flexWrap: 'wrap',
        gap: '0.5rem'
      }}>
        {shown.map((p, i) => {
          const name = basename(p)
          return (
            <div
              key={p}
              data-no-row-click=""
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
        // Inline arrows again: identity churn is load-bearing
        // against memoization (see ImagePreview's call site).
        <ImageModal
          type={type}
          paths={paths}
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
    // Array-valued stars (write-through rating on Model Bank comes
    // back as [] for a raterless viewer) collapse to the single
    // value; empty renders the em dash, not a zero-star row.
    const num = typeof value === 'number' ? value
      : Array.isArray(value)
        ? value.length > 0 ? Number(value[0]) : null
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

  // markdown read-only: rendered, full length (View mode is the
  // reading surface; the list cell is the clamped preview). Empty
  // renders the em dash, mirroring the code / textarea branch.
  if (widget === 'markdown') {
    const text = Array.isArray(value) ? value.join(', ')
      : formatNumber(value, field)
    if (!text) {
      return <div style={{ color: 'var(--muted-2)' }}>—</div>
    }
    return (
      <div style={{ padding: '0.3rem 0', minHeight: '1.2em' }}>
        <Markdown text={String(text)} />
      </div>
    )
  }

  // code / textarea read-only: plain-text projection, but
  // pre-wrap so newlines survive — a collapsed blob is
  // unreadable for model source. code adds the monospace
  // styling its edit twin carries.
  if (widget === 'code' || widget === 'textarea') {
    const text = Array.isArray(value) ? value.join(', ')
      : formatNumber(value, field)
    return (
      <div style={{
        padding: '0.3rem 0',
        color: 'var(--label)',
        minHeight: '1.2em',
        whiteSpace: 'pre-wrap',
        ...(widget === 'code'
          ? { fontFamily: 'monospace', fontSize: '0.95em' }
          : {})
      }}>
        {text || '—'}
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
  const [viewMode, setViewMode] = useState<ViewMode>('app')
  const [type, setType] = useState('__init__')
  // :page-description open/closed state, App-level and per-type:
  // the block mounts inside the list page, which refetches on
  // every sort / search / page / chip change — component-local
  // state would snap back to the model default on each refetch.
  // The map records only explicit toggles; an absent entry falls
  // to the model default (pageDescriptionOpen === true). React
  // state only: resets on reload, cleared on logout.
  const [pageDescOpen, setPageDescOpen] = useState<
    Record<string, boolean>>({})
  const [showAddForm, setShowAddForm] = useState(false)
  const [formValues, setFormValues] = useState<Record<string, any>>({})
  // Confirmation box for :widget :password-new fields. Keyed by
  // field name so multi-password forms stay independent; cleared
  // with the rest of the form state. (:password-read never
  // confirms — it collects an existing secret, it does not set
  // one.)
  const [passwordConfirm, setPasswordConfirm] =
    useState<Record<string, string>>({})
  const [selectedIds, setSelectedIds] = useState<string[]>([])
  const [editRecord, setEditRecord] = useState<any>(null)
  // Read-only record view: the open editRecord renders through
  // the read-only field branches instead of inputs. Set by
  // openEditForm(record, true); cleared everywhere editRecord is.
  const [formViewing, setFormViewing] = useState(false)
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
  // :filter-with :boolean selects: fieldKey -> 'yes' | 'no'. An
  // absent key means Any (the no-row state; absence is what makes
  // "active" a key count). Kept separate from listFilters (chip
  // shape Record<string, string[]>) — do not overload it.
  const [boolFilters, setBoolFilters] = useState<
    Record<string, 'yes' | 'no'>
  >({})
  // Seeding latch for those selects: the type we last seeded the
  // authored defaults for, or null (re-seed on next entry). Lives
  // with the other refs so the hook order is unconditional — a
  // hooks-order crash here blanks the page on login.
  const boolSeededForRef = useRef<string | null>(null)
  // Chip picker (searchable chip filtering): which chip field's
  // picker is open, or null. Lifted here, not owned per-picker,
  // so opening one closes any other. Not cleared by
  // resetListQuery: the filter bar unmounts on every type /
  // view change, which unmounts the open panel with it — an
  // effect adding setChipPickerOpen(null) to those paths would
  // be dead code in practice.
  const [chipPickerOpen, setChipPickerOpen] = useState<
    string | null
  >(null)
  // App state in the URL (Step 2.5): names the type whose first
  // fetch went out without its form-dependent rows (not-ilike /
  // bool :eq) because data did not yet belong to it — a URL
  // re-seed assigns those fields in the same commit as setType,
  // so the first request runs against the previous type's form
  // (or none). The arrival effect below consumes it: one refetch
  // when the type's form arrives. Cleared by resetListQuery
  // alongside boolSeededForRef, and by the arrival effect itself
  // (one-shot by construction). Never armed on the click paths —
  // changeType / switchViewMode / returnToLanding run
  // resetListQuery first, so wantsFormRows is false there.
  const pendingFormRefetchRef = useRef<string | null>(null)
  // Terminal button statuses (complete / failed: … / idle) are
  // stale the moment their form is closed; openEditForm blanks them
  // on the next open. Set for the single re-open handleAction does
  // right after a press, so the just-earned status still shows.
  const preserveStatusOnOpenRef = useRef(false)
  // Restore signal: set once by the restore effect before the
  // /api/public-info effect's async callback runs (mount effects
  // fire in declaration order). The public-info effect reads it
  // inside its .then to suppress :guest-auto for a restored
  // session. A ref, not state: both effects fire in the same
  // commit and the flag must be readable without racing
  // setLoggedIn. Must stay an unconditional hook up here with
  // the other refs — a hooks-order crash blanks the page.
  const restoredRef = useRef(false)

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

  // Restore a persisted session ("stay logged in"): seed the
  // api module vars and flip loggedIn so the [loggedIn] data
  // effects run exactly as after a fresh login. No preflight:
  // a stale access token is recovered by apiFetch's 401 →
  // refresh → retry, and a dead refresh token lands on the
  // login screen via onAuthFailure. No cleanup: StrictMode
  // double-invokes mount effects in dev and restoreSession is
  // idempotent — a cleanup nulling the module vars would log
  // the user out in dev only.
  useEffect(() => {
    const session = restoreSession()
    if (session) {
      restoredRef.current = true
      setLoggedIn(true)
      setLoggedInUser(session.username)
      fetchAndApplyCssVariables()
    }
  }, [])

  // Re-render image URLs after a silent token rotation: <img>
  // srcs bypass apiFetch (no 401 retry), and fileUrl reads
  // getAccessToken() at render time — the bump is what makes
  // that render happen. Registered from a mount effect, never
  // the render body (a render-body assignment stacks stale
  // callbacks). Single-slot: this registration replaces any
  // earlier one, and the cleanup restores whatever was there
  // before us. Only the setter is taken today: nothing reads
  // the count while nothing is memoized — the re-render itself
  // rebuilds the URLs. If React.memo ever lands on the image
  // path, take the value too and thread it as a prop.
  const [, setTokenVersion] = useState(0)
  useEffect(() => {
    const displaced = onTokenRefreshed(
      () => setTokenVersion(v => v + 1))
    return () => { onTokenRefreshed(displaced) }
  }, [])

  // Shared post-login path: the Login button, "Continue as guest",
  // and auto-guest all land here.
  const completeLogin = (user: string, access: string, refresh: string) => {
    setTokens(access, refresh, user)
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
          // A restored session suppresses auto-guest (the ref is
          // set by the restore effect before this callback runs)
          // — restore must win, or guest login would clobber the
          // stored session and replace the server-side refresh
          // token.
          const guestAuto = json.result['guest-auto']
          if (guestAuto !== false
              && !restoredRef.current
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
  // View mode: an open record rendered read-only (no Update /
  // Delete / action buttons) — for users who can read a type
  // but not update it, and for editors who just want to look.
  const isViewMode = !!editRecord && formViewing
  // The form an open record form reads from. Absent view-form
  // means no view surface (no fallback), so view mode exists
  // only when the payload declares it. hasViewForm gates the
  // only two entry points (the row click, the settings
  // auto-open).
  const hasViewForm = !!data?.result?.['view-form']
  const viewFormName = 'view-form'
  const openFormName = isViewMode ? viewFormName : 'update-form'

  // The open form's :hide-list t: the author suppressed the list
  // (table, pagination, record count) while this form is open.
  // Resolve the open form once with the renderer's own precedence
  // — isEditMode before showAddForm, the <form> block's condition
  // (isViewMode implies isEditMode; both derive from editRecord) —
  // then read the flag through Record<string, any> (a Field-typed
  // index does not compile against boolean; the flag is not a
  // field). Absent, false, and a legacy [] all mean show — strict
  // === true, the hide-label guard's shape. No form open →
  // undefined → false → the list shows: that is the whole
  // product rule.
  const activeForm = isEditMode
    ? data?.result?.[openFormName]
    : showAddForm ? data?.result?.['add-form'] : undefined
  const hideList =
    (activeForm as Record<string, any> | undefined)
      ?.['hide-list'] === true

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
    // Chip filters, two families sharing the listFilters values
    // slot: M2M chips emit one [type, fieldKey, "has-all", values]
    // row per chip-eligible field — intersection within the field
    // (additive chip clicks narrow), field-scoped shape, first
    // element the listed type. Reference chips emit one
    // target-keyed [table, column, "in", values] row — first
    // element the field's :target type key, second the target's
    // identity display column (the wire :column), union within the
    // field (a single-value FK has no intersection reading); two
    // reference fields sharing one target emit two rows, ANDed
    // through the one shared join column (distinct selections
    // typically match nothing — v1 limitation; the joined-column
    // aliasing relaxation owns per-field rows).
    // Across fields the rows AND-compose. Both families' rows are
    // form-dependent (meta comes from the arrived form), hence the
    // chipMeta guard: an empty map when formReady is false — no
    // rows against a departed type's form; the armed one-shot
    // refetch rebuilds them.
    {
      const rows: (string | string[])[][] = []
      // Raw non-emptiness — NOT a picker-arm narrowing: the arming
      // path runs only when formReady is false, and chipMeta is {}
      // exactly then (a narrowing is empty precisely where it is
      // consulted — dead code that silently drops the filter).
      // listFilters keys always belong to the requested type at
      // fetch time (resetListQuery clears on type change; both URL
      // seeds assign together with setType); keys naming no picker
      // are skipped by the (!meta) continue below and cost one
      // harmless refetch.
      const formReady = data?.result?.['type-key'] === type
      const chipMeta = formReady
        ? buildChipFieldMeta(
            data?.result?.['list-form'] || {}, type)
        : {}
      for (const [fieldKey, values] of Object.entries(listFilters)) {
        if (values.length === 0) continue
        const meta = chipMeta[fieldKey]
        if (!meta) continue
        if (meta.m2m) {
          // M2M: unchanged emission, intersection within the
          // field (the todo-list chip-narrowing behavior).
          rows.push([type, fieldKey, 'has-all', values])
        } else {
          // Reference: target-keyed, the backend's existing
          // join-filter pushdown row — first element the field's
          // :target type key, second the target's identity display
          // column (the wire :column).
          rows.push([meta.table, meta.column!, 'in', values])
        }
      }
      // Phase 1: hide the per-user exclusive roles (name suffix
      // :exclusive, trigger-enforced — same signal the backend's
      // exclusive-role-p reads). Case-sensitive not-like is exact
      // here: the suffix is machine-written lowercase.
      if (type === 'roles' && hideExclusive) {
        rows.push(['roles', 'name', 'not-like', '%:exclusive'])
      }
      // Phase 2: one not-ilike row per searchable field — the
      // complement of search's OR-ILIKE group (rows AND
      // together). Form-dependent rows (this loop and the bool
      // :eq loop below) read the *last fetched* type's form, not
      // the requested type's — data is not a fetch-effect dep.
      // On the click paths resetListQuery has emptied these
      // fields, so the stale form contributes nothing; a URL
      // re-seed (login / history listener) assigns notq / bool
      // in the same commit as setType, so the first request
      // would read the wrong form ({} at login, the departed
      // type on cross-type Back — silent unfiltered list or a
      // parse-field 404, with nothing healing it). Guard: build
      // these rows only when data belongs to the requested type;
      // otherwise arm the one-shot refetch (Step 2.5's arrival
      // effect) that refires once the form arrives.
      const wantsFormRows =
        !!debouncedNotTerm || Object.keys(boolFilters).length > 0
        || Object.values(listFilters)
             .some(v => v.length > 0)
      if (!formReady && wantsFormRows) {
        pendingFormRefetchRef.current = type
      }
      const form = formReady
        ? data?.result?.['list-form'] || {} : {}
      if (debouncedNotTerm) {
        for (const [fieldKey, f] of Object.entries(form)) {
          if (f.searchable === true) {
            rows.push([type, fieldKey, 'not-ilike',
                       `%${debouncedNotTerm}%`])
          }
        }
      }
      // Boolean selects (:filter-with :boolean): one :eq row per
      // field with a Yes/No choice. The value is the string
      // ":true" / ":false" — parse-filters coerces those to the
      // keywords on :type :boolean fields, the only encoding
      // value-type-p accepts. Any sends no row. Same
      // form-dependent guard as the not-ilike loop above.
      {
        for (const [fieldKey, f] of Object.entries(form)) {
          if (filterWithKind(f['filter-with']) !== 'boolean') continue
          const choice = boolFilters[fieldKey]
          if (choice === 'yes' || choice === 'no') {
            rows.push([type, fieldKey, 'eq',
                       choice === 'yes' ? ':true' : ':false'])
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
    setBoolFilters({})
    // Forget the per-type seeding so the next type (or re-entry
    // into this one) re-seeds its authored defaults. This ref is
    // the only thing standing between every list refetch and
    // re-clobbering the user's filters — it must be cleared by
    // every future {} clearer too, or the next fetch re-seeds
    // and silently undoes the user's Any.
    boolSeededForRef.current = null
    // App state in the URL (Step 2.5): a type click between the
    // arming fetch and the arrival must not fire a refetch for a
    // type the user left. Cheap, and keeps the two per-type refs
    // under one clearer (logout is covered too —
    // resetSessionState calls this).
    pendingFormRefetchRef.current = null
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
    setData(null)
    setShowAddForm(false)
    setEditRecord(null)
    setFormViewing(false)
    setFormValues({})
    setPasswordConfirm({})
    setListError(null)
    setPageDescOpen({})
    resetListQuery()
  }

  const changeType = (newType: string) => {
    setType(newType)
    setShowAddForm(false)
    setEditRecord(null)
    setFormViewing(false)
    setFormValues({})
    setPasswordConfirm({})
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
    setFormViewing(false)
    setFormValues({})
    setPasswordConfirm({})
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

  const openEditForm = async (record: any, viewing = false) => {
    // Fetch the full record from /api/item so that fields absent
    // from the form we already hold (e.g. deploy-status,
    // my-rating) are populated correctly in the open form.
    // Edit mode reads update-form; view mode reads view-form,
    // which only exists when the model declares it (no
    // fallback) — callers gate entry on hasViewForm / the
    // update flag, so the name resolves here.
    const formName = viewing ? viewFormName : 'update-form'
    let fullRecord = record
    try {
      const res = await apiFetch(
        `/api/item?type=${type}&id=${record.id}&form=${formName}`
      )
      if (res.ok) {
        const json = await res.json()
        fullRecord = json?.result?.record || record
      }
    } catch {
      // Fall back to list record on error
    }
    // A terminal status written by an earlier action is stale on
    // every fresh open (row Edit/View, the settings auto-open) —
    // clear it so the form never greets the user with an old
    // "complete". Kept when this open is the post-action re-open
    // (handleAction set the ref) or when a worker is still
    // running. Display-only: the blank lives in editRecord /
    // formValues and never reaches the server (submit drops
    // empty-string fields).
    // Exception: a field named in some button's :enabled-when
    // rows. evalEnabledWhen (the button render block) evaluates
    // the gate against editRecord, so blanking a gated status
    // would flip a gated-off button back to enabled on open —
    // the terminal value this loop hides is exactly the state
    // the gate keys on; for a gated button it is durable state,
    // not stale run telemetry. Collected from the gate rows, not
    // from button names, so a gate on another button's status
    // companion is covered too; plain-field gates (Deploy's
    // :model) need nothing — only *-status keys are blanked.
    const keepStatus = preserveStatusOnOpenRef.current
    preserveStatusOnOpenRef.current = false
    if (!keepStatus) {
      const cleared: Record<string, any> = { ...fullRecord }
      const openFormMeta = data?.result?.[formName] || {}
      const gatedFields = new Set<string>()
      for (const f of formFieldKeys(openFormMeta)) {
        const rows = openFormMeta[f]?.['enabled-when']
        if (!rows) continue
        for (const row of rows) gatedFields.add(row[0])
      }
      for (const k of Object.keys(cleared)) {
        if (k.endsWith('-status')
            && typeof cleared[k] === 'string'
            && !cleared[k].startsWith('running')
            && !gatedFields.has(k)) {
          cleared[k] = ''
        }
      }
      fullRecord = cleared
    }
    setEditRecord(fullRecord)
    setFormViewing(viewing)
    // Filter out the current user's own exclusive role from the
    // roles — it's injected automatically by the backend and should
    // never appear as a manual checkbox or be sent back. Harmless
    // in view mode (roles render as comma-joined text).
    const myExclusive = `${loggedInUser}:exclusive`
    const cleanRoles = (fullRecord.roles || []).filter(
      (r: string) => r !== myExclusive
    )
    setFormValues({ ...fullRecord, roles: cleanRoles })
    // Fresh form session: a confirm value typed into an earlier,
    // abandoned form must not leak into this form's password gate.
    setPasswordConfirm({})
    setShowAddForm(false)
    // The form renders at the top of the page, above the table.
    // Without this, clicking Edit/View on a row near the bottom
    // leaves the viewport scrolled down — the form is open but
    // off-screen. openEditForm is async (the /api/item fetch
    // above), so React has already re-rendered and mounted the
    // form by the time we get here.
    window.scrollTo(0, 0)
  }

  const closeForm = () => {
    setEditRecord(null)
    setFormViewing(false)
    setShowAddForm(false)
    setFormValues({})
    setPasswordConfirm({})
  }

  // Row click opens the read-only View form (guarded on
  // hasViewForm — no view form, no surface). A text-selection
  // drag fires click on mouseup — bail so selecting text never
  // opens (checked first; a collapsed selection stringifies to
  // ''). Interactive descendants disarm themselves through
  // INTERACTIVE_SELECTOR.
  const handleRowClick = (
    e: React.MouseEvent<HTMLTableRowElement>, rec: any
  ) => {
    if (window.getSelection()?.toString()) return
    const t = e.target as HTMLElement | null
    if (t?.closest(INTERACTIVE_SELECTOR)) return
    openEditForm(rec, true)
  }

  // Enter activates the focused row (Space keeps scrolling the
  // page). Same disarm guard as the click path: keydown bubbles
  // like click, so a focused child (checkbox, markdown link)
  // handling its own Enter must not also open the row behind
  // its own action. e.target is the focused element; the <tr>
  // itself is not in the selector, so Enter on the row is
  // unaffected. No selection guard here: Enter is never the
  // tail of a drag.
  const handleRowKeyDown = (
    e: React.KeyboardEvent<HTMLTableRowElement>, rec: any
  ) => {
    if (e.key !== 'Enter') return
    const t = e.target as HTMLElement | null
    if (t?.closest(INTERACTIVE_SELECTOR)) return
    openEditForm(rec, true)
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

  // Hover-blur for the login screen's buttons: Firefox's
  // saved-login dropdown is browser chrome that drops below the
  // focused input — right over the Login button — and the first
  // click while it is open is consumed dismissing it. Blurring
  // the focused input on button hover closes the popup before
  // mousedown, so the click lands.
  const handleLoginButtonHover = () => {
    const el = document.activeElement
    if (el instanceof HTMLInputElement) el.blur()
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
    // Edit-form field metadata for the open form (update-form when
    // editing; add-form never renders buttons, but the lookup is
    // guarded anyway).
    const formDef = isEditMode
      ? data!.result['update-form'] || {}
      : data!.result['add-form'] || {}
    // Password confirmation gate: the same loop submitForm runs.
    // Without it the confirm boxes are decorative on the one path
    // that actually changes a password (e.g. :change-password).
    for (const f of formFieldKeys(formDef)) {
      if (formDef[f]['widget'] !== 'password-new') continue
      const pw = typeof formValues[f] === 'string'
        ? formValues[f] : ''
      const confirm = passwordConfirm[f] || ''
      if (pw === '' && confirm === '') continue
      if (pw !== confirm) {
        alert(`Password entries for "${formDef[f].label}" do not match`)
        return
      }
    }
    // Form values for hooks that read typed input (e.g. :change-password
    // reads :current-password / :new-password). Whitelist: string-valued
    // fields the form schema knows (password-new, password-read,
    // textbox, textarea, select, code), with untouched passwords
    // normalized to '' — never the raw formValues spread (File
    // objects, array-valued roles, and status companions are not
    // JSON-representable form data).
    const actionData: Record<string, string> = {}
    for (const f of formFieldKeys(formDef)) {
      const w = formDef[f]['widget']
      if (!['password-new', 'password-read', 'textbox', 'textarea',
            'select', 'code'].includes(w)) continue
      const v = formValues[f]
      actionData[f] = (typeof v === 'string') ? v : ''
    }
    setPendingActions(prev => new Set(prev).add(fieldKey))
    try {
      const res = await apiFetch('/api/actions', {
        method: 'POST',
        body: JSON.stringify({
          type,
          id,
          field: fieldKey,
          data: actionData
        })
      })
      // Parse the body exactly once — a Response body is
      // single-read, so the alert and the status check must share
      // this parse.
      let body: any = null
      try {
        body = await res.json()
      } catch {
        // A 200 whose body is not JSON is not a success; leave
        // body null. The re-open branch below is the safe
        // direction — never close on doubt.
      }
      if (!res.ok) {
        // 400 bodies (permissions, already-running) put the
        // message in top-level error — the same lookup
        // errorMessage() performs, minus the second body read.
        alert(body?.error || 'Action failed')
      }
      if (res.ok && body?.result?.status === 'complete') {
        // Sync success: the action changed the record under the
        // form (e.g. :spawn closed it and spawned a successor).
        // Close, don't re-open — the form would sit open on
        // stale history and invite edits. Same shape as
        // submitForm's success branch (close, then refresh); the
        // order is cosmetic — closeForm only clears React state
        // and fetchList doesn't read the form.
        if (viewMode === 'settings') {
          // Not awaited, and no fetchList: returnToLanding closes
          // the form and triggers its own landing fetch
          // (/api/info → type change → the list effect). A second
          // fetchList here would race it.
          returnToLanding()
        } else {
          closeForm()
          await fetchList()
        }
      } else {
        // HTTP error, sync "failed", async "accepted", or
        // unparseable body: keep today's behavior. The re-opened
        // form is where the failure message shows and where the
        // running-status spinner / 3s poll live.
        // Reload the list with the current page/sort/search, then
        // re-open the edit form by id: an action that changes a
        // sort key can move the row off the current page, so the
        // row must not be looked up in the current page's records.
        await fetchList()
        // This re-open keeps terminal statuses: the user just
        // pressed the button and earned the "failed: …" feedback.
        preserveStatusOnOpenRef.current = true
        openEditForm({ id })
      }
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
    // View mode has no submit control; this guard is insurance
    // against future wiring mistakes.
    if (formViewing) return
    // Find file field if present. Rollup types emit no add/update
    // forms (read-only), so the form lookup is guarded.
    const formDef = isEditMode
      ? data!.result['update-form'] || {}
      : data!.result['add-form'] || {}

    // Password confirmation gate: every :widget :password-new field
    // on the form must match its confirmation box before anything
    // is sent (covers both the JSON and file-upload paths below).
    for (const f of formFieldKeys(formDef)) {
      if (formDef[f]['widget'] !== 'password-new') continue
      // An untouched edit-form password arrives from the backend
      // as [] (the JSON form of nil), which is truthy and !== '' —
      // normalize non-strings to '' so blank means "keep current
      // password" (see the matching normalization in the render).
      const pw = typeof formValues[f] === 'string'
        ? formValues[f] : ''
      const confirm = passwordConfirm[f] || ''
      if (pw === '' && confirm === '') continue
      if (pw !== confirm) {
        alert(`Password entries for "${formDef[f].label}" do not match`)
        return
      }
    }

    const fileField = formFieldKeys(formDef)
      .find(f => formDef[f]['widget'] === 'file')
    const fileValue = fileField ? formValues[fileField] : null

    if (fileField && fileValue instanceof File) {
      // Two-phase upload.
      //
      // POST 1: multipart/form-data to /api/upload with all add-form
      // fields, including the file field. The browser sets the
      // multipart boundary automatically (see apiFetch).
      const formData = new FormData()
      formData.append('type', type)
      for (const f of formFieldKeys(formDef)) {
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
      // A form writes only the fields it renders. formValues is
      // seeded from the whole record (openEditForm), so off-form keys
      // (e.g. the [] echo of a NULL timestamp hidden from the form)
      // are stale echoes — drop them. hasOwnProperty, not `k in
      // formDef`: `in` walks the prototype chain, so a field
      // literally named toString or constructor would false-positive.
      // Membership through formFieldKeys keeps the own-property
      // protection while excluding the form-level 'hide-list' key —
      // a seeded formValues['hide-list'] must never ride to the
      // server (POST 1 appends it to FormData directly otherwise).
      // On-form empty arrays (checkbox-list / image-list "clear
      // all") still go out; empty strings stay dropped as today.
      const { roles, [fileField]: _omit, ...rest } = formValues
      const allowedKeys = new Set(formFieldKeys(formDef))
      const filteredRest = Object.fromEntries(
        Object.entries(rest).filter(
          ([k, v]) =>
            allowedKeys.has(k) &&
            (typeof v !== 'string' || v.trim() !== '')
        )
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

    // Normal (no file) path. Same form-membership rule as the
    // file-upload POST 2 above: formValues is seeded from the whole
    // record in edit mode, and off-form keys (e.g. the [] echo of a
    // NULL timestamp the update form does not render) must not ride
    // along on the update — absent keys leave their columns
    // untouched. Membership through formFieldKeys (own-property
    // derived, 'hide-list' excluded — the seeded phantom-field
    // leak). On-form empty arrays (checkbox-list / image-list
    // "clear all") still go out; empty strings stay dropped.
    const { roles, ...rest } = formValues
    const allowedKeys = new Set(formFieldKeys(formDef))
    const filteredRest = Object.fromEntries(
      Object.entries(rest).filter(
        ([k, v]) =>
          allowedKeys.has(k) &&
          (typeof v !== 'string' || v.trim() !== '')
      )
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
      const res = await apiFetch('/api/delete', {
        method: 'POST',
        body: JSON.stringify(payload)
      })
      if (!res.ok) {
        alert(await errorMessage(res, 'Failed to delete'))
        break
      }
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
            ? 'user' : t.category,
          userSetting: typeof t === 'string'
            ? false : !!t['user-setting'],
          // Wire presence → boolean | undefined. Not !!t[...] —
          // that would collapse an explicit false (declared
          // :page-description-open nil) into undefined-ish loss.
          // The === true split preserves declared-false; the
          // legacy-string branch renders no block.
          pageDescription: typeof t === 'string'
            ? undefined : t['page-description'],
          pageDescriptionOpen:
            typeof t === 'string'
            || t['page-description-open'] === undefined
              ? undefined
              : t['page-description-open'] === true
        }))
      setTypes(typeInfos)
      const t = info.result?.['title']
      if (t) setTitle(String(t))
      const lp = info.result?.['landing-page']
      // App state in the URL, Step 3: seed state ← URL. Parse
      // the LIVE hash on every login transition (not once per
      // mount): nothing writes the hash while logged out, so at
      // first login the live hash IS the parked hash; at a
      // relogin it is the last-written hash — exactly the view
      // the previous session showed. A consume-once ref would
      // fail the plan's own relogin test.
      const seed = parseHash()
      const byName = (n: string) =>
        typeInfos.find(
          t => t.name.toLowerCase() === n.toLowerCase())
      const target = seed?.type ? byName(seed.type) : null

      if (seed && seed.type && !target) {
        // Shared link naming a type this user/model does not
        // have (typo, cross-model, stale bookmark, role
        // mismatch — /api/types returns only visible types).
        console.warn('app-state-url: unknown or invisible type',
                     seed.type)
      }

      if (target && seed) {
        setType(target.name)
        setViewMode(
          target.category === 'system' ? 'admin'
          : target.category === 'settings' ? 'settings'
          : 'app')

        // The canonical entry: the raw seed can carry what
        // canonicalization drops (page=1, hide-exclusive off a
        // non-roles type, bool={}, a values-based-empty
        // filters) — assign from the canonical form so absent
        // keys are really absent. target.name folds canonical
        // case in too.
        const c = canonicalize({ ...seed, type: target.name })

        // Full assignment, not present-key merging: every query
        // field gets exactly one write from one source — c.
        // Absent keys assign their defaults; nothing from a
        // previous session survives. The writer effect fires on
        // this commit and canonical-compares against the hash
        // the browser already shows — equal (or one corrected
        // entry for a case-variant type), never a seed entry.
        // Clear the debounce timers BEFORE assigning the pairs:
        // resetListQuery's shape, generalized — a pending
        // commit would otherwise overwrite the seeded query.
        if (searchTimer.current) clearTimeout(searchTimer.current)
        if (notTimer.current) clearTimeout(notTimer.current)
        const q = c.q ?? ''
        setSearchTerm(q); setDebouncedSearch(q)
        const nq = c.notq ?? ''
        setNotTerm(nq); setDebouncedNotTerm(nq)
        setListFilters(c.filters ?? {})
        setBoolFilters(c.bool ?? {})
        setHideExclusive(c.hideExclusive ?? true)
        const sf = c.sort
          ? (c.sort.startsWith('-')
             ? c.sort.slice(1) : c.sort)
          : null
        setSortField(sf)
        setSortDir(
          c.sort && c.sort.startsWith('-') ? 'desc' : 'asc')
        setCurrentPage(c.page ?? 1)
        setSelectedIds([])

        // defaultSortRef is query state (resetListQuery nulls
        // it): a stale name from the previous session's last
        // type would steer the next sort-header click. The
        // fetch echo re-caches it — the only writer allowed.
        defaultSortRef.current = null

        // URL bools are the explicit intent: the link author
        // picked them, so authored defaults must not overwrite
        // them when data arrives. Absent bool = Any: leave the
        // latch null so authored defaults seed as usual — a
        // link that says nothing gets the type's natural view.
        // Checked on c, not seed — raw bool={} is truthy and
        // would suppress the defaults.
        if (c.bool) boolSeededForRef.current = target.name
      } else {
        // No seed, seed with no type at all (#/app?q=x — the q
        // is discarded; a search without a type is not a view,
        // and the writer never produces such an entry), or seed
        // dropped as invalid: existing landing-page resolution,
        // unchanged.
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
      }
    }).catch(() => {
      setTypes([])
    })
  }, [loggedIn])

  useEffect(() => {
    if (!loggedIn || type === '__init__') return
    fetchList()
  }, [loggedIn, type, sortField, sortDir, debouncedSearch,
      debouncedNotTerm, currentPage, listFilters, boolFilters,
      hideExclusive])

  // App state in the URL, Step 2: mirror state → URL. Same deps
  // as the main fetch effect above — the URL mirrors committed
  // state, one history entry per commit (the debounce already
  // collapses keystrokes; sort clicks, chip toggles, and page
  // turns are single commits — do not add extra batching).
  // viewMode is not a dep and not encoded: every view change
  // lands in the same batch as a type change (switchViewMode
  // sets the new category's first type, returnToLanding the
  // landing type), so the type dep covers view switches. The
  // guards keep the login screen and the empty-app shell from
  // writing URLs. Echo/clamp commits write correctly (rollup
  // default sort, clamped page — shared links reproduce the
  // on-screen order); the canonical compare in writeUrl no-ops
  // the agreeing ones.
  useEffect(() => {
    if (!loggedIn || type === '__init__') return
    writeUrl(buildUrlState({
      type, debouncedSearch, debouncedNotTerm,
      listFilters, boolFilters, hideExclusive,
      sortField, sortDir, currentPage
    }))
  }, [loggedIn, type, sortField, sortDir, debouncedSearch,
      debouncedNotTerm, currentPage, listFilters, boolFilters,
      hideExclusive])

  // App state in the URL, Step 2.5: fire the one-shot refetch
  // once data belongs to the type whose first fetch went out
  // without its form-dependent rows (armed in fetchList).
  // Freshness: a dep-triggered effect body is re-created every
  // render — React runs the function from the render whose deps
  // changed, so fetchList here is the one that closed over the
  // data this effect observes (formReady is true on the
  // refetch). Only a persisting callback (the 3s poll's
  // setInterval) needs fetchListRef; calling it here would be
  // harmless but implies a staleness that does not exist — same
  // reason the bool-seeding effect above reads
  // data.result['list-form'] directly.
  useEffect(() => {
    if (!loggedIn) return
    // Keep the type-key compare: it is what stops a same-type
    // commit (data already this type's) from ever arming or
    // firing. Do NOT "simplify" it to !!data.
    if (data?.result?.['type-key'] !== type) return
    if (pendingFormRefetchRef.current !== type) return
    // One-shot by construction: the ref is cleared before the
    // refetch; the refetch's own data arrival re-runs this
    // effect, finds the ref null, and returns. No loop, and
    // data stays out of the main fetch effect's deps.
    // Do NOT null the ref on the type-key bail above: the
    // mismatched run is this effect's normal first run (the
    // departed type's data is still mounted in the commit that
    // armed it); nulling there wipes it in the commit that
    // armed it and the refetch never fires.
    pendingFormRefetchRef.current = null
    fetchList()
  }, [data, type])

  // Seed the boolean selects' authored initial state once per type
  // entry. The ref survives fetches; resetListQuery clears it so
  // the next type entry seeds fresh. This ref is the only thing
  // standing between every list refetch (sort, search, page, the
  // seeding fetch itself) and re-clobbering the user's filters.
  // Must live here with the other effects — an unconditional hook
  // call — never in the post-data-guard render body.
  useEffect(() => {
    // Stale-form guard: on a type change the old data is still
    // mounted when this effect first runs with the new type —
    // seed only from the form that belongs to the requested
    // type, or the ref would be set against the old form and the
    // new type's defaults would never seed.
    if (data?.result?.['type-key'] !== type) return
    if (boolSeededForRef.current === type) return
    boolSeededForRef.current = type
    const defaults = boolDefaultsOf(data?.result?.['list-form'])
    if (Object.keys(defaults).length > 0) {
      setBoolFilters(defaults)
    }
  }, [data, type])

  // App state in the URL, Step 4: the history listener
  // (back / forward). The writer above mirrors state → URL on
  // commit; this mirrors URL → state on history navigation.
  // Both events, one idempotent handler (hash-only navigation
  // is delivered as hashchange in some browsers, popstate in
  // others, both in the rest — listening to only one means Back
  // updates the address bar while the list stays put, on some
  // browser). Own writes re-enter only via hashchange (Safari;
  // pushState never fires popstate), and the equality gate
  // below turns that into a no-op.
  useEffect(() => {
    const onPop = () => {
      if (!loggedIn || type === '__init__') return
      const s = parseHash()
      // Not ours: an out-of-app entry — the pre-login '#', a
      // bare load, an anchor someone else owns (including the
      // title <a href="#"> link on a missed
      // preventDefault — a modifier or middle click, a future
      // edit that drops the handler). IGNORE, deliberately:
      // state is never reset on history nav. Back from
      // #/app?type=todo to '#' leaves the app showing todo
      // while the address bar shows '#' — the exact shape
      // logout already produces. Do NOT "helpfully" reset here:
      // the park-and-restore path lives on state outliving
      // non-app entries.
      if (!s) return
      const byName = (n: string) =>
        types.find(
          t => t.name.toLowerCase() === n.toLowerCase())
      const target = s.type ? byName(s.type) : null
      if (s.type && !target)
        return  // hand-edited hash: leave state alone
              // (the login seed warns; here the URL is
              // already bogus)
      // Case-fold by resolution, BEFORE the gate: byName
      // matches case-insensitively and live state is always
      // canonical, so a case-only edit ('#/app?type=Todo' with
      // state 'todo') changes nothing — without this the gate
      // fails it and the full assignment below closes the form
      // and pushes a corrected entry.
      const entry = target ? { ...s, type: target.name } : s
      // The canonical entry — the gate's left side AND the
      // assignment source for everything below it: the raw
      // parse can carry values canonicalization drops (page=1,
      // hide-exclusive off a non-roles type, bool={}, a
      // values-based-empty filters), and assigning one would
      // ride a canonically-absent value into state with nothing
      // to reconcile it (the writer canonical-compares and
      // no-ops).
      const c = canonicalize(entry)
      // The equality gate — before ANY state is touched. BOTH
      // sides through canonicalize: buildUrlState is the raw
      // mirror (page: 1, empty q / notq, empty filters / bool,
      // null sort, raw hideExclusive), so a bare build on one
      // side compares canonical against raw and misses on every
      // default it carries. Equal → this arrival changes
      // nothing: return before the close, the selection clear,
      // the timer clears, the assignments, the ref writes.
      // Machine-written Back entries are never equal (the
      // writer never pushes an entry identical to the live
      // hash), so no required close is lost. This gate is also
      // what makes own-write hashchange re-entry harmless
      // wherever the browser delivers it.
      if (urlStatesEqual(
            c,
            canonicalize(buildUrlState(
              { type, debouncedSearch, debouncedNotTerm,
                listFilters, boolFilters, hideExclusive,
                sortField, sortDir, currentPage }))))
        return
      if (target) {
        // Derive the view from the type's actual category —
        // exactly the [loggedIn] effect's rule. Entries never
        // carry view; the derivation is the only rule, so a
        // hand-crafted admin type cannot land under the user
        // tab strip.
        setViewMode(
          target.category === 'system' ? 'admin'
          : target.category === 'settings' ? 'settings'
          : 'app')
        if (target.name !== type) setType(target.name)
      }
      // No type in the entry (same-type nav: sort, page, chips,
      // bool): keep both type and view — the entry was written
      // against them.
      // The form and row selection close on every applied entry
      // that changed something (the gate has established this
      // one did): an open edit form is session chrome the
      // history entry did not have — a same-type back from page
      // 2 to page 1 must not leave it mounted over the restored
      // list. Cross-type entries into settings close too — the
      // open form belongs to the departed type's record, and
      // the settings auto-open effect re-opens from the arrived
      // data. A same-type settings entry never reaches here
      // (query-equal, the gate returned; settings types have no
      // list UI, so their entries carry type alone).
      closeForm()
      setSelectedIds([])
      // NOT changeType: changeType → resetListQuery, whose
      // setters land in the same batch and fight the seed
      // assignments below — last write wins on call order
      // inside one handler. Full assignment, absent keys =
      // defaults — same rule as the login seed: every field
      // gets exactly one write, sourced from the canonical
      // entry alone; nothing from the pre-nav state survives.
      // Clear the debounce timers BEFORE assigning the pairs:
      // a Back landing inside the 300 ms window would
      // otherwise leave the timer alive to commit the PRE-NAV
      // trimmed query over the restored one — overwriting both
      // the input and the URL.
      if (searchTimer.current) clearTimeout(searchTimer.current)
      if (notTimer.current) clearTimeout(notTimer.current)
      const q = c.q ?? ''
      setSearchTerm(q); setDebouncedSearch(q)
      const nq = c.notq ?? ''
      setNotTerm(nq); setDebouncedNotTerm(nq)
      setListFilters(c.filters ?? {})
      setBoolFilters(c.bool ?? {})
      setHideExclusive(c.hideExclusive ?? true)
      const sf = c.sort
        ? (c.sort.startsWith('-')
           ? c.sort.slice(1) : c.sort)
        : null
      setSortField(sf)
      setSortDir(c.sort && c.sort.startsWith('-')
                 ? 'desc' : 'asc')
      setCurrentPage(c.page ?? 1)
      // defaultSortRef is query state: a stale name from the
      // previous type would steer the next sort-header click on
      // a same-named column into the toggle branch instead of
      // the clear branch. The fetch echo re-caches it — the
      // only writer allowed.
      defaultSortRef.current = null
      // Latch on EVERY applied entry: URL bools when present
      // (they win by being applied above), otherwise the
      // arrived type's name — never the pre-nav value. Nulling
      // here instead would re-seed authored defaults the entry
      // never had and push an echo entry whose Back-revisit
      // regenerates it — a loop.
      boolSeededForRef.current = target ? target.name : type
    }
    window.addEventListener('popstate', onPop)
    window.addEventListener('hashchange', onPop)
    return () => {
      window.removeEventListener('popstate', onPop)
      window.removeEventListener('hashchange', onPop)
    }
  }, [loggedIn, type, types, sortField, sortDir,
      debouncedSearch, debouncedNotTerm, currentPage,
      listFilters, boolFilters, hideExclusive])

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
          `&form=${openFormName}`
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
    // Auto-open only for :user-setting types (one record per user
    // by nature, e.g. preferences). Multi-record settings types
    // like secrets display as a normal list — even when they
    // happen to hold exactly one row. Read-only users get the
    // view form — an Update button that can only fail is worse
    // than none.
    const info = types.find(t => t.name === type)
    if (!info?.userSetting) return
    if (data.result.records.length > 1) return
    // Three-way auto-open: update-capable → edit; else the view
    // form when the payload declares one; else don't open (the
    // row shows in the list — an Update button that can only
    // fail is worse than none, and view mode without a declared
    // view form has no payload form to render). The edit-vs-view
    // test reads the list record's own per-row flag (base-type
    // degeneration makes it equal to the envelope flag here;
    // the row flag is the honest source the View form's Edit
    // button also gates on).
    if (data.result.records[0]?.update === true) {
      openEditForm(data.result.records[0])
    } else if (data.result['view-form']) {
      openEditForm(data.result.records[0], true)
    }
  }, [data, viewMode, type, types])

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
          <button
            type="submit"
            onMouseEnter={handleLoginButtonHover}
            style={{ width: '100%' }}
          >Login</button>
        </form>
        {guestAllowed && (
          <button
            onClick={handleContinueAsGuest}
            onMouseEnter={handleLoginButtonHover}
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
    // Description block lookup for the "No records" shell (the
    // types state is in scope here; listFields etc. are not yet
    // bound — they belong to the list-carrying render below).
    const shellTypeInfo = types.find(t => t.name === type)
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
          <div style={{ position: 'absolute', right: 0, top: '50%', transform: 'translateY(-50%)', display: 'flex', alignItems: 'center', gap: '0.5rem' }}>
            {/* View switcher: Home/Admin/user (Settings) select
                viewMode (Home via returnToLanding); the username
                button doubles as Settings. Not the type tab row. */}
            <button
              onClick={() => returnToLanding()}
              aria-current={viewMode === 'app' ? 'true' : undefined}
              style={{
                fontSize: '1.1rem',
                fontWeight: viewMode === 'app' ? 'bold' : 'normal',
                border: 'none',
                background: 'transparent',
                borderBottom: viewMode === 'app'
                  ? '3px solid var(--tab-border)'
                  : '3px solid transparent'
              }}
            >
              🏠 Home
            </button>
            {systemTypes.length > 0 && (
              <button
                onClick={() => switchViewMode('admin')}
                aria-current={viewMode === 'admin' ? 'true' : undefined}
                style={{
                  fontSize: '1.1rem',
                  fontWeight: viewMode === 'admin' ? 'bold' : 'normal',
                  border: 'none',
                  background: 'transparent',
                  borderBottom: viewMode === 'admin'
                    ? '3px solid var(--tab-border)'
                    : '3px solid transparent'
                }}
              >
                🔧 Admin
              </button>
            )}
            {settingsTypes.length > 0 ? (
              <button
                onClick={() => switchViewMode('settings')}
                aria-current={viewMode === 'settings' ? 'true' : undefined}
                style={{
                  fontSize: '1.1rem',
                  fontWeight: viewMode === 'settings' ? 'bold' : 'normal',
                  border: 'none',
                  background: 'transparent',
                  borderBottom: viewMode === 'settings'
                    ? '3px solid var(--tab-border)'
                    : '3px solid transparent'
                }}
              >
                👤 {loggedInUser}
              </button>
            ) : (
              <span style={{ fontSize: '1.1rem' }}>{loggedInUser}</span>
            )}
            <button
              onClick={handleLogout}
              style={{
                fontSize: '1.1rem',
                border: 'none',
                background: 'transparent'
              }}
            >
              ⏏ Logout
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
        {/* List-mode shell (empty list / fetch error): the
            description block renders here too — the empty list is
            exactly the page the description exists to orient. The
            !(showAddForm || isEditMode) guard is kept for symmetry
            with the main return; forms never mount in this shell. */}
        {!(showAddForm || isEditMode)
          && shellTypeInfo?.pageDescription && (
          <PageDescription
            text={shellTypeInfo.pageDescription}
            open={pageDescOpen[type]
              ?? shellTypeInfo.pageDescriptionOpen === true}
            onToggle={() =>
              setPageDescOpen(prev => ({
                ...prev,
                [type]: !(prev[type]
                  ?? shellTypeInfo.pageDescriptionOpen === true)
              }))}
          />
        )}
        <p style={{ color: listError ? 'var(--error)' : undefined }}>
          {listError || 'No records'}
        </p>
      </div>
    )
  }

  const listFields = formFieldKeys(data.result['list-form'])
  const records = data.result.records
  // Page-level ✕ gate: any visible row deletable under its own
  // per-record flag (a creator-delete user holds no type-level
  // delete, so the envelope flag would hide the column over
  // deletable rows). Hoisted once — the th and every td read
  // the same const, so header and cells cannot drift on
  // column count. A page of all-non-deletable rows shows no ✕
  // column while a later page might: the accepted lie — the
  // alternative is a type-level OR that reintroduces the lie
  // the per-row gate exists to remove. "Delete Selected"
  // stays on the envelope flag on purpose (type-level
  // question).
  const anyDeletable =
    records.some((r: any) => r.delete === true)
  // Description block lookup; both list-mode render sites (main
  // return + the "No records" early-return shell) consume it.
  const typeInfo = types.find(t => t.name === type)

  // Chip eligibility, both families, via buildChipFieldMeta (the
  // same function fetchList calls — one membership rule): M2M
  // (checkbox-list whose source table differs from the listed
  // type; e.g. users.roles, table "roles" ≠ "users") and
  // reference (:filter-with :reference, table is the :target,
  // column is the target's identity display column). Cell
  // click-to-filter follows membership for both families — the
  // per-family render guards at the cell site own the [] nil
  // encoding of a NULL FK (M2M needs an array, reference a
  // non-empty string). Plain call, not useMemo: this site sits
  // after the "No records" early return, and a conditional hook
  // would change the hook count between renders (React #310). The
  // walk is a cheap object scan over the form — memo bought
  // nothing.
  const chipFieldMeta = buildChipFieldMeta(
    data.result['list-form'] || {}, type)
  const hasChipFields = Object.keys(chipFieldMeta).length > 0
  const hasSearchable =
    Object.values(data.result['list-form'])
      .some(f => f.searchable === true)
  // :filter-with :boolean fields on the list form. Scans the whole
  // form (not listFields — listFields is the rendered column set
  // and can diverge from the form the query covers); a field
  // excluded from :list-form renders no control, intended.
  const boolFilterFields = Object.entries(data.result['list-form'])
    .filter(([, f]) => filterWithKind(f['filter-with']) === 'boolean')
    .map(([fieldKey, f]) => ({ fieldKey, label: f.label }))
  // Authored initial state for the boolean selects, same source
  // (the whole list-form). Empty when the type declares no
  // defaults — Clear filters then restores today's bare-Any
  // behavior (setBoolFilters({}) equivalence). The seeding effect
  // (with the other effects, above) computes its own copy via
  // boolDefaultsOf.
  const boolDefaults = boolDefaultsOf(data.result['list-form'])
  // "Any filter active" is some values array with length > 0, not
  // Object.keys(listFilters).length — clearing the last chip leaves
  // { field: [] } in state, which the key count would still report.
  // Boolean selects count too: they are data filters under the
  // same Clear filters button. Hide exclusive stays out of it.
  const anyFilterActive =
    Object.values(listFilters).some(vals => vals.length > 0)
    || Object.keys(boolFilters).length > 0

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
        <div style={{ position: 'absolute', right: 0, top: '50%', transform: 'translateY(-50%)', display: 'flex', alignItems: 'center', gap: '0.5rem' }}>
          {/* View switcher: Home/Admin/user (Settings) select
              viewMode (Home via returnToLanding); the username
              button doubles as Settings. Not the type tab row. */}
          <button
            onClick={() => returnToLanding()}
            aria-current={viewMode === 'app' ? 'true' : undefined}
            style={{
              fontSize: '1.1rem',
              fontWeight: viewMode === 'app' ? 'bold' : 'normal',
              border: 'none',
              background: 'transparent',
              borderBottom: viewMode === 'app'
                ? '3px solid var(--tab-border)'
                : '3px solid transparent'
            }}
          >
            🏠 Home
          </button>
          {systemTypes.length > 0 && (
            <button
              onClick={() => switchViewMode('admin')}
              aria-current={viewMode === 'admin' ? 'true' : undefined}
              style={{
                fontSize: '1.1rem',
                fontWeight: viewMode === 'admin' ? 'bold' : 'normal',
                border: 'none',
                background: 'transparent',
                borderBottom: viewMode === 'admin'
                  ? '3px solid var(--tab-border)'
                  : '3px solid transparent'
              }}
            >
              🔧 Admin
            </button>
          )}
          {settingsTypes.length > 0 ? (
            <button
              onClick={() => switchViewMode('settings')}
              aria-current={viewMode === 'settings' ? 'true' : undefined}
              style={{
                fontSize: '1.1rem',
                fontWeight: viewMode === 'settings' ? 'bold' : 'normal',
                border: 'none',
                background: 'transparent',
                borderBottom: viewMode === 'settings'
                  ? '3px solid var(--tab-border)'
                  : '3px solid transparent'
              }}
            >
              👤 {loggedInUser}
            </button>
          ) : (
            <span style={{ fontSize: '1.1rem' }}>{loggedInUser}</span>
          )}
          <button
            onClick={handleLogout}
            style={{
              fontSize: '1.1rem',
              border: 'none',
              background: 'transparent'
            }}
          >
            ⏏ Logout
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
          {typeInfo?.pageDescription && (
            <PageDescription
              text={typeInfo.pageDescription}
              open={pageDescOpen[type]
                ?? typeInfo.pageDescriptionOpen === true}
              onToggle={() =>
                setPageDescOpen(prev => ({
                  ...prev,
                  [type]: !(prev[type]
                    ?? typeInfo.pageDescriptionOpen === true)
                }))}
            />
          )}
          {data.result.create && (
            <button onClick={() => {
              setShowAddForm(true)
              setEditRecord(null)
              setFormViewing(false)
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
      {!(showAddForm || isEditMode) &&
        (hasSearchable || hasChipFields || boolFilterFields.length > 0) && (
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
          {/* :filter-with :boolean selects — one per declared
              field, labeled with the field's label. Any / Yes / No;
              absence in boolFilters is Any. Same restart contract
              as chips / search / sort: page 1, selection cleared. */}
          {boolFilterFields.map(({ fieldKey, label }) => (
            <label key={fieldKey} style={{ whiteSpace: 'nowrap' }}>
              <select
                value={boolFilters[fieldKey] || 'any'}
                onChange={e => {
                  const v = e.target.value
                  setBoolFilters(prev => {
                    const next = { ...prev }
                    if (v === 'yes' || v === 'no') {
                      next[fieldKey] = v
                    } else {
                      delete next[fieldKey]
                    }
                    return next
                  })
                  setCurrentPage(1)
                  setSelectedIds([])
                }}
              >
                <option value="any">{label}: Any</option>
                <option value="yes">{label}: Yes</option>
                <option value="no">{label}: No</option>
              </select>
            </label>
          ))}
          {/* Searchable chip pickers, both families: one per
              chip-eligible field, in form order. Lets the user
              name a chip value that is not on the current page; the
              palette is the same allowed-values the edit-form
              CheckboxList / SearchSelect uses. Button hidden on an
              empty palette (nothing to pick — decision 4 does not
              rescue it; allowed-values rides the /api/list
              response and is stable across pages). A URL-carried
              selection against an empty palette still filters —
              the active-chip row below never consults the
              palette. */}
          {Object.keys(chipFieldMeta).map(f => {
            const opts =
              data.result['allowed-values']?.[f] || []
            if (opts.length === 0) return null
            return (
              <ChipPicker
                key={f}
                label={chipFieldMeta[f].label}
                options={opts}
                selected={listFilters[f] || []}
                open={chipPickerOpen === f}
                onOpenChange={o =>
                  setChipPickerOpen(o ? f : null)}
                onToggle={v => toggleChipFilter(f, v)}
              />
            )
          })}
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
                // Restore the authored initial view, not bare Any
                // (with no defaults on the type this is {}, the
                // previous behavior).
                setBoolFilters(boolDefaults)
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
        <form
          className={(() => {
            // Label gutter: render it iff at least one field on
            // the OPEN form draws a label in this mode. Skipped
            // widgets (view mode skips password / file / button /
            // hidden; edit mode skips hidden) never keep the
            // gutter alive, and :hide-label t never does either —
            // :settings' (:fields t) view form is the live case
            // (its only labeled fields are password/button
            // widgets that never draw there). Computed once from
            // the open form's field set, never per row.
            const skippedInMode = isViewMode
              ? new Set(['password-new', 'password-read',
                         'file', 'button', 'hidden'])
              : new Set(['hidden'])
            const formMeta = data.result[openFormName] || {}
            const anyLabelShown = formFieldKeys(formMeta).some(f =>
              formMeta[f]['hide-label'] !== true
              && !skippedInMode.has(formMeta[f]['widget']))
            return anyLabelShown ? '' : 'form-no-labels'
          })()}
          style={{ marginTop: '1rem', marginLeft: '1.5rem' }}
        >
          <h3>{isViewMode ? 'View' : isEditMode ? 'Edit' : 'Add'} {data.result['type-key']}</h3>

          <div style={{ marginBottom: '1rem' }}>
            {!isViewMode && (
              <button type="button" onClick={submitForm}>
                {isEditMode ? 'Update' : 'Submit'}
              </button>
            )}
            {isViewMode && editRecord?.update === true && (
              <button
                type="button"
                onClick={() => openEditForm(editRecord)}
              >
                Edit
              </button>
            )}
            <button
              type="button" onClick={handleCancel}
              style={{ marginLeft: '0.5rem' }}
            >
              {isViewMode ? 'Close' : 'Cancel'}
            </button>
          </div>

          {(formFieldKeys(isEditMode
             ? data.result[openFormName] || {}
             : data.result['add-form'] || {})).map(f => {
            const fieldMeta = isEditMode
              ? (data.result[openFormName] || {})[f]
              : (data.result['add-form'] || {})[f]
            const allowed = data.result['allowed-values']?.[f] || []
            // View mode renders every field read-only; password,
            // file, and button fields are skipped first — nothing
            // about them is displayable (a view-mode status
            // companion renders through its own read-only
            // branch).
            if (isViewMode) {
              const w = fieldMeta['widget'] || ''
              if (w === 'password-new' || w === 'password-read'
                  || w === 'file' || w === 'button'
                  || w === 'hidden') {
                return null
              }
              return (
                <FieldRow key={f} field={fieldMeta}>
                  {renderReadOnlyField(
                    fieldMeta, formValues[f]
                  )}
                </FieldRow>
              )
            }
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
                <FieldRow key={f} field={fieldMeta}>
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
                </FieldRow>
              )
            }

            if (isCheckbox) {
              // Label moves to the left column like every other
              // field; the value column holds the bare checkbox
              // (the old inline "☑ Label" put this field's label
              // in a different place than every other widget's).
              const checked = !!formValues[f]
              return (
                <FieldRow key={f} field={fieldMeta}>
                  <input
                    type="checkbox"
                    checked={checked}
                    onChange={e => setFormValues({ ...formValues, [f]: e.target.checked })}
                  />
                </FieldRow>
              )
            }

            if (fieldMeta['widget'] === 'select') {
              return (
                <FieldRow key={f} field={fieldMeta}>
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
                </FieldRow>
              )
            }

            if (fieldMeta['widget'] === 'search-select') {
              // Searchable single-select over the same
              // allowed-values palette :select consumes. ✕ (clear
              // to null) only on a nullable field — fe-fields
              // emits :not-null as a JSON boolean; absent means
              // clearable (older payloads / synthesized fields).
              // The branch is load-bearing for the clear path:
              // renderFormField's onChange is (v: string) => void,
              // so null must go through here, not the fallback.
              return (
                <FieldRow key={f} field={fieldMeta}>
                  <SearchSelect
                    options={allowed}
                    value={formValues[f] ?? null}
                    clearable={fieldMeta['not-null'] !== true}
                    onChange={v =>
                      setFormValues({ ...formValues, [f]: v })
                    }
                  />
                </FieldRow>
              )
            }

            if (fieldMeta['widget'] === 'file') {
              return (
                <FieldRow key={f} field={fieldMeta}>
                  <input
                    type="file"
                    onChange={e => {
                      const file = e.target.files?.[0] || null
                      setFormValues({ ...formValues, [f]: file })
                    }}
                  />
                </FieldRow>
              )
            }

            if (fieldMeta['widget'] === 'file-path') {
              // Folder select + editable filename. Lives beside
              // the file branch (not inside renderFormField):
              // the suggested filename comes from the sibling
              // :widget :file field's File object, which only
              // this form map can see. At most one file field
              // exists today (submitForm's find makes the same
              // assumption).
              const fileFieldKey = formFieldKeys(
                isEditMode
                  ? (data.result[openFormName] || {})
                  : (data.result['add-form'] || {})
              ).find(k => {
                const meta = isEditMode
                  ? (data.result[openFormName] || {})[k]
                  : (data.result['add-form'] || {})[k]
                return meta?.['widget'] === 'file'
              })
              const siblingFile = fileFieldKey
                ? formValues[fileFieldKey] : null
              const suggested = siblingFile instanceof File
                // File.name is a basename per spec; strip any
                // path anyway ( defenses for browsers that
                // include one)
                ? siblingFile.name.split('/').pop()
                  || siblingFile.name
                : ''
              return (
                <FieldRow key={f} field={fieldMeta}>
                  <FilePath
                    directoryType={
                      fieldMeta['directory-type'] || ''}
                    value={
                      typeof formValues[f] === 'string'
                        ? formValues[f] : ''}
                    onChange={v =>
                      setFormValues({ ...formValues, [f]: v })
                    }
                    suggestedName={suggested}
                  />
                </FieldRow>
              )
            }

            if (fieldMeta['widget'] === 'password-new') {
              // Same [] normalization as the submit gate: an
              // untouched edit-form password arrives as [] (JSON
              // nil) and must read as blank, not as a mismatch.
              const pw = typeof formValues[f] === 'string'
                ? formValues[f] : ''
              const confirm = passwordConfirm[f] || ''
              const touched =
                pw !== '' || confirm !== ''
              const mismatched =
                touched && pw !== confirm
              const matchHint = !touched ? '' : mismatched
                ? ' — entries do not match'
                : ' — entries match'
              return (
                <FieldRow key={f} field={fieldMeta}>
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
                </FieldRow>
              )
            }

            if (fieldMeta['widget'] === 'password-read') {
              // Collect an existing secret (e.g. settings
              // Current Password for :change-password): one
              // masked box, no confirm, no blank-keeps placeholder,
              // autoComplete="current-password" so browsers offer
              // saved credentials, not generation.
              return (
                <FieldRow key={f} field={fieldMeta}>
                  <input
                    type="password"
                    autoComplete="current-password"
                    value={typeof formValues[f] === 'string'
                      ? formValues[f] : ''}
                    onChange={e =>
                      setFormValues({ ...formValues, [f]: e.target.value })
                    }
                  />
                </FieldRow>
              )
            }

            if (fieldMeta['widget'] === 'hidden') {
              return null
            }

            if (fieldMeta['read-only'] === true) {
              return (
                <FieldRow key={f} field={fieldMeta}>
                  {renderReadOnlyField(
                    fieldMeta, formValues[f]
                  )}
                </FieldRow>
              )
            }

            if (fieldMeta['widget'] === 'textarea'
                || fieldMeta['widget'] === 'markdown') {
              // markdown edits through the same plain textarea —
              // no preview pane, no editor (View mode is the
              // reading surface).
              return (
                <FieldRow key={f} field={fieldMeta}>
                  <textarea
                    value={formValues[f] || ''}
                    onChange={e =>
                      setFormValues({ ...formValues, [f]: e.target.value })
                    }
                    rows={8}
                    style={{ width: '100%', resize: 'vertical' }}
                  />
                </FieldRow>
              )
            }

            if (fieldMeta['widget'] === 'code') {
              return (
                <FieldRow key={f} field={fieldMeta}>
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
                </FieldRow>
              )
            }

            if (fieldMeta['widget'] === 'button') {
              const statusKey = `${f}-status`
              const statusVal = editRecord?.[statusKey] || ''
              const isRunning = statusVal.startsWith('running')
              const isPending = pendingActions.has(f)
              // State gate (:enabled-when): the shared evaluator
              // against the open record; disabled is UX, the
              // backend rejects independently.
              const isGatedOff = !evalEnabledWhen(
                fieldMeta['enabled-when'], editRecord)
              return (
                <FieldRow key={f} field={fieldMeta}>
                  <button
                    type="button"
                    disabled={isRunning || isPending || isGatedOff}
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
                </FieldRow>
              )
            }

            return (
              <FieldRow key={f} field={fieldMeta}>
                {renderFormField(fieldMeta, formValues[f], v =>
                  setFormValues({ ...formValues, [f]: v })
                )}
              </FieldRow>
            )
          })}

          {!isViewMode && (
            <>
              <button type="button" onClick={submitForm}>
                {isEditMode ? 'Update' : 'Submit'}
              </button>
              <button type="button" onClick={handleCancel} style={{ marginLeft: '0.5rem' }}>
                Cancel
              </button>
            </>
          )}
          {isViewMode && (
            <button type="button" onClick={handleCancel}>
              Close
            </button>
          )}
        </form>
      )}

      {!hideList && (
      <table>
        <thead>
          <tr>
            {anyDeletable && (
              <th style={{ width: '40px', textAlign: 'center', color: 'var(--error-bright)' }}>✕</th>
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
            <tr
              key={idx}
              className={hasViewForm
                ? 'row-clickable' : undefined}
              tabIndex={hasViewForm ? 0 : undefined}
              role={hasViewForm ? 'button' : undefined}
              aria-label={hasViewForm
                ? `Open ${type} record ${rec.id}` : undefined}
              onClick={hasViewForm
                ? e => handleRowClick(e, rec) : undefined}
              onKeyDown={hasViewForm
                ? e => handleRowKeyDown(e, rec) : undefined}
            >
              {anyDeletable && (
                <td style={{ textAlign: 'center' }}>
                  {(rec as RecordCaps).delete === true && (
                    <input
                      type="checkbox"
                      checked={selectedIds.includes(rec.id)}
                      onChange={() => toggleSelect(rec.id)}
                    />
                  )}
                </td>
              )}
              {listFields.map(f => {
                const field = data.result['list-form'][f]
                // Global one-line rule: text cells clamp to one
                // line with an ellipsis; the full value rides
                // along as the hover tooltip. Media cells and
                // chip cells (M2M arrays, reference scalars)
                // keep their own layout (chips wrap).
                const widget = field['widget'] || ''
                const isMedia =
                  widget === 'image' || widget === 'image-list'
                // Chip membership is the shared chipFieldMeta
                // rule (the picker's own), tested as an own
                // property — the object literal's prototype
                // would otherwise hand a non-chip field named
                // constructor / toString inherited metadata
                // through `in`. Membership gates the render
                // branches below; the clamp follows what
                // actually renders (per-family guards: M2M
                // needs an array — an empty array renders the
                // empty chip-cell span, as today; reference
                // needs a non-empty string — NULL FKs arrive
                // as the [] nil encoding and fall through,
                // clamped). A future chip family without its
                // own branch here renders via renderCellValue,
                // clamped — safe.
                const meta =
                  Object.prototype.hasOwnProperty.call(
                    chipFieldMeta, f)
                  ? chipFieldMeta[f] : undefined
                // Reference scalar chip value, computed in the
                // component body (JSX children cannot hold a
                // bare IIFE — only expressions in {}).
                const refVal = meta && !meta.m2m
                  && typeof rec[f] === 'string' && rec[f] !== ''
                    ? rec[f] : null
                // Clamp exemption couples to the render
                // branches, not membership alone.
                const chipRendered =
                  (meta && meta.m2m
                    && Array.isArray(rec[f])) || refVal !== null
                const clamp = !(isMedia || chipRendered)
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
                    {meta && meta.m2m && Array.isArray(rec[f])
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
                      : meta && !meta.m2m && refVal !== null
                      ? (
                        <span className="chip-cell">
                          <button
                            type="button"
                            className={(listFilters[f] || [])
                              .includes(refVal)
                              ? 'chip chip-active' : 'chip'}
                            title={(listFilters[f] || [])
                              .includes(refVal)
                              ? `Remove filter: ${refVal}`
                              : `Filter by ${refVal}`}
                            onClick={() =>
                              toggleChipFilter(f, refVal)}
                          >{refVal}</button>
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
      )}

      {!hideList && (
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
      )}
    </div>
  )
}

export default App