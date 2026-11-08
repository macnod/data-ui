import { useEffect, useRef, useState, Fragment }
  from 'react'
import { apiFetch, setTokens, clearTokens, getAccessToken,
         onAuthFailure, onTokenRefreshed, restoreSession }
  from './api'
import StarRating from './StarRating'
import CheckboxList from './CheckboxList'
import ChipPicker from './ChipPicker'
import FilePath from './FilePath'
import SearchSelect from './SearchSelect'
import { parseHash, canonicalize, buildUrlState,
         urlStatesEqual, writeUrl, parseScope, scopeToParam,
         SCOPE_NONE } from './urlState'
import type { RecordMode, ScopeState } from './urlState'
import { hashPermalink, bestLinkUrl } from './publicLink'
import { Markdown } from './Markdown'
import { PageDescription } from './PageDescription'
import { FieldHelp } from './FieldHelp'
import { composeReply, normalizeRoles, lastOffset,
         regionPayloadData, insertOutcome, separateQuotes }
  from './region'
import type { InsertOutcome } from './region'

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
  // :target wire value (fe-fields, singular-target fields only):
  // the field's actual :target type key, downcased string. The
  // View-form FK link gate — emitted on suppressed shapes too
  // (the sibling id's absence withholds the link, not this key).
  target?: string
}

// The shared Add/Reply field renderer for the committed v1
// region alphabet (plan 03): hidden (nothing), read-only, and
// markdown/textarea, falling back to renderFormField (stars and
// textbox/default). Both the region's panel map and any future
// shared consumer call this; the parent's own form map keeps
// its select/code/etc. branches (the region alphabet does not
// widen without a plan). A field outside the alphabet renders
// nothing — *widgets* is untouched and the fallback logs.
function renderRegionField(
  field: Field, value: any,
  onChange: (v: string) => void
): React.ReactNode {
  const widget = field['widget'] || ''
  if (widget === 'hidden') return null
  if (field['read-only'] === true) {
    return renderReadOnlyField(field, value)
  }
  if (widget === 'markdown' || widget === 'textarea') {
    return (
      <textarea
        value={value || ''}
        onChange={e => onChange(e.target.value)}
        rows={8}
        style={{ width: '100%', resize: 'vertical' }}
      />
    )
  }
  return renderFormField(field, value, onChange)
}

// The region: a slim, region-local reading surface under the
// parent's view form (plan 03 — "Region renderer"). One
// component owns its fetch (/api/list on the child type,
// form=view-form, the fixed fk-eq filter, the region's :sort),
// its pager, accordion expansion, and the Add/Reply panel. It
// never touches App state: no setData, no formValues, no query
// setters, no router — the child payload (rows, forms, create
// flag) lives in region-local state. Requests are fenced by a
// region sequence + mounted flag (a superseded or unmounted
// response assigns nothing); the offset state is the sole
// effect-driven display-fetch trigger (pager handlers and
// insert/shrink handoffs synchronously invalidate old work and
// set the target offset; the effect allocates the next request).
function Region({ spec, parentId, onShowAll }: {
  spec: ChildSpec
  parentId: string
  // The escape hatch (plan 04): the Region owns the footer
  // link and its current total; App owns the callback and all
  // navigation / state writes (the Region never touches the
  // query setters or the router). Optional — an absent
  // callback renders no link.
  onShowAll?: (spec: ChildSpec, parentId: string) => void
}) {
  // Child payload + status flags — never an inference from
  // missing rows.
  const [region, setRegion] = useState<ListResponse | null>(
    null)
  const [inFlight, setInFlight] = useState(true)
  const [failed, setFailed] = useState(false)
  const [reloadTick, setReloadTick] = useState(0)
  // Offset state: the sole effect-driven display-fetch trigger.
  const [offset, setOffset] = useState(0)
  // Expansion / Reply.
  const [expandedId, setExpandedId] = useState<string | null>(
    null)
  const [panelOpen, setPanelOpen] = useState(false)
  const [panelValues, setPanelValues] =
    useState<Record<string, any>>({})
  const [panelError, setPanelError] = useState('')
  const [pending, setPending] = useState(false)
  // Request fencing: one counter advancing on invalidation and
  // request allocation; a response commits only when its
  // captured sequence is still the latest AND the component is
  // still mounted. Cleanup increments the sequence and clears
  // the mounted flag, so an old instance's response (a parent
  // swap remounts the component) can never assign.
  const seqRef = useRef(0)
  const mountedRef = useRef(true)
  // The Add/Reply panel anchor: the panel renders once at the
  // region's bottom (after the table and the pager), so on a
  // long list a freshly opened panel can sit below the fold
  // and its button (Reply, Add) looks dead.
  const panelRef = useRef<HTMLDivElement>(null)
  useEffect(() => {
    mountedRef.current = true
    return () => {
      mountedRef.current = false
      ++seqRef.current
    }
  }, [])
  const isCurrentList = (seq: number) =>
    mountedRef.current && seqRef.current === seq

  // Request layer: allocates a sequence, builds and sends the
  // region's own /api/list request (the child type, the
  // view-form hydration, the fixed fk-eq row, the region's
  // :sort, PAGE_SIZE, the given offset), and returns
  // { seq, json } on success or throws with the sequence
  // attached. Never assigns region state — every commit goes
  // through loadRegion's fence or an explicit isCurrentList
  // check at the commit site.
  const requestRegionList = async (reqOffset: number) => {
    const seq = ++seqRef.current
    const rows = [[spec.type, spec.via, 'fk-eq', parentId]]
    const url = `/api/list?type=${
      encodeURIComponent(spec.type)}&form=view-form`
      + `&limit=${PAGE_SIZE}&offset=${reqOffset}`
      + `&sort=${spec.sort[0]}:${spec.sort[1]}`
      + `&filters=${encodeURIComponent(JSON.stringify(rows))}`
    try {
      const res = await apiFetch(url)
      if (!res.ok) {
        throw new Error(`Request failed (${res.status})`)
      }
      const json: ListResponse = await res.json()
      return { seq, json }
    } catch (err: any) {
      ;(err as any).__seq = seq
      throw err
    }
  }

  // Success-commit path shared by loadRegion and the manual
  // Reload: shrink-aware (an out-of-range page returning zero
  // rows with total > 0 is a shrink — invalidate that response
  // without committing it, set the computed last offset, and
  // let the offset effect own the single clamped-page fetch;
  // total === 0 is handled before the arithmetic). total === 0
  // with create false renders nothing at all (the caller's
  // empty-state rules), so the commit itself only carries the
  // envelope.
  const commitRegion = (seq: number, json: ListResponse) => {
    if (!isCurrentList(seq)) return
    const total = json?.result?.total ?? 0
    const records = json?.result?.records || []
    if (total > 0 && records.length === 0) {
      // Shrink: do not commit the out-of-range page. A
      // zero-row response with total > 0 necessarily has an
      // offending offset greater than the computed last offset,
      // so the state change below cannot be a same-offset no-op.
      const target = lastOffset(total, PAGE_SIZE)
      ++seqRef.current
      setOffset(target)
      return
    }
    setRegion(json)
    setFailed(false)
    setInFlight(false)
  }

  // Display-fetch loader: sets in-flight, owns success / error
  // commits through the fence.
  const loadRegion = async (reqOffset: number) => {
    setInFlight(true)
    try {
      const { seq, json } = await requestRegionList(reqOffset)
      commitRegion(seq, json)
    } catch (err: any) {
      if (!isCurrentList((err as any).__seq ?? -1)) return
      console.error('region: failed to load',
        spec.type, err)
      setRegion(null)
      setFailed(true)
      setInFlight(false)
    }
  }

  // The one offset effect: the sole effect-driven
  // display-fetch trigger. Mount, pager, shrink, and post-insert
  // handoffs all change offset (or the reload tick); nothing
  // else reloads.
  useEffect(() => {
    loadRegion(offset)
  }, [offset, reloadTick])

  // Scroll a freshly opened Add/Reply panel into view. An
  // effect (not openPanel) owns the scroll so every open path
  // is covered once; 'nearest' makes it a no-op when the panel
  // is already on screen. Closing scrolls nothing — the panel
  // unmounts with it.
  useEffect(() => {
    if (!panelOpen) return
    panelRef.current?.scrollIntoView(
      { behavior: 'smooth', block: 'nearest' })
    // Reply preset: focus the body textarea with the caret at
    // the end — below the quote and its trailing blank line —
    // so the first typed character starts a fresh top-level
    // paragraph. Without this the user must know where typing
    // is safe: a caret placed on (or a click into) the quote's
    // last line lazy-joins the quote (CommonMark continuation).
    // Guarded on the body preset, so the plain Add panel is
    // untouched; generic add-forms with several textareas take
    // the first (the preset path is the comment body).
    // openPanel batches values+open, so the preset is already
    // in panelValues when this runs.
    if (panelValues.body !== undefined) {
      const ta =
        panelRef.current?.querySelector('textarea')
      if (ta) {
        ta.focus()
        const end = ta.value.length
        ta.setSelectionRange(end, end)
      }
    }
  }, [panelOpen])

  // Manual Reload: reads the latest current offset at invocation
  // (never a stale closure), issues a fenced probe, and commits
  // it through the ordinary success/clamp path; it does not
  // change offset merely to force a reload.
  const handleReload = async () => {
    const cur = offset
    setInFlight(true)
    try {
      const { seq, json } = await requestRegionList(cur)
      commitRegion(seq, json)
      // Expansion retention: a same-offset Reload keeps the
      // expanded row open only when its id is still on the
      // refreshed page; otherwise close it and its Reply.
      const records = json?.result?.records || []
      if (expandedId != null
          && !records.some((r: any) => r.id === expandedId)) {
        setExpandedId(null)
        setPanelOpen(false)
        setPanelValues({})
      }
    } catch (err: any) {
      if (!isCurrentList((err as any).__seq ?? -1)) return
      console.error('region: reload failed', spec.type, err)
      setRegion(null)
      setFailed(true)
      setInFlight(false)
    }
  }

  // Pager handlers: synchronously invalidate older list work,
  // close expansion / Reply, and set the target offset — the
  // offset effect allocates the next request sequence.
  const pagePrev = () => {
    ++seqRef.current
    setExpandedId(null)
    setPanelOpen(false)
    setPanelValues({})
    setOffset(Math.max(0, offset - PAGE_SIZE))
  }
  const pageNext = () => {
    ++seqRef.current
    setExpandedId(null)
    setPanelOpen(false)
    setPanelValues({})
    setOffset(offset + PAGE_SIZE)
  }

  // The Add/Reply payload roles: the child's type-roles,
  // normalized (each string to itself, each array to element
  // zero) — never verbatim from type-roles (a nested entry
  // would make the backend's role lookup treat permission names
  // as role names). Omitted roles means private, so the region
  // sends the normalized array only when it is nonempty.
  const create = region?.result?.create === true
  const viewForm = region?.result?.['view-form']
  const addForm = region?.result?.['add-form']
  const records = region?.result?.records || []
  const total = region?.result?.total ?? 0
  const roles = normalizeRoles(
    region?.result?.['type-roles'] || [])

  const openPanel = (presetBody?: string) => {
    // Sparse init: only actual presets — the hidden FK (the
    // parent's uuid, never the title; resolve-reference-id
    // short-circuits on uuid-p so it binds directly) and
    // Reply's body. An untouched user stays undefined so the
    // fixture's :autofill :user can supply the current
    // username. The preset must actually be sent in the JSON
    // body: full-data fills any missing field with its model
    // default, so an omitted via becomes the default, not the
    // parent.
    const values: Record<string, any> = { [spec.via]: parentId }
    if (presetBody !== undefined) values.body = presetBody
    setPanelValues(values)
    setPanelError('')
    setPanelOpen(true)
  }

  // The insert: POST /api/insert from region-local state. Never
  // submitForm (closeForm on a :hide-list t view form routes
  // through returnToLanding and strands the post). On a
  // confirmed new insertion: close the panel, clear
  // expansion/Reply, probe the fresh total without committing
  // its rows, and hand off to the last-page offset (the effect
  // makes the only display fetch; exactly one response owns the
  // final rows commit). inserted:false is the idempotent no-op:
  // close, clear pending, same-page Reload. A failed insert
  // keeps the panel, the body, and the page.
  const submitPanel = async () => {
    if (pending) return
    if (!addForm) return
    setPending(true)
    setPanelError('')
    const payload: any = {
      type: spec.type,
      data: regionPayloadData(
        formFieldKeys(addForm), spec.via,
        panelValues.body !== undefined
          ? { ...panelValues,
              body: separateQuotes(String(panelValues.body)) }
          : panelValues)
    }
    if (roles.length > 0) payload.roles = roles
    try {
      const res = await apiFetch('/api/insert', {
        method: 'POST',
        body: JSON.stringify(payload)
      })
      let body: any = null
      try { body = await res.json() } catch { /* invalid */ }
      const outcome: InsertOutcome =
        insertOutcome(res.ok, body)
      if (outcome === 'invalid') {
        // Server message when present, neutral otherwise; the
        // edited body and the open panel survive.
        setPanelError(body?.error || 'Failed to post')
        setPending(false)
        return
      }
      // Close the panel and clear expansion/Reply on every
      // successful outcome (new insertion and no-op alike).
      setPanelOpen(false)
      setPanelValues({})
      setExpandedId(null)
      setPending(false)
      if (outcome === 'noop') {
        // Same-page Reload — no last-page jump, no new-row
        // claim.
        setReloadTick(t => t + 1)
        return
      }
      // Confirmed new insertion: probe the fresh total (same
      // offset), then either set the last-page offset (the
      // effect makes the only display fetch) or commit the
      // probe as the display response under its own fence.
      try {
        const { seq, json } =
          await requestRegionList(offset)
        const freshTotal = json?.result?.total ?? 0
        const target = lastOffset(freshTotal, PAGE_SIZE)
        if (target !== offset) {
          // Synchronously invalidate the probe (its rows are
          // never committed) and set the target offset.
          ++seqRef.current
          setOffset(target)
        } else {
          commitRegion(seq, json)
        }
      } catch (err: any) {
        // A failed probe after a confirmed insertion is a load
        // failure, not an insert failure: show the region load
        // error with its Reload control; never retry the POST
        // or restore the submitted body. The server already
        // inserted the row.
        if (!isCurrentList((err as any).__seq ?? -1)) return
        console.error('region: post-insert probe failed',
          spec.type, err)
        setRegion(null)
        setFailed(true)
        setInFlight(false)
      }
    } catch (err: any) {
      // Network exception: local error line, panel and body
      // preserved. (Expired auth goes through apiFetch's global
      // auth-failure callback instead — the existing logout
      // path, not a local promise.)
      setPanelError(err?.message || 'Failed to post')
      setPending(false)
    }
  }

  // Early render: nothing but the container while the first
  // fetch is in flight (neither the empty conclusion nor the
  // error).
  if (inFlight && region == null && !failed) {
    return <div style={{ marginTop: '1rem',
                         marginLeft: '1.5rem' }} />
  }
  if (failed) {
    return (
      <div style={{ marginTop: '1rem', marginLeft: '1.5rem' }}>
        <p style={{ color: 'var(--error)', margin: '0.5rem 0' }}>
          Failed to load {spec.type}.
        </p>
        <button type="button" onClick={handleReload}>
          Reload
        </button>
      </div>
    )
  }
  // Loaded-empty, no create permission: render nothing (no
  // header, no prose).
  if (region == null || total === 0) {
    if (!create) return null
    // Loaded-empty with create: the Add control is the empty
    // state (region-side defensive gating still requires the
    // add-form before rendering the write surface).
    if (!addForm) return null
    return (
      <div style={{ marginTop: '1rem', marginLeft: '1.5rem' }}>
        <button type="button" onClick={() => openPanel()}>
          Add {spec.type}
        </button>
        {panelOpen && (
          <div ref={panelRef}>
            <RegionPanel
              addForm={addForm}
              spec={spec}
              values={panelValues}
              error={panelError}
              pending={pending}
              onChange={(k, v) => setPanelValues(
                { ...panelValues, [k]: v })}
              onSubmit={submitPanel}
              onCancel={() => {
                setPanelOpen(false)
                setPanelValues({})
                setPanelError('')
              }}
            />
          </div>
        )}
      </div>
    )
  }
  if (!viewForm) return null

  const viewFieldKeys = formFieldKeys(viewForm)
  const totalPages = Math.max(1,
    Math.ceil(total / PAGE_SIZE))
  const page = Math.floor(offset / PAGE_SIZE) + 1

  return (
    <div style={{ marginTop: '1rem', marginLeft: '1.5rem' }}>
      <div style={{
        marginBottom: '0.25rem',
        display: 'flex',
        alignItems: 'center',
        gap: '0.5rem'
      }}>
        <h4 style={{ margin: 0 }}>
          {spec.type} ({total})
        </h4>
        <button type="button" onClick={handleReload}>
          Reload
        </button>
        {create && addForm && (
          <button type="button"
            onClick={() => openPanel()}>
            Add
          </button>
        )}
      </div>
      <table>
        <thead>
          <tr>
            {viewFieldKeys.map(f => (
              <th key={f}>{viewForm[f].label}</th>
            ))}
          </tr>
        </thead>
        <tbody>
          {records.map((rec: any) => (
            <Fragment key={rec.id}>
              <tr
                className="row-clickable"
                tabIndex={0}
                role="button"
                aria-label={expandedId === rec.id
                  ? `Collapse ${spec.type} record ${rec.id}`
                  : `Expand ${spec.type} record ${rec.id}`}
                onClick={e => {
                  if (window.getSelection()?.toString())
                    return
                  const t = e.target as HTMLElement | null
                  if (t?.closest(INTERACTIVE_SELECTOR))
                    return
                  if (expandedId === rec.id) {
                    // Collapsing a row also closes its Reply.
                    setExpandedId(null)
                    setPanelOpen(false)
                    setPanelValues({})
                  } else {
                    setExpandedId(rec.id)
                  }
                }}
                onKeyDown={e => {
                  if (e.key !== 'Enter') return
                  const t = e.target as HTMLElement | null
                  if (t?.closest(INTERACTIVE_SELECTOR))
                    return
                  if (expandedId === rec.id) {
                    setExpandedId(null)
                    setPanelOpen(false)
                    setPanelValues({})
                  } else {
                    setExpandedId(rec.id)
                  }
                }}
              >
                {viewFieldKeys.map(f => {
                  const field = viewForm[f]
                  const text = cellText(rec[f], field)
                  return (
                    <td
                      key={f}
                      style={{
                        maxWidth: '22rem',
                        overflow: 'hidden',
                        textOverflow: 'ellipsis',
                        whiteSpace: 'nowrap'
                      }}
                      title={text || undefined}
                    >
                      {renderCellValue(rec[f], field)}
                    </td>
                  )
                })}
              </tr>
              {expandedId === rec.id && (
                <tr>
                  <td
                    colSpan={viewFieldKeys.length}
                    style={{ padding: '0.5rem 0.25rem' }}
                  >
                    {viewFieldKeys.map(f => (
                      <FieldRow key={f} field={viewForm[f]}>
                        {renderReadOnlyField(
                          viewForm[f], rec[f])}
                      </FieldRow>
                    ))}
                    <div style={{
                      marginTop: '0.5rem',
                      display: 'flex',
                      gap: '0.75rem',
                      alignItems: 'center'
                    }}>
                      {create && addForm && (
                        <button
                          type="button"
                          onClick={() =>
                            openPanel(composeReply(
                              rec.user,
                              String(rec.body ?? '')))}
                        >
                          Reply
                        </button>
                      )}
                      {/* Review fix 2026-11-03: the record
                          permalink hop. A plain hash anchor —
                          the href is byte-identical to the
                          Best Link URL the child's own view
                          form manufactures, so the click rides
                          the existing history listener (the
                          same path as a pasted permalink):
                          atomic tab/type switch, strict view-
                          form open, Back returns to the post.
                          Native <a> behaviors come free
                          (middle-click new tab, copy link
                          address); 'a' is in
                          INTERACTIVE_SELECTOR, so the click
                          never toggles the row expansion. No
                          scope: the list under the opened form
                          is the child type's full list (the
                          footer "All N »" link is the scoped
                          hop). */}
                      <a
                        href={`#/app?type=${
                          encodeURIComponent(spec.type)}&id=${
                          encodeURIComponent(rec.id)}`}
                        style={{
                          color: 'var(--link)',
                          textDecoration: 'underline'
                        }}
                      >
                        Open »
                      </a>
                    </div>
                  </td>
                </tr>
              )}
            </Fragment>
          ))}
        </tbody>
      </table>
      <div className="list-footer">
        {totalPages > 1 && (
          <div className="pagination">
            <button type="button" onClick={pagePrev}
              disabled={page <= 1}>
              ‹ Prev
            </button>
            <span>Page {page} of {totalPages}</span>
            <button type="button" onClick={pageNext}
              disabled={page >= totalPages}>
              Next ›
            </button>
          </div>
        )}
        <span className="record-count">
          {total} records
        </span>
        {/* Footer link (plan 04): the hop to the scoped tab.
            N is the current successful response's total —
            hidden while a replacement fetch is in flight and
            after a failed fetch (never a stale total), and
            absent in the render-nothing empty/no-create state
            by construction (that path returns early above). A
            button styled as a link: neither the Region nor
            the callback writes the hash. */}
        {onShowAll != null && !inFlight && !failed && (
          <button
            type="button"
            onClick={() => onShowAll(spec, parentId)}
            style={{
              background: 'transparent',
              border: 'none',
              padding: 0,
              color: 'var(--link)',
              textDecoration: 'underline',
              cursor: 'pointer',
              font: 'inherit'
            }}
          >
            All {total} {spec.type} »
          </button>
        )}
      </div>
      {panelOpen && (
        <div ref={panelRef}>
          <RegionPanel
            addForm={addForm!}
            spec={spec}
            values={panelValues}
            error={panelError}
            pending={pending}
            onChange={(k, v) =>
              setPanelValues({ ...panelValues, [k]: v })}
            onSubmit={submitPanel}
            onCancel={() => {
              setPanelOpen(false)
              setPanelValues({})
              setPanelError('')
            }}
          />
        </div>
      )}
    </div>
  )
}

// The Add/Reply panel: a plain <div> with type="button"
// controls and an explicit Submit handler — never a nested
// <form> (Enter in a text input must not trigger the parent
// view form's implicit submission). Skips the preset via field
// (the FK) and the synthetic :roles field in the visible map;
// the preset itself rides the payload unconditionally
// (regionPayloadData).
function RegionPanel({
  addForm, spec, values, error, pending,
  onChange, onSubmit, onCancel
}: {
  addForm: Record<string, Field>
  spec: ChildSpec
  values: Record<string, any>
  error: string
  pending: boolean
  onChange: (key: string, value: any) => void
  onSubmit: () => void
  onCancel: () => void
}) {
  return (
    <div style={{
      marginTop: '0.75rem',
      marginBottom: '0.75rem',
      padding: '0.75rem',
      border: '1px solid var(--border)',
      borderRadius: '4px',
      background: 'var(--tab-inactive-bg)'
    }}>
      <div style={{
        marginBottom: '0.5rem',
        display: 'flex',
        gap: '0.5rem'
      }}>
        <button
          type="button" onClick={onSubmit}
          disabled={pending || !addForm}
        >
          {pending ? 'Posting…' : 'Submit'}
        </button>
        <button type="button" onClick={onCancel}>
          Cancel
        </button>
      </div>
      {error && (
        <p style={{
          color: 'var(--error)', margin: '0.5rem 0'
        }}>
          {error}
        </p>
      )}
      {formFieldKeys(addForm)
        // The synthetic :roles checkbox-list (fe-fields'
        // show-roles-p injects it into every non-base
        // add-form) is payload-owned, not panel-rendered: the
        // region sends roles top-level from normalizeRoles,
        // and regionPayloadData keeps them out of data. The
        // filter is name-keyed to match that payload
        // exclusion — a field spelled "roles" never rides
        // region data either way.
        .filter(f => f !== spec.via && f !== 'roles')
        .map(f => {
          const fieldMeta = addForm[f]
          return (
            <FieldRow key={f} field={fieldMeta}>
              {renderRegionField(
                fieldMeta, values[f],
                v => onChange(f, v)
              )}
            </FieldRow>
          )
        })}
    </div>
  )
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
    // Plan 02 widened this for the region's normalizeRoles: a
    // nested :type-roles entry (role-name (perm...)) — legal
    // model syntax that rides the wire verbatim — serializes as
    // [string, ...unknown[]]. The parent Add path still copies
    // it verbatim; the region normalizes element zero.
    'type-roles'?: Array<string | [string, ...unknown[]]>
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

// The /api/item envelope: { type, fields, record,
// allowed-values } (the type key arrives as "type", never
// "type-key" — rest.lisp's be-rec handler spells it that way).
// Plan 03: result.children carries the emitted region spec
// (beside fields, not inside record) on :view-form fetches of
// :children-declaring types.
interface ItemResponse {
  result?: {
    type?: string
    fields?: Record<string, Record<string, Field>>
    record?: Record<string, any> | any[] | null
    'allowed-values'?: Record<string, string[]>
    children?: ChildSpec[]
  }
}

// One normalized region spec from the wire: the compiler's
// exactly-one rule means the frontend reads element zero and
// never selects among siblings.
interface ChildSpec {
  type: string
  via: string
  sort: [string, string]
}

// Desired record target: {generation, type, id, mode} — the
// URL-facing request. id/mode null when no record is requested
// (the sentinel spelling; the target object itself is never
// bare null).
interface NavTarget {
  generation: number
  type: string | null
  id: string | null
  mode: RecordMode | null
}

// An attempted record open: what has actually been sent toward
// /api/item. attemptId is a monotonic counter (outranks
// generation); generation/type/id/mode identify the open.
interface Attempt {
  attemptId: number
  generation: number
  type: string
  id: string
  mode: RecordMode
}

// listError's owner object: only current when
// err.type === type && err.sequence === currentListSeq.
interface ListErrorOwner {
  type: string
  sequence: number
  message: string
}

// A record-open failure: cause decides the Retry protocol
// ('item' re-fetches the item directly; 'metadata' re-issues
// the list fetch and lets the arrival effect fetch the item).
type RecordErrorKind = 'item' | 'metadata'
interface RecordError {
  kind: RecordErrorKind
  message: string
}

// Lowercase + trim a record id for target comparisons — the
// one shared normalization rule outside canonical state.
const normId = (id: string | null): string | null =>
  id == null ? null : id.trim().toLowerCase()

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
    // The quote strip takes a whole marker run (nested quotes
    // write "> > >"); [ \t]? not \s? so it never eats the line
    // terminator — a bare ">" line becomes an empty line and
    // joins as "·", matching the paragraph break it renders as.
    .replace(/^\s{0,3}(?:>[ \t]?)+/gm, '')      // blockquotes
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

// RecordErrorBox (step 18): the failed-record surface — one
// component for both shells (the main return and the no-data /
// error early return) so they cannot drift. A contained alert
// box, not inline form chrome: Retry / Dismiss only. Dismiss IS
// the close — a third Close button here is what commingled with
// form controls — and its destination is dismissRecordError's
// stranding rule (single-row view form or landing). role=alert
// for screen readers; centered max-width box reads as a dialog
// without an overlay (the list below is legitimate context on
// non-hide-list types).
function RecordErrorBox({ message, onRetry, onDismiss }: {
  message: string
  onRetry: () => void
  onDismiss: () => void
}) {
  return (
    <div
      role="alert"
      style={{
        maxWidth: '30rem',
        margin: '1.5rem auto',
        padding: '1rem 1.25rem',
        textAlign: 'center',
        border: '1px solid var(--error-bright)',
        borderRadius: '0.375rem',
        background: 'var(--error-bg, transparent)'
      }}
    >
      <p style={{
        margin: '0 0 0.75rem',
        fontWeight: 500,
        color: 'var(--error-bright)'
      }}>
        {message}
      </p>
      <div>
        <button type="button" onClick={onRetry}>
          Retry
        </button>
        <button type="button" onClick={onDismiss}
          style={{ marginLeft: '0.5rem' }}>
          Dismiss
        </button>
      </div>
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

// Context for the View-form FK link (step 25): what the link
// branch needs beyond (field, value) — the field key (to read
// the "<field>-id" sibling), the full record, the visible-type
// list (lowercased names), the is-View-form flag (Edit/Add
// read-only fields stay plain), and the permalink open callback.
interface ReadOnlyCtx {
  fieldKey: string
  record: Record<string, any>
  viewTypeList: string[]
  isViewForm: boolean
  open: ((t: string, id: string) => void) | null
}

function renderReadOnlyField(
  field: Field,
  value: any,
  ctx?: ReadOnlyCtx
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

  // FK link (View form only): every gate leg must hold — the
  // field carries a target, the target names a visible type,
  // the record carries a string "<field>-id" sibling, and the
  // display value is a nonempty string (empty renders exactly
  // today's output, the em dash; whitespace-only text also
  // stays unlinked but renders as whitespace, exactly as
  // today; no fallback label is invented). Suppressed shapes
  // have target but no sibling (backend), invisible targets
  // fail the type list, and Edit/Add read-only fields never
  // enter (isViewForm false).
  if (ctx?.isViewForm && field.target != null) {
    const targetVisible =
      ctx.viewTypeList.includes(field.target)
    const sibling = ctx.record[`${ctx.fieldKey}-id`]
    const idOk = typeof sibling === 'string'
      && sibling.trim() !== ''
    const valOk = typeof value === 'string'
      && value.trim().length > 0
    if (targetVisible && idOk && valOk && ctx.open != null) {
      const target = field.target
      const href = `#/app?type=${
        encodeURIComponent(target)}&id=${
        encodeURIComponent(sibling)}`
      // A real <a>: modifier-click, middle-click, and open-in-
      // new-tab keep native behavior; only an unmodified left
      // click is intercepted into the app's open path (the
      // same path as a permalink arrival — writeUrl stays the
      // only hash writer). A link to the record already open
      // is a no-op (openEditForm's target check).
      const handleClick = (e: React.MouseEvent) => {
        if (e.metaKey || e.ctrlKey || e.shiftKey || e.altKey
            || e.button !== 0) return
        e.preventDefault()
        ctx.open!(target, sibling)
      }
      return (
        <div style={{ padding: '0.3rem 0', minHeight: '1.2em' }}>
          <a
            className="chip"
            href={href}
            title={`Open ${value}`}
            style={{ textDecoration: 'none' }}
            onClick={handleClick}
          >
            {value}
          </a>
        </div>
      )
    }
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

// The scope control (plan 04): the one clearing surface shared
// by both shells (the filter-bar chip and the no-data
// fallback), so the two cannot drift on clearing semantics. A
// valid scope renders "Post: <title> ×" when the title has
// resolved and "Post ×" while unresolved or after a failed
// lookup — never the uuid. The × is clearScope, never
// toggleChipFilter (the scope is not a listFilters entry).
function ScopeChip({ title, onClear }: {
  title: string | null
  onClear: () => void
}) {
  return (
    <span className="chip chip-active">
      {title != null ? `Post: ${title}` : 'Post'}
      <button
        type="button"
        className="chip-x"
        title="Clear post scope"
        onClick={onClear}
      >×</button>
    </span>
  )
}

// The invalid-scope surface (plan 04): a neutral error line
// with its Clear scope action — never a Post chip pretending
// the field is known. Structurally malformed input only; a
// structurally valid but server-invalid value takes the
// ordinary list error path instead.
function InvalidScopeNotice({ onClear }: {
  onClear: () => void
}) {
  return (
    <p style={{ color: 'var(--error)', margin: '0.5rem 0' }}>
      Invalid post scope
      <button
        type="button"
        onClick={onClear}
        style={{ marginLeft: '0.5rem' }}
      >
        Clear scope
      </button>
    </p>
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
  // Plan 03 (region renderer): the normalized single region
  // spec retained from the open path's /api/item envelope
  // (result.children[0] ?? null — 02 attaches it beside
  // fields, and openEditForm's local itemJson is where it is
  // visible). Cleared everywhere editRecord is; the region
  // reads it, never data.result.
  const [childSpec, setChildSpec] =
    useState<ChildSpec | null>(null)
  // Read-only record view: the open editRecord renders through
  // the read-only field branches instead of inputs. Set by
  // openEditForm(record, true); cleared everywhere editRecord is.
  const [formViewing, setFormViewing] = useState(false)
  // Desired record target (URL-facing): recordId / recordMode
  // hold the requested record state. Kept distinct from the
  // loaded form (editRecord / formViewing) so the writer never
  // erases id during a fetch, and a failed open keeps its
  // permalink while showing a retryable error.
  const [recordId, setRecordId] = useState<string | null>(null)
  const [recordMode, setRecordMode] =
    useState<RecordMode>('view')
  // Record-open failure surface, separate from listError.
  // kind: 'item' (fetch failed / bad shape / edit-not-permitted
  // — Retry re-fetches the item directly) or 'metadata' (list
  // metadata failed / the requested form is not declared —
  // Retry re-issues the list fetch).
  const [recordError, setRecordError] =
    useState<RecordError | null>(null)
  // Navigation refs (record permalinks):
  //   navGen — persistent clock; advances on every
  //     invalidation (close, opens, type/view switches, logout,
  //     no-target rewrites, terminal metadata decisions) so two
  //     no-target states still differ.
  //   navRef — the *requested* target {generation, type, id,
  //     mode} (id/mode null when none; never bare null).
  //   attemptRef — the *attempted* open (sent toward
  //     /api/item): {attemptId, generation, type, id, mode}.
  //     attemptId outranks generation: a direct cross-type open
  //     can fetch before the target type's list metadata
  //     resolves, and a metadata Retry can issue a second fetch
  //     under the still-current generation; only the attempt
  //     identity fences both.
  //   attemptSeq — monotonic counter allocating attemptIds.
  const navGen = useRef(0)
  const navRef = useRef<NavTarget>(
    { generation: 0, type: null, id: null, mode: null })
  const attemptRef = useRef<Attempt | null>(null)
  const attemptSeq = useRef(0)
  // Writer cause ref (the echo rule): 'push' pending → the next
  // writer fire pushes (default); 'replace' pending and no push
  // → replaceState. Consumed and cleared on every writer fire.
  const pendingUrlCause = useRef<'push' | 'replace' | null>(null)
  // Parked-history latch: the latest history arrival
  // deliberately left app state untouched (a non-app hash or an
  // unknown/invisible type). While set, the writer suppresses
  // an automatic replace entirely; an intentional push is
  // allowed and clears the latch after writing.
  const parkedHistoryRef = useRef(false)
  // Settings auto-open suppression latch: the settings type
  // whose auto-open is suppressed (a record permalink wins over
  // the one-row convenience, and survives Close/Dismiss so it
  // cannot reopen-rewrite id in a loop).
  const settingsAutoOpenSuppressedRef = useRef<string | null>(
    null)
  // List-request freshness: one counter advancing on both
  // invalidation and request allocation; a response commits only
  // when its captured sequence is still the latest.
  const listSeq = useRef(0)
  // Clipboard-operation token for Best Link: fences a pending
  // writeText so record A's in-flight copy cannot paint
  // confirmation on record B's button.
  const clipboardToken = useRef(0)
  // Best Link confirmation state: null | 'copied' | 'failed',
  // with its clear timer.
  const [copyState, setCopyState] =
    useState<'copied' | 'failed' | null>(null)
  const copyTimer = useRef<ReturnType<
    typeof setTimeout> | null>(null)
  const [listError, setListError] =
    useState<ListErrorOwner | null>(null)
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
  // Post scope (plan 04): the comments-tab FK scope — its own
  // state slot and its own URL dimension, never a listFilters
  // row (display-column chip semantics) and never a reuse of
  // id (which names the open record; a scoped list may carry
  // both). none / valid {via, id} / invalid {raw, kept for the
  // writer / history round trip}. SCOPE_NONE is the shared
  // constant so the effect deps treat "no scope" as one stable
  // value.
  const [scope, setScope] = useState<ScopeState>(SCOPE_NONE)
  // Resolved scope title: the visible chip text for the
  // current scope, or null (the generic "Post" chip). Keyed by
  // the current title key (titleKeyRef, assigned during
  // render) — an unkeyed string can flash post A's title on
  // post B.
  const [scopeTitle, setScopeTitle] = useState<string | null>(
    null)
  // Title-resolution session machinery (plan 04): a monotonic
  // epoch advanced by resetSessionState (logout / auth
  // failure), a per-session cache and in-flight registry keyed
  // epoch|targetType|id (the epoch in the key is what makes a
  // cached guest failure unable to suppress a later session's
  // lookup), and a request sequence + current-key ref fencing
  // visible commits. A cache write requires the same epoch; a
  // visible commit additionally requires the latest sequence
  // and the same current key.
  const sessionEpochRef = useRef(0)
  const titleCacheRef = useRef<
    Map<string, string | null>>(new Map())
  const titleInFlightRef = useRef<
    Map<string, Promise<string | null>>>(new Map())
  const titleSeqRef = useRef(0)
  const titleKeyRef = useRef<string | null>(null)
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
  // The requested form's metadata for a pending or failed
  // record target (editRecord null → activeForm undefined).
  // Gated on the envelope being the target type's — a
  // cross-type transition's stale envelope must not leak its
  // forms into the decision.
  const requestedForm: Record<string, any> | undefined =
    recordId != null && data?.result?.['type-key'] === type
      ? (data.result as Record<string, any>)[
          recordMode === 'edit'
            ? 'update-form' : 'view-form']
      : undefined
  const hideList =
    (activeForm as Record<string, any> | undefined)
      ?.['hide-list'] === true
    // A pending or failed record target also hides the list
    // when the *requested* form says so: the one-row table
    // would otherwise render under an in-flight open or the
    // error box on a :hide-list t type (settings).
    || (!isEditMode && !showAddForm
        && requestedForm?.['hide-list'] === true)

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
    // Sequence guard: capture the sequence this request runs
    // under (allocation advances the same counter invalidation
    // uses); every commit below re-checks it, so an older
    // response can never replace newer metadata or regress
    // newer sort/page/error state.
    const seq = ++listSeq.current
    // ListErrorOwner now, not a bare string: setListError(null)
    // stays a plain clear.
    setListError(null)
    // An invalid structural scope stands down before any
    // request (plan 04): the sequence is allocated (older work
    // invalidated), the error and the old envelope clear, and
    // nothing is fetched — never an unscoped bootstrap, never
    // stale rows. A structurally valid but server-invalid
    // field or non-UUID value takes the ordinary error path
    // below instead (fail-closed through listError).
    if (scope.kind === 'invalid') {
      setData(null)
      return null
    }
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
      // The scope row (plan 04): emitted independently of
      // formReady — the via is already in the scope, so there
      // is no reason to wait for list-form metadata — and
      // before the final nonempty check. A cold scoped request
      // therefore carries fk-eq on the FIRST request; the
      // form-dependent rows (chips / notq / bool) still ride
      // the one-shot refetch. Never an unscoped metadata
      // bootstrap.
      if (scope.kind === 'valid') {
        rows.push([type, scope.via, 'fk-eq', scope.id])
      }
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
      // Stale-response fence: every commit in this resolution
      // (setData, the sort echo, the defaultSortRef cache, the
      // page clamp) happens only when this response's captured
      // sequence is still the latest issued.
      if (seq !== listSeq.current) return json
      setData(json)
      // Plan 09: paint sortField/sortDir from the echoed effective
      // sort on every successful fetch (including null — base types
      // with no sort echo unsorted). The backend owns the ranking
      // default (first sortable measure DESC on rollups), so the
      // header indicator always matches the actual order.
      const echoed = json?.result?.sort ?? null
      if (echoed && typeof echoed.field === 'string') {
        if (sortField !== echoed.field
            || sortDir !== (echoed.dir === 'desc'
              ? 'desc' : 'asc')) {
          // Echo commit — mark replace (an automatic
          // reconciliation write earns no Back entry), but
          // never while a user push is pending (push precedence
          // for the entire pending writer run).
          if (pendingUrlCause.current !== 'push') {
            pendingUrlCause.current = 'replace'
          }
        }
        setSortField(echoed.field)
        setSortDir(echoed.dir === 'desc' ? 'desc' : 'asc')
      } else {
        if (sortField !== null) {
          if (pendingUrlCause.current !== 'push') {
            pendingUrlCause.current = 'replace'
          }
        }
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
      if (currentPage > maxPage) {
        // Echo commit (page clamp) — replace, same precedence.
        if (pendingUrlCause.current !== 'push') {
          pendingUrlCause.current = 'replace'
        }
        setCurrentPage(maxPage)
      }
      return json
    } catch (err: any) {
      // Error commits are sequence-guarded too, and the error
      // carries its owner (type + sequence + message): readers
      // treat it as current only under
      // err.type === type && err.sequence === listSeq.current.
      if (seq !== listSeq.current) return null
      setData(null)
      setListError({
        type, sequence: seq,
        message: err.message || 'Failed to load data'
      })
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
  // Every commit inside is a list-query change: the sequence
  // bumps synchronously here (a committed change does not issue
  // its replacement request until the fetch effect runs after
  // the render, so an older response resolving in that gap still
  // holds the latest issued sequence).
  const resetListQuery = () => {
    ++listSeq.current
    setSortField(null)
    setSortDir('asc')
    defaultSortRef.current = null
    setSearchTerm('')
    setDebouncedSearch('')
    setNotTerm('')
    setDebouncedNotTerm('')
    setListFilters({})
    setBoolFilters({})
    // The scope clears with the list identity (plan 04): a
    // scope omitted here would survive a type change and emit
    // against the wrong type. clearScope is the explicit
    // user-facing clearer; this is the identity reset.
    setScope(SCOPE_NONE)
    // Forget the per-type seeding so the next type (or re-entry
    // into this one) re-seeds its authored defaults. This ref is
    // the only thing standing between every list refetch and
    // re-clobbering the user's filters — it must be cleared by
    // every future {} clearer too, or the next fetch re-seeds
    // and silently undoes the user's Any.
    boolSeededForRef.current = null
    // Settings auto-open suppression (step 16): a fresh list
    // identity is an explicit entry (type tab, view switch,
    // Home, logout, a cross-type open) — the convenience
    // auto-open resumes there. History arrivals never pass
    // through here, so a Back into the id-less settings entry
    // keeps the latch (no reopen-rewrite loop).
    settingsAutoOpenSuppressedRef.current = null
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
    clearRecordTarget({ invalidateList: false })
    setListError(null)
    setPageDescOpen({})
    // Session end invalidates all navigation and list state.
    ++navGen.current
    navRef.current =
      { generation: navGen.current,
        type: null, id: null, mode: null }
    attemptRef.current = null
    ++listSeq.current
    resetListQuery()
    // Title-resolution session invalidation (plan 04): advance
    // the epoch, clear the cache / in-flight registry / visible
    // result, and fence outstanding title work — an old promise
    // can never repopulate the new session (cache writes
    // require the captured epoch), and the epoch in every
    // cache key means a cached guest failure cannot suppress a
    // later session's lookup.
    ++sessionEpochRef.current
    titleCacheRef.current.clear()
    titleInFlightRef.current.clear()
    ++titleSeqRef.current
    titleKeyRef.current = null
    setScopeTitle(null)
  }

  const changeType = (newType: string) => {
    clearRecordTarget({ invalidateList: false })
    setType(newType)
    setShowAddForm(false)
    setFormValues({})
    setPasswordConfirm({})
    resetListQuery()
  }

  // The Region's escape hatch (plan 04): one React-batched
  // state navigation to the child tab scoped to this parent.
  // changeType's internal resetListQuery clears the old query
  // and scope, and its clearRecordTarget closes the parent
  // form (and child 3's childSpec); the scope installs after
  // that reset, the old list envelope clears synchronously
  // (no stale unscoped rows under the pending scoped request),
  // and the writer effect — not this callback — creates the
  // hash entry. No separate resetListQuery, no hash write, no
  // hashchange wait; pushState emits no popstate and the
  // own-write hashchange is rejected by the equality gate, so
  // the parent form closes through changeType alone.
  const onShowAll = (spec: ChildSpec, parentId: string) => {
    pendingUrlCause.current = 'push'
    changeType(spec.type)
    setData(null)
    setScope({ kind: 'valid', via: spec.via, id: parentId })
  }

  // The scope × and the invalid-scope Clear control (plan 04):
  // one restart. Synchronously invalidates old list work,
  // clears the old envelope, marks the user push, sets scope
  // to none, and resets page and selection. Never
  // toggleChipFilter, never a fetchList through a closure
  // holding the old scope — the fetch effect's scope
  // dependency owns exactly one replacement fetch. Preserves
  // positive/negative search, ordinary chips, and boolean
  // choices.
  const clearScope = () => {
    ++listSeq.current
    setData(null)
    pendingUrlCause.current = 'push'
    setScope(SCOPE_NONE)
    setCurrentPage(1)
    setSelectedIds([])
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
    clearRecordTarget({ invalidateList: false })
    setViewMode(mode)
    setShowAddForm(false)
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

  // clearLoadedForm: the loaded-form-only half of the record
  // clearing (no navRef / attemptRef / desired-target writes —
  // the login seed and history listener install the incoming
  // target AFTER invalidating older work; calling the full
  // clearer there would fight their assignment).
  const clearLoadedForm = () => {
    setEditRecord(null)
    setFormViewing(false)
    setFormValues({})
    setPasswordConfirm({})
    setRecordError(null)
    setPendingActions(new Set())
    // Add-form state rides the loaded-form clears: a terminal
    // record decision (arrival-effect paths) must not leave an
    // Add form mounted under the recordError it sets.
    setShowAddForm(false)
    // The retained region spec clears with the loaded form
    // (plan 03): no open record, no region.
    setChildSpec(null)
  }

  // clearRecordTarget: the single record-target clearing helper
  // (steps 5/10). Clears desired id/mode, the loaded form state,
  // and recordError; rewrites navRef with a new generation
  // (fencing stale item responses); clears attemptRef. Takes an
  // explicit list-invalidation option: ordinary record-only
  // Close/Dismiss, same-type Close → reopen, and Add pass false
  // (they change no list query and must not strand the only
  // pending metadata/list response); type/view switches and
  // logout/login rely on resetListQuery / the session reset for
  // their own sequence invalidation.
  const clearRecordTarget = (
    opts: { invalidateList: boolean }
  ) => {
    setRecordId(null)
    setRecordMode('view')
    setEditRecord(null)
    setFormViewing(false)
    setFormValues({})
    setPasswordConfirm({})
    setRecordError(null)
    setPendingActions(new Set())
    setChildSpec(null)
    ++navGen.current
    navRef.current =
      { generation: navGen.current,
        type: null, id: null, mode: null }
    attemptRef.current = null
    if (opts.invalidateList) ++listSeq.current
  }

  // Options for openEditForm — the one open protocol.
  interface OpenOpts {
    // The type the record belongs to (defaults to the global
    // type; an FK link passes its own target type).
    type?: string
    // Strict mode: permalink / FK-link opens fail closed to
    // recordError instead of falling back to the supplied row.
    strict?: boolean
    // An already-registered attempt identity (the arrival
    // effect's registration; direct opens let this function
    // allocate its own).
    attempt?: Attempt
    // URL-facing: assign recordId/recordMode (same-type) or
    // install the atomic cross-type transition. Non-URL opens
    // (settings auto-open, post-action reopen) register in
    // navRef but never write the id into the URL state.
    url?: boolean
    // Preserve terminal button statuses on this open (the
    // post-action reopen's attempt-owned intent).
    preserveStatus?: boolean
  }

  // openEditForm: the single open choke point. Every open — row
  // click, View → Edit, FK link, history/login arrivals, the
  // settings auto-open, post-action reopens — goes through here.
  // Registers the requested target in navRef and the attempt in
  // attemptRef synchronously (before its fetch) unless an
  // already-registered attempt is passed in; clears the previous
  // loaded form before the fetch; guards every commit against
  // the captured attemptId / generation / target.
  const openEditForm = async (
    record: any, viewing = false, opts: OpenOpts = {}
  ) => {
    const openType = opts.type ?? type
    const recId = typeof record?.id === 'string'
      ? record.id : null
    if (recId == null) return
    const mode: RecordMode = viewing ? 'view' : 'edit'
    // Attempt + navigation registration. Direct opens allocate
    // the generation and attempt identity themselves; the
    // arrival effect passes its already-registered attempt in
    // (adopted without allocating a newer generation).
    const attempt: Attempt = opts.attempt ?? {
      attemptId: ++attemptSeq.current,
      generation: ((): number => {
        ++navGen.current
        navRef.current = {
          generation: navGen.current,
          type: openType, id: normId(recId), mode
        }
        return navGen.current
      })()
      , type: openType, id: normId(recId)!, mode
    }
    if (!opts.attempt) attemptRef.current = attempt
    // Every new open synchronously clears the previously loaded
    // record/form state — no old form or Update control survives
    // into the new request (a throttled A → B open must not
    // leave A's Update control active under B's URL).
    setEditRecord(null)
    setChildSpec(null)
    setFormValues({})
    setFormViewing(false)
    setPasswordConfirm({})
    setRecordError(null)
    setPendingActions(new Set())
    // Add-form state too (plan step: every new open clears the
    // previously loaded record/form state): without this, an
    // Add form open when a strict open fails leaves the Add
    // form mounted beside / instead of the error box.
    setShowAddForm(false)
    if (opts.url) {
      // URL-facing open: same-type assigns the record target;
      // cross-type is one atomic transition (type, derived
      // viewMode, record target, list-query reset, selection,
      // timers, synchronous sequence invalidation — one batch).
      if (openType !== type) {
        const target = types.find(
          t => t.name.toLowerCase() === openType.toLowerCase())
        setViewMode(
          target?.category === 'system' ? 'admin'
          : target?.category === 'settings' ? 'settings'
          : 'app')
        setType(openType)
        resetListQuery()
        setSelectedIds([])
      }
      setRecordId(normId(recId))
      setRecordMode(mode)
    } else if (recordId != null) {
      // A non-URL open (settings auto-open, post-action reopen)
      // while a URL permalink target stands: the permalink wins.
      // Keep the URL state; the open still renders (its loaded
      // target is the recorded non-URL open below).
    }
    const formName = viewing ? viewFormName : 'update-form'
    let itemJson: ItemResponse | null = null
    let fetchFailed = false
    try {
      const res = await apiFetch(
        `/api/item?type=${encodeURIComponent(openType)}` +
        `&id=${encodeURIComponent(recId)}` +
        `&form=${formName}`)
      if (res.ok) {
        itemJson = await res.json()
      } else {
        fetchFailed = true
      }
    } catch {
      fetchFailed = true
    }
    // The once-guard: attemptId first (outranks generation),
    // then generation + navRef target. After it passes, the
    // continuation's synchronous commits run unchecked.
    const current = navRef.current
    const currentAttempt = attemptRef.current
    const stillCurrent =
      currentAttempt != null
      && currentAttempt.attemptId === attempt.attemptId
      && current.generation === attempt.generation
      && current.type === openType
      && normId(current.id) === normId(recId)
      && current.mode === mode
    if (!stillCurrent) return
    const envelopeRecord = itemJson?.result?.record
    const envelopeTypeOk =
      itemJson?.result?.type == null
      || itemJson.result.type === openType
      || itemJson.result.type.toLowerCase()
        === openType.toLowerCase()
    // The non-strict fallback (the /api/item fetch failed): the
    // supplied list row becomes the record. Drop its public-url —
    // a list row can be minutes old, and a role revoked since would
    // otherwise resurrect a dead public URL; only a fresh item fetch
    // supplies the key.
    let fullRecord: any = opts.strict ? null
      : (({ 'public-url': _drop, ...rest }) => rest)(record)
    if (itemJson && !fetchFailed
        && envelopeRecord != null
        && !Array.isArray(envelopeRecord)
        && typeof envelopeRecord === 'object'
        && typeof envelopeRecord.id === 'string'
        && envelopeRecord.id.toLowerCase()
          === normId(recId)
        && envelopeTypeOk) {
      fullRecord = envelopeRecord
    } else if (opts.strict) {
      // Strict mode fails closed: non-OK, empty result, absent /
      // array record, wrong id, wrong type, network exception —
      // generic not-found/not-accessible, editRecord stays null.
      // mode=edit with a successful fetch but update !== true:
      // "Edit not permitted" (the fetched row's own update flag
      // is the gate, not the mounted page's row).
      if (mode === 'edit' && envelopeRecord != null
          && !Array.isArray(envelopeRecord)
          && typeof envelopeRecord === 'object'
          && envelopeRecord.update !== true) {
        setRecordError({
          kind: 'item',
          message: 'Edit not permitted'
        })
      } else {
        setRecordError({
          kind: 'item',
          message:
            'Record not found or not accessible'
        })
      }
      return
    }
    // A terminal status written by an earlier action is stale on
    // every fresh open (row Edit/View, the settings auto-open) —
    // clear it so the form never greets the user with an old
    // "complete". Kept when this open is the post-action re-open
    // (the attempt-owned preserveStatus option) or when a worker
    // is still running. Display-only: the blank lives in
    // editRecord / formValues and never reaches the server
    // (submit drops empty-string fields).
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
    // Form metadata comes from the fetched item envelope (its
    // own fields), not the singleton list data — a cross-type
    // open must not read the source type's forms.
    const keepStatus = opts.preserveStatus === true
    if (!keepStatus) {
      const cleared: Record<string, any> = { ...fullRecord }
      const openFormMeta =
        itemJson?.result?.fields?.[formName]
        || (data?.result?.['type-key'] === openType
          ? data?.result?.[formName] : undefined)
        || {}
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
    // Plan 03: capture the normalized single region spec — only
    // when viewing (the region is a reading surface), only
    // after the once-guard passed and the envelope was accepted
    // (a strict failure or a non-strict fallback leaves it
    // null; the fallback row never carried children). Absent or
    // empty array means null — the parent type has no region.
    setChildSpec(viewing
      ? (itemJson?.result?.children?.[0] ?? null)
      : null)
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
    // Record-only clearing (step 10): no list-sequence bump —
    // Close changes no fetch-effect dependency and schedules no
    // replacement request; an unconditional bump would discard
    // the only in-flight metadata/list response. An intentional
    // user close that removes a URL id marks push.
    if (recordId != null) pendingUrlCause.current = 'push'
    // Hide-list stranding (landing-only): closing the open form
    // on a type whose form hides the list must not expose the
    // list the author hid or strand a blank page — route to the
    // landing page, which performs its own close. Settings mode
    // never reaches here (handleCancel routes it through
    // returnToLanding directly); this covers Close / Cancel and
    // the post-submit / post-action closes on hide-list user
    // types. Only when a form is actually open (activeForm
    // defined) — a no-op close keeps today's bare behavior.
    if (viewMode !== 'settings'
        && (activeForm as Record<string, any> | undefined)
          ?.['hide-list'] === true) {
      setShowAddForm(false)
      returnToLanding()
      return
    }
    clearRecordTarget({ invalidateList: false })
    setShowAddForm(false)
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
    // Row opens are URL-facing user opens: they register their
    // own attempt (openEditForm), assign the record target, and
    // mark push (an intentional navigation).
    pendingUrlCause.current = 'push'
    openEditForm(rec, true, { url: true })
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
    pendingUrlCause.current = 'push'
    openEditForm(rec, true, { url: true })
  }

  // Return to the landing page (used by settings Submit/Cancel).
  const returnToLanding = () => {
    // Record-only synchronous close (step 10): no list-sequence
    // invalidation here — the /api/info continuation may hang or
    // return no landing page, and an immediate bump would strand
    // the only pending list response with no guaranteed
    // replacement. resetListQuery inside the delayed arms owns
    // that invalidation (it bumps synchronously wherever it
    // runs). The synchronous close writes a source-type list
    // entry without the record; the delayed landing assignment
    // writes a second entry when canonically distinct (two
    // Back-reachable states, each naming the screen it showed).
    if (recordId != null) pendingUrlCause.current = 'push'
    clearRecordTarget({ invalidateList: false })
    // Settings-exit latch: the synchronous close above leaves
    // exactly the state the settings auto-open hunts (settings
    // view still current, the type's one-row envelope still
    // mounted, isEditMode freshly false, suppression latch
    // null). Without arming here the auto-open refires in the
    // gap before the /api/info continuation lands, its open
    // advances navGen, and the continuation's generation guard
    // then rejects the landing — Home / Cancel / Submit strand
    // the user in the reopened settings form (only a
    // synchronous view switch, Admin, escapes). The
    // continuation's resetListQuery clears the latch after
    // viewMode flips in the same batch, so a later Settings
    // entry auto-opens as before; a sync view/type change during
    // the gap clears it too (an explicit entry resumes the
    // convenience). A hung /api/info leaves it set — the
    // auto-open then waits for the next explicit entry, and the
    // row stays clickable in the list.
    if (viewMode === 'settings') {
      settingsAutoOpenSuppressedRef.current = type
    }
    fetchAndApplyCssVariables()
    // Generation captured AFTER the synchronous close/generation
    // advance: the continuation compares against the generation
    // returnToLanding itself installed, so its own assignments
    // pass while a newer navigation's do not.
    const gen = navGen.current
    apiFetch('/api/info')
      .then(r => r.json())
      .then(info => {
        if (navRef.current.generation !== gen) return
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
        if (navRef.current.generation !== gen) return
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
    // The open attempt identity owns this action's
    // continuations: a newer open or terminal decision clears
    // attemptRef, fencing the reopen.
    const openAttempt = attemptRef.current
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
        // and fetchList doesn't read the form. Once-guard: a
        // navigation during the await fences the continuation.
        if (attemptRef.current?.attemptId
            !== openAttempt?.attemptId) return
        if (viewMode === 'settings') {
          // Not awaited, and no fetchList: returnToLanding closes
          // the form and triggers its own landing fetch
          // (/api/info → type change → the list effect). A second
          // fetchList here would race it.
          returnToLanding()
        } else {
          if (recordId != null) {
            pendingUrlCause.current = 'push'
          }
          closeForm()
          await fetchListRef.current()
        }
      } else {
        // HTTP error, sync "failed", async "accepted", or
        // unparseable body: keep today's behavior. The re-opened
        // form is where the failure message shows and where the
        // running-status spinner / 3s poll live.
        // Once-guard first: a navigation during the response
        // (including a terminal metadata decision that advanced
        // navGen / cleared attemptRef) must not be followed by an
        // unwanted reopen.
        if (attemptRef.current?.attemptId
            !== openAttempt?.attemptId) return
        // Reload the list with the current page/sort/search, then
        // re-open the edit form by id: an action that changes a
        // sort key can move the row off the current page, so the
        // row must not be looked up in the current page's records.
        // fetchListRef.current, never the captured fetchList
        // closure — a same-type sort/page change during the
        // action does not advance navGen, so the once-guard alone
        // cannot fence it; the ref call issues the current query.
        await fetchListRef.current()
        if (attemptRef.current?.attemptId
            !== openAttempt?.attemptId) return
        // This re-open keeps terminal statuses: the user just
        // pressed the button and earned the "failed: …"
        // feedback. preserveStatus travels as the attempt-owned
        // option; a discarded/failed reopen leaves nothing for a
        // later unrelated open to consume. The reopen preserves
        // the form's URL-facing-ness: a URL-facing reopen
        // re-installs the target as URL state (same target, one
        // new push-marked entry only when the id was live); a
        // non-URL reopen (the settings auto-open) stays id-free
        // — the hash never gains id.
        const wasUrlOpen = recordId != null
          && normId(recordId) === normId(id)
          && navRef.current.type === type
        if (wasUrlOpen) pendingUrlCause.current = 'push'
        openEditForm({ id }, false, {
          url: wasUrlOpen,
          preserveStatus: true
        })
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
        // Once-guard: capture the navigation generation at
        // submit; on pass, run the synchronous close/landing/
        // refresh without further checks (a per-call re-check
        // would reject the operation's own close, which bumps
        // the generation the check would compare against).
        // Refresh goes through fetchListRef.current, never the
        // captured fetchList closure (the old query under a new
        // sequence).
        const gen = navGen.current
        const submitOk = () =>
          navRef.current.generation === gen
        if (viewMode === 'settings') {
          if (submitOk()) returnToLanding()
        } else {
          if (submitOk()) {
            if (recordId != null) {
              pendingUrlCause.current = 'push'
            }
            closeForm()
          }
          if (submitOk()) fetchListRef.current()
        }
      } else {
        alert(await errorMessage(res, isEditMode ? 'Failed to update' : 'Failed to insert'))
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
      // Same once-guard as the file path (see there).
      const gen = navGen.current
      const submitOk = () =>
        navRef.current.generation === gen
      if (viewMode === 'settings') {
        if (submitOk()) returnToLanding()
      } else {
        if (submitOk()) {
          if (recordId != null) {
            pendingUrlCause.current = 'push'
          }
          closeForm()
        }
        if (submitOk()) fetchListRef.current()
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

    // Once-guard: capture the navigation generation; the
    // trailing selection clear + refresh run only when no
    // navigation superseded the delete.
    const gen = navGen.current
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
    if (navRef.current.generation === gen) {
      setSelectedIds([])
      fetchListRef.current()
    }
  }

  useEffect(() => {
    if (!loggedIn) return
    // Fetch landing page + types in parallel on login
    // The seed's generation: captured before the awaits; the
    // continuation (resolution OR rejection) applies its
    // assignments only when the current navRef generation still
    // is the seed's (logout or newer navigation wins; a
    // superseded seed assigns nothing — the old .catch(() =>
    // setTypes([])) would erase a newer session's types).
    // Capture, not allocation: the seed is not a navigation
    // invalidator and owns no target until its continuation
    // installs one, so it must not advance the clock — every
    // legitimate fence bumps navGen AND writes navRef together,
    // keeping navRef.generation === navGen.current; a fresh
    // login has both at 0.
    const seedGen = navGen.current
    Promise.all([
      apiFetch('/api/info').then(r => r.json()),
      apiFetch('/api/types').then(r => r.json())
    ]).then(([info, typesJson]) => {
      if (navRef.current.generation !== seedGen) return
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
      // Accepted login seed unparks history (step 13): the
      // browser's initial permalink entry is the live entry and
      // the echoes may replace into it.
      parkedHistoryRef.current = false
      const t = info.result?.['title']
      if (t) setTitle(String(t))
      const lp = info.result?.['landing-page']
      // App state in the URL, Step 3: seed state ← URL. Parse
      // the LIVE hash on every login transition (not once per
      // mount): nothing writes the hash while logged out, so at
      // first login the live hash IS the parked hash; at a
      // relogin it is the last-written hash — exactly the view
      // the previous session showed. A consume-once ref would
      // fail the plan's own relogin test. The seed *requests*
      // the incoming record target: navRef installs it and
      // attemptRef clears (the arrival effect attempts).
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

      // Clear any previous loaded form and pending settings
      // auto-open before assignment (the seed requests; the
      // arrival effect attempts).
      clearLoadedForm()
      settingsAutoOpenSuppressedRef.current = null

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

        // Record target assignment in the same React batch as
        // the other URL fields: the requested id/mode ride the
        // seed exactly like list state, retained across
        // password, restored-session, and guest login.
        setRecordId(c.id ?? null)
        setRecordMode(c.mode ?? 'view')
        navRef.current = {
          generation: navGen.current,
          type: target.name,
          id: c.id ?? null,
          mode: c.id != null ? (c.mode ?? 'view') : null
        }
        attemptRef.current = null

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
        // Post scope (plan 04): full assignment, same source
        // (c) as every other query field — the canonical fk
        // string re-parsed by the shared pure helper, never a
        // re-derived grammar.
        setScope(c.fk != null ? parseScope(c.fk) : SCOPE_NONE)
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
        // No record target when the seed carried none or was
        // dropped: navRef holds the no-target sentinel.
        setRecordId(null)
        setRecordMode('view')
        // Post scope (plan 04): the no-target arm clears it —
        // a canonical fk never survives without its type.
        setScope(SCOPE_NONE)
        navRef.current = {
          generation: navGen.current,
          type: null, id: null, mode: null
        }
        attemptRef.current = null
      }
    }).catch(() => {
      // Rejection arm, same generation guard: a superseded
      // seed assigns nothing (an old session's failure must not
      // erase a newer session's types).
      if (navRef.current.generation !== seedGen) return
      setTypes([])
    })
  }, [loggedIn])

  // ---- record-arrival effect (step 15) ----
  // Runs only while logged in with a desired id, after the
  // metadata-usable check; owns the attempt registration for
  // history/login arrivals. Never fetches twice for the same
  // target (an identical registered attempt means a direct open
  // or this effect's earlier run already fetched). listError is
  // an explicit dependency: a repeated metadata failure after a
  // Retry leaves data already null and changes only listError;
  // without it in the deps the effect never re-runs and the app
  // sits in the silent wait step 17 exists to prevent.
  useEffect(() => {
    if (!loggedIn) return
    if (recordId == null) return
    if (data?.result?.['type-key'] !== type) {
      // Metadata not arrived yet for the target type — but
      // first, step 17's ownership check: a *failed* current
      // list fetch for the target type routes to the metadata
      // error, never a silent wait. The no-list-form shell (a
      // type declaring :view-form but no :list-form) cannot
      // pass the early render return — this error surface is
      // what renders there.
      const err = listError
      const metaFailed =
        err != null && err.type === type
        && err.sequence === listSeq.current
      if (metaFailed && attemptRef.current == null) {
        // Terminal metadata failure: invalidate the outstanding
        // item attempt, clear the loaded form, advance the
        // clock, preserve the target + URL (a direct cross-type
        // open may already have committed editRecord before the
        // metadata failed; a generation-only action continuation
        // must not reopen it afterward; the poller keyed to the
        // dropped form dies with its editRecordId dependency).
        attemptRef.current = null
        clearLoadedForm()
        ++navGen.current
        navRef.current = {
          generation: navGen.current,
          type, id: recordId, mode: recordMode
        }
        setRecordError({
          kind: 'metadata',
          message: "Unable to load the record's page metadata"
        })
      }
      return
    }
    // Metadata usable. Requested-form check (terminal, step
    // 15): no view-form for View, no update-form for edit — set
    // recordError immediately, never a wait, never a fallback
    // to the other form.
    const requestedForm = recordMode === 'edit'
      ? 'update-form' : 'view-form'
    if (data.result[requestedForm] == null) {
      attemptRef.current = null
      clearLoadedForm()
      ++navGen.current
      navRef.current = {
        generation: navGen.current,
        type, id: recordId, mode: recordMode
      }
      setRecordError({
        kind: 'metadata',
        message: recordMode === 'edit'
          ? 'This record cannot be edited here'
          : 'This record has no view form'
      })
      return
    }
    // Identical registered attempt = already fetched (a direct
    // open, this effect's earlier run — StrictMode included —
    // or an item-failure Retry's direct call).
    const att = attemptRef.current
    if (att != null
        && att.type === type
        && normId(att.id) === normId(recordId)
        && att.mode === recordMode) {
      return
    }
    // Register the attempt first, then fetch through it (a
    // write placed after the call re-registers under a new
    // attemptId and strands the in-flight guards). The failed
    // attempt stays marked — no refetch loop; retry only on a
    // new target, an explicit Retry, or a new login session.
    const attempt: Attempt = {
      attemptId: ++attemptSeq.current,
      generation: navGen.current,
      type, id: normId(recordId)!, mode: recordMode
    }
    attemptRef.current = attempt
    openEditForm(
      { id: recordId },
      recordMode === 'view',
      { strict: true, attempt, url: false, type })
  }, [loggedIn, data, type, recordId, recordMode, listError])

  // ---- Best Link copy (step 27) ----
  // The pending write is fenced by the clipboard-operation
  // token (record A's in-flight writeText must not paint
  // confirmation on record B's button); confirmation clears on
  // its timer, re-click, unmount, and any record or mode
  // change. The cleanup also resets copyState itself: the token
  // bump used to make the timer's own clear a no-op, so a
  // "Copied" suffix painted on record A survived onto record B
  // until the next click. The dependency list includes the loaded
  // record's identity: a non-URL A → B transition (the
  // dismissRecordError recovery) leaves recordId and recordMode
  // unchanged, so without it B's button could show A's suffix.
  useEffect(() => {
    return () => {
      if (copyTimer.current) clearTimeout(copyTimer.current)
      clipboardToken.current++
      setCopyState(null)
    }
  }, [recordId, recordMode, editRecord?.id])
  const copyBestLink = () => {
    // The loaded record's own id, not the URL-facing recordId: a
    // non-URL open (the one-row recovery) leaves recordId null (or
    // pointing at a failed id) with a loaded record, and the button
    // must not silently no-op or build the wrong hash there.
    const id = editRecord?.id ?? recordId
    if (id == null) return
    const token = ++clipboardToken.current
    if (copyTimer.current) clearTimeout(copyTimer.current)
    const url = bestLinkUrl(
      editRecord?.['public-url'],
      hashPermalink(window.location.origin,
        window.location.pathname, type, id))
    const finish = (s: 'copied' | 'failed') => {
      if (clipboardToken.current !== token) return
      setCopyState(s)
      if (copyTimer.current) clearTimeout(copyTimer.current)
      copyTimer.current = setTimeout(() => {
        if (clipboardToken.current === token) {
          setCopyState(null)
        }
      }, 2000)
    }
    try {
      if (navigator.clipboard == null
          || typeof navigator.clipboard.writeText
            !== 'function') {
        finish('failed')
        return
      }
      navigator.clipboard.writeText(url)
        .then(() => finish('copied'))
        .catch(() => finish('failed'))
    } catch {
      finish('failed')
    }
  }

  // ---- recordError surfaces (step 18) ----
  // Retry by cause: 'item' re-fetches the item directly through
  // the strict open (one /api/item request; it registers the
  // attempt itself, so the arrival effect cannot double-fetch);
  // 'metadata' re-issues the list fetch (clearing recordError
  // and the current listError, bumping the sequence) and lets
  // the arrival effect fetch the item once the new envelope's
  // gates pass. Every Retry preserves the failed target and the
  // URL. Dismiss is the ordinary close path and preserves the
  // settings suppression latch (step 16).
  const retryRecord = () => {
    if (recordId == null) return
    if (recordError?.kind === 'metadata') {
      setRecordError(null)
      setListError(null)
      ++listSeq.current
      fetchListRef.current()
    } else {
      setRecordError(null)
      attemptRef.current = null
      pendingUrlCause.current = 'push'
      openEditForm({ id: recordId },
        recordMode === 'view',
        { strict: true, url: true })
    }
  }
  const dismissRecordError = () => {
    // Dismiss destination (hide-list types): a bare close
    // strands the page — the requested form hides the list, so
    // nothing renders under the header. A-then-landing: exactly
    // one visible row on the current type's envelope with a
    // declared view-form → open that row's view form (the
    // auto-open's non-URL pattern: the URL keeps type-only, so
    // Back revisits the failed permalink; read-only by design,
    // Edit is one click away on the form). Anything else → the
    // landing page (returnToLanding performs its own close and
    // arms the settings-exit latch). Non-hide-list types keep
    // the bare close — the list renders under where the box was.
    const meta = data?.result
    const sameType = meta?.['type-key'] === type
    const formMeta = (sameType
      ? (recordMode === 'edit'
          ? meta['update-form'] : meta['view-form']) || {}
      : {}) as Record<string, any>
    if (formMeta['hide-list'] === true) {
      const rows = sameType ? (meta?.records || []) : []
      if (rows.length === 1 && meta?.['view-form']) {
        if (recordId != null) pendingUrlCause.current = 'push'
        clearRecordTarget({ invalidateList: false })
        setShowAddForm(false)
        // Non-URL open: no url option, no push mark. The
        // closure recordId is still the old (non-null) value,
        // so openEditForm takes its permalink-wins no-op branch
        // and writes no URL state — exactly what we want.
        openEditForm(rows[0], true)
      } else {
        returnToLanding()
      }
      return
    }
    if (recordId != null) pendingUrlCause.current = 'push'
    clearRecordTarget({ invalidateList: false })
    setShowAddForm(false)
  }

  useEffect(() => {
    if (!loggedIn || type === '__init__') return
    fetchList()
  }, [loggedIn, type, sortField, sortDir, debouncedSearch,
      debouncedNotTerm, currentPage, listFilters, boolFilters,
      hideExclusive, scope])

  // App state in the URL, Step 2: mirror state → URL. Same deps
  // as the main fetch effect above plus the record target — the
  // URL mirrors committed state, one history entry per commit
  // (the debounce already collapses keystrokes; sort clicks,
  // chip toggles, and page turns are single commits — do not add
  // extra batching). viewMode is not a dep and not encoded:
  // every view change lands in the same batch as a type change
  // (switchViewMode sets the new category's first type,
  // returnToLanding the landing type), so the type dep covers
  // view switches. The guards keep the login screen and the
  // empty-app shell from writing URLs. Echo/clamp commits write
  // correctly (rollup default sort, clamped page — shared links
  // reproduce the on-screen order); the canonical compare in
  // writeUrl no-ops the agreeing ones.
  // The echo rule (automatic reconciliation writes replace; user
  // writes push): the pending-cause ref is consulted and cleared
  // on every fire — 'replace' pending and no 'push' →
  // replaceState; otherwise push. While parkedHistoryRef is
  // set, an automatic replace is suppressed entirely (the parked
  // non-app / hand-edited entry must not be overwritten by an
  // app URL); an intentional push remains allowed and unparks
  // history after writing. No mark defaults to push (the safe
  // default).
  useEffect(() => {
    if (!loggedIn || type === '__init__') return
    const cause = pendingUrlCause.current
    pendingUrlCause.current = null
    const replace = cause === 'replace'
    if (replace && parkedHistoryRef.current) {
      // Parked history: suppress the automatic replace, keep
      // the latch (only an accepted app arrival or an
      // intentional push unparks).
      return
    }
    writeUrl(buildUrlState({
      type, recordId, recordMode, scope,
      debouncedSearch, debouncedNotTerm,
      listFilters, boolFilters, hideExclusive,
      sortField, sortDir, currentPage
    }), cause === 'replace' ? { replace: true }
      : undefined)
    if (cause !== 'replace') {
      // An intentional push unparks history (a user navigation
      // is an accepted app write).
      parkedHistoryRef.current = false
    }
  }, [loggedIn, type, recordId, recordMode, sortField, sortDir,
      debouncedSearch, debouncedNotTerm, currentPage,
      listFilters, boolFilters, hideExclusive, scope])

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

  // ---- scope title resolution (plan 04) ----
  // Raw FK metadata from the arrived list form, only when it
  // belongs to the listed type: Field.target is the
  // authoritative FK target and Field.column the target
  // identity field's wire key — never buildChipFieldMeta (a
  // legal :via need not declare :filter-with :reference) and
  // never Field.table (the display source table). Missing
  // metadata means no lookup and the generic "Post" chip (the
  // accepted v1 limitation for generic :children models).
  const scopeVia = scope.kind === 'valid' ? scope.via : null
  const scopeId = scope.kind === 'valid' ? scope.id : null
  const viaMeta: Field | undefined =
    scopeVia != null
    && data?.result?.['type-key'] === type
      ? data?.result?.['list-form']?.[scopeVia]
      : undefined
  const scopeTargetType =
    typeof viaMeta?.target === 'string'
      ? viaMeta.target : null
  const scopeTitleKey =
    typeof viaMeta?.column === 'string'
      ? viaMeta.column : null
  // The current title key, assigned during RENDER (the
  // fetchListRef pattern): a promise resolving between the
  // scope-B render and its effect must not commit A's title —
  // the ref is current before any callback can run. Primitive
  // inputs only (never the whole data object), so the effect
  // below re-runs exactly when the lookup identity changes.
  const currentTitleKey = scopeTargetType != null
    && scopeTitleKey != null && scopeId != null
    ? sessionEpochRef.current + '|' + scopeTargetType
      + '|' + scopeId.toLowerCase()
    : null
  titleKeyRef.current = currentTitleKey
  useEffect(() => {
    if (!loggedIn) return
    const key = titleKeyRef.current
    if (key == null) {
      // No valid scope, or no metadata for it: the visible
      // title clears (a same-value null setState bails).
      setScopeTitle(null)
      return
    }
    const cached = titleCacheRef.current.get(key)
    if (cached !== undefined) {
      setScopeTitle(cached)
      return
    }
    // Capture the epoch and request identity before the fetch:
    // a cache write requires the same epoch (a privileged
    // response delayed until after guest login writes
    // nothing); a visible commit additionally requires the
    // latest title sequence and the same current key (A→B and
    // A→clear responses may populate only their own
    // same-session cache entry, never the visible chip).
    const seq = ++titleSeqRef.current
    const epoch = sessionEpochRef.current
    const targetType = scopeTargetType!
    const titleKey = scopeTitleKey!
    const id = scopeId!
    const commit = (title: string | null) => {
      if (sessionEpochRef.current !== epoch) return
      if (titleSeqRef.current !== seq) return
      if (titleKeyRef.current !== key) return
      setScopeTitle(title)
    }
    // In-flight dedup by cache key: "one fetch" is literal
    // across renders (a re-run for the same key — StrictMode,
    // an A→B→A return — reuses the pending promise and
    // registers its own current commit).
    const pending = titleInFlightRef.current.get(key)
    if (pending != null) {
      pending.then(commit)
      return
    }
    const p = (async (): Promise<string | null> => {
      try {
        // form=view-form explicitly — the REST default is not
        // the title contract.
        const res = await apiFetch(
          `/api/item?type=${
            encodeURIComponent(targetType)}` +
          `&id=${encodeURIComponent(id)}` +
          `&form=view-form`)
        if (!res.ok) return null
        const json = await res.json()
        const rec = json?.result?.record
        if (rec == null || Array.isArray(rec)
            || typeof rec !== 'object') return null
        if (typeof rec.id !== 'string'
            || rec.id.toLowerCase()
              !== id.toLowerCase()) return null
        const rt = json?.result?.type
        if (typeof rt === 'string' && rt !== targetType
            && rt.toLowerCase()
              !== targetType.toLowerCase()) return null
        const t = rec[titleKey]
        if (typeof t !== 'string' || t.trim() === '') {
          return null
        }
        return t
      } catch {
        return null
      }
    })().then(title => {
      // Cache success and failure alike, same epoch only.
      if (sessionEpochRef.current === epoch) {
        titleCacheRef.current.set(key, title)
      }
      titleInFlightRef.current.delete(key)
      return title
    })
    titleInFlightRef.current.set(key, p)
    p.then(commit)
  }, [loggedIn, type, scopeVia, scopeId, scopeTargetType,
      scopeTitleKey])

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
      // state is never reset on history nav, and the arrival
      // parks automatic replacement (an echo must not replace
      // the non-app entry with an app URL). Do NOT
      // "helpfully" reset here: the park-and-restore path
      // lives on state outliving non-app entries.
      if (!s) {
        parkedHistoryRef.current = true
        return
      }
      const byName = (n: string) =>
        types.find(
          t => t.name.toLowerCase() === n.toLowerCase())
      const target = s.type ? byName(s.type) : null
      if (s.type && !target) {
        // hand-edited / unknown / invisible type: park too —
        // the URL is not an app entry we may replace.
        parkedHistoryRef.current = true
        return
      }
      // An accepted app arrival unparks history — BEFORE the
      // canonical-equality gate, so even a canonically equal
      // arrival clears the latch and later echoes can replace
      // normally.
      parkedHistoryRef.current = false
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
              { type, recordId, recordMode, scope,
                debouncedSearch, debouncedNotTerm,
                listFilters, boolFilters, hideExclusive,
                sortField, sortDir, currentPage }))))
        return
      // Invalidate outstanding list work synchronously FIRST
      // (the sequence bump, before any state assignment — the
      // listener runs synchronously, the refetch effect later),
      // then invalidate outstanding opens (navRef rewrite with
      // the new generation; attemptRef cleared so the arrival
      // effect will attempt the incoming target — the bug
      // class the two-ref split closes), then clear the old
      // form in both arms.
      ++listSeq.current
      // A scope transition must not leave the previous query's
      // records painted while the replacement request is
      // pending (plan 04): clear the old envelope
      // synchronously when the incoming scope differs (the
      // param form compares states; over-clearing on a case
      // variant is harmless — a refetch lands either way).
      if (c.fk !== scopeToParam(scope)) {
        setData(null)
      }
      ++navGen.current
      navRef.current = {
        generation: navGen.current,
        type: c.type ?? null,
        id: c.id ?? null,
        mode: c.id != null ? (c.mode ?? 'view') : null
      }
      attemptRef.current = null
      clearLoadedForm()
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
      // list. The desired record target is assigned from the
      // entry (with an id it defers opening to the arrival
      // effect until the target type's metadata arrives).
      setRecordId(c.id ?? null)
      setRecordMode(c.id != null ? (c.mode ?? 'view') : 'view')
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
      // Post scope (plan 04): full assignment from the
      // canonical entry — the existing full-assignment
      // protocol, now including scope (an omitted assignment
      // here would make the scope invisible to Back).
      setScope(c.fk != null ? parseScope(c.fk) : SCOPE_NONE)
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
  }, [loggedIn, type, types, recordId, recordMode, sortField,
      sortDir, debouncedSearch, debouncedNotTerm, currentPage,
      listFilters, boolFilters, hideExclusive, scope])

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

  // Elapsed time keyed by the loaded record's type + id (not
  // only by hasRunningStatus — running A → running B keeps that
  // boolean true and must not show A's elapsed on B).
  const runningKey = hasRunningStatus
    ? `${type}:${editRecordId}` : null
  useEffect(() => {
    if (runningKey != null && runningStartRef.current === null) {
      runningStartRef.current = Date.now()
      setElapsed(0)
    } else if (runningKey == null) {
      runningStartRef.current = null
      setElapsed(null)
    }
  }, [runningKey])

  // 1s ticker for elapsed display
  useEffect(() => {
    if (runningKey == null) return
    const ticker = setInterval(() => {
      if (runningStartRef.current !== null) {
        setElapsed(Math.floor(
          (Date.now() - runningStartRef.current) / 1000
        ))
      }
    }, 1000)
    return () => clearInterval(ticker)
  }, [runningKey])

  // 3s poll for record updates. Same-record request sequence +
  // attempt guard: an out-of-order response must not regress a
  // terminal status back to running, and a stale poll (record
  // closed / newer open / terminal decision) must not overwrite
  // a newer open or resurrect a closed form.
  useEffect(() => {
    if (!editRecordId || !type || !hasRunningStatus) return
    const attempt = attemptRef.current
    let pollSeq = 0
    const interval = setInterval(async () => {
      const seq = ++pollSeq
      try {
        const res = await apiFetch(
          `/api/item?type=${type}&id=${editRecordId}` +
          `&form=${openFormName}`
        )
        if (!res.ok) return
        const json = await res.json()
        const updated = json?.result?.record
        if (!updated) return
        if (seq !== pollSeq) return  // out-of-order poll
        if (attemptRef.current?.attemptId
            !== attempt?.attemptId) return  // stale attempt
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
        if (attemptRef.current?.attemptId
            === attempt?.attemptId) {
          fetchListRef.current()
        }
      } catch {
        // Network errors during polling are non-fatal
      }
    }, 3000)

    return () => clearInterval(interval)
  }, [editRecordId, hasRunningStatus, type, openFormName])

  // In settings mode, auto-enter edit mode with the single record.
  // Depends on `type` too: when switching to settings, viewMode changes
  // first (firing this effect with stale data), then type changes and
  // fetchList delivers the correct record. Without `type` in the deps,
  // the stale-data call sets isEditMode=true and the guard prevents the
  // correct record from ever loading.
  // A record permalink wins over the one-row convenience behavior:
  // while recordId is non-null the auto-open returns outright, and
  // the current settings target's latch is set BEFORE every
  // data/records early return so a Dismiss before the list arrives
  // cannot lose it to the no-records guard.
  useEffect(() => {
    if (viewMode !== 'settings') return
    // Arm the latch only while an explicit record target stands
    // (step 16's wording): a permalink to this settings record
    // suppresses the convenience auto-open. Without the
    // recordId guard the latch arms on the stale-data run of a
    // plain header entry (switchViewMode commits viewMode/type
    // before the list swaps) and the auto-open never fires.
    if (recordId != null
        && types.find(t => t.name === type)?.userSetting) {
      settingsAutoOpenSuppressedRef.current = type
    }
    if (recordId != null) return
    if (!data?.result?.records?.length) return
    if (data.result['type-key'] !== type) return
    if (isEditMode) return
    if (settingsAutoOpenSuppressedRef.current === type) return
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
    // button also gates on). The auto-open is a legacy non-URL
    // open: openEditForm allocates its nav/attempt identity
    // through the ordinary guarded path, but no url option —
    // the writer never encodes its id, and no push is marked.
    if (data.result.records[0]?.update === true) {
      openEditForm(data.result.records[0])
    } else if (data.result['view-form']) {
      openEditForm(data.result.records[0], true)
    }
  }, [data, viewMode, type, types, recordId, recordMode,
      isEditMode])

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
        <div className="app-header">
          <h1>
            <a href="#" onClick={(e) => {
              e.preventDefault()
              returnToLanding()
            }} style={{ textDecoration: 'none', color: 'inherit' }}>
              {title}
            </a>
          </h1>
          <div className="view-switcher">
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
        {/* recordError surface, no-data/error shell (step 18):
            a metadata failure or missing-form decision lands
            while data is null / unusable, so the surface must
            render here too — the form block belongs to the
            list-carrying render this shell never reaches.
            Terminal decisions clear the loaded form before
            setting recordError, so the box renders alone; Retry
            is by cause, Dismiss the close path (stranding rule
            included). */}
        {recordError != null && (
          <RecordErrorBox
            message={recordError.message}
            onRetry={retryRecord}
            onDismiss={dismissRecordError}
          />
        )}
        {/* Post-scope surfaces, no-data shell (plan 04): a
            fail-closed scope needs a surface here — the filter
            bar below belongs to the list-carrying render. A
            valid scope renders the generic Post × control
            (no metadata here to resolve a title); invalid
            structural input renders the neutral error with its
            Clear scope action. Both call clearScope; no
            invalid-scope path renders the previous list or an
            unscoped list. */}
        {scope.kind === 'valid' && (
          <ScopeChip title={null} onClear={clearScope} />
        )}
        {scope.kind === 'invalid' && (
          <InvalidScopeNotice onClear={clearScope} />
        )}
        <p style={{ color: listError ? 'var(--error)' : undefined }}>
          {listError ? (listError as ListErrorOwner).message
            : 'No records'}
        </p>
      </div>
    )
  }

  const listFields = formFieldKeys(data.result['list-form'])
  const records = data.result.records
  // Visible type names (lowercased) for the FK link gate; the
  // openPermalink callback routes an intercepted link click
  // through the same open path as a permalink arrival.
  const viewTypeNames = types.map(t => t.name.toLowerCase())
  const openPermalink = (t: string, id: string) => {
    // Already-open no-op: openEditForm's target check handles
    // it (same type + id re-open is guarded as identical).
    pendingUrlCause.current = 'push'
    openEditForm({ id }, true,
      { type: t, url: true, strict: true })
  }
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
  // A present scope counts (plan 04): a scope-only list lights
  // the button.
  const anyFilterActive =
    Object.values(listFilters).some(vals => vals.length > 0)
    || Object.keys(boolFilters).length > 0
    || scope.kind !== 'none'

  return (
    <div>
      <div className="app-header">
        <h1>
          <a href="#" onClick={(e) => {
            e.preventDefault()
            returnToLanding()
          }} style={{ textDecoration: 'none', color: 'inherit' }}>
            {title}
          </a>
        </h1>
        <div className="view-switcher">
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

      {/* Toolbar and filter row stand down while a record
          error owns the page (the dialog is the content; an Add
          button or filter controls above it are the same
          commingle the box exists to end). Rows below on
          non-hide-list types stay — a row click is a new open
          that clears the error. */}
      {!(showAddForm || isEditMode) && recordError == null && (
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
              // Parent-type Add clears the retained region spec
              // with editRecord (plan 03: cleared on the
              // setShowAddForm(true) sites).
              setChildSpec(null)
              // Add is a new form session: a lingering
              // recordError from a dismissed-then-abandoned
              // target must not ride along (the error box and
              // the Add form are mutually exclusive states).
              setRecordError(null)
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
      {!(showAddForm || isEditMode) && recordError == null &&
        (scope.kind !== 'none' || hasSearchable
         || hasChipFields || boolFilterFields.length > 0) && (
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
          {/* Post-scope chip (plan 04): a sibling of the active
              chip spans with its own × — never an entry in
              listFilters. "Post: <title> ×" when resolved,
              "Post ×" while unresolved or after a failed
              lookup; never the uuid. */}
          {scope.kind === 'valid' && (
            <ScopeChip title={scopeTitle} onClear={clearScope} />
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
                // Clear filters (plan 04): clears chips, bools,
                // AND the scope; restores the authored boolean
                // defaults; preserves q / notq; resets page and
                // selection; synchronously invalidates old list
                // work and marks the user push (both additions
                // in plan 04 — the old handler relied on the
                // dep-driven refetch and the writer's no-mark
                // push default). When it clears a scope it also
                // clears the old envelope — no stale scoped
                // rows under the pending unscoped refetch.
                const hadScope = scope.kind !== 'none'
                ++listSeq.current
                if (hadScope) {
                  setScope(SCOPE_NONE)
                  setData(null)
                }
                pendingUrlCause.current = 'push'
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

      {/* Failed-target surface (step 18): recordError implies
          editRecord == null (every error path clears the loaded
          form, and every new open / Add clears the error), so
          the box and a real form never coexist — it mounts here,
          alone, above where the form would sit. On hide-list
          types the list below is hidden (hideList reads the
          requested form), making this the page's only content. */}
      {recordError != null && editRecord == null && (
        <RecordErrorBox
          message={recordError.message}
          onRetry={retryRecord}
          onDismiss={dismissRecordError}
        />
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
                onClick={() => {
                  // View → Edit: a direct user open of the
                  // already-loaded record (URL-facing, same
                  // target, mode change).
                  pendingUrlCause.current = 'push'
                  openEditForm(editRecord, false,
                    { url: true })
                }}
              >
                Edit
              </button>
            )}
            {isViewMode && (
              <button
                type="button"
                onClick={copyBestLink}
                title={
                  typeof editRecord?.['public-url'] === 'string'
                  && editRecord['public-url'] !== ''
                    ? 'Copies the public page.'
                    : 'Copies the link to hand someone. The address bar may include filters.'}
                style={{ marginLeft: '0.5rem' }}
              >
                Best Link{copyState === 'copied'
                  ? ' — Copied'
                  : copyState === 'failed'
                    ? ' — Copy failed' : ''}
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
                    fieldMeta, formValues[f],
                    {
                      fieldKey: f,
                      record: formValues,
                      viewTypeList: viewTypeNames,
                      isViewForm: true,
                      open: openPermalink
                    }
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
              // Edit/Add read-only fields stay exactly as they
              // are — no ctx, no link branch.
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

      {/* Embedded child region (plan 03): under the parent's
          view form, outside every !hideList block (the flag
          suppresses the parent table, not the region). Keyed by
          the full region identity — parent id + normalized
          spec — so a parent or spec change unmounts the old
          instance and mounts a fresh one (page, expansion,
          Reply, request sequence, error state reset by
          construction). No footer link here: the scoped-tab
          hop is item 4. */}
      {isViewMode && childSpec != null
        && typeof editRecord?.id === 'string'
        && editRecord.id !== '' && (
        <Region
          key={[childSpec.type, childSpec.via,
               childSpec.sort.join(':'),
               editRecord.id].join('|')}
          spec={childSpec}
          parentId={editRecord.id}
          onShowAll={onShowAll}
        />
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