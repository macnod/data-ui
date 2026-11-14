// ColumnsPicker — column-visibility picker for record lists.
//
// A copy of ChipPicker's documented structure (wrapper /
// button / panel, document-mousedown close-outside registered
// while open, autofocus search input, capped match window with
// "N more — type to narrow", an empty-match state, Escape
// close), with three deltas the anatomy doc calls out:
//
//   1. rows are native label-wrapped checkboxes, and toggling
//      never closes the popover (multiple toggles per open —
//      ChipPicker's onToggle closes on every row click, chip-
//      filter-shaped semantics a checkbox variant would fork
//      mid-component);
//   2. a "Show all" footer renders when anything is hidden
//      (and never closes either);
//   3. the pinned section is the *hidden* set — the deviation
//      from all-visible, the direct analogue of ChipPicker's
//      selected-values pinning. Checked means visible; the
//      pinned rows are the unchecked ones. A hidden field
//      past the cap window must stay reachable there, or
//      un-hiding fails for exactly the fields the cap hides.
//
// Purely presentational: props in, no localStorage (App owns
// storage). The ✕ delete-selection column renders as its own
// bottom group — never filtered by the search, never touched
// by "Show all" (App enforces that by key separation).
//
// Component structure (click-outside contract):
//
//   wrapper (position: relative)          ← wrapRef
//     button (▦)                          ← the rest control
//     panel (position: absolute)
//       input (Search…)
//       pinned hidden rows (never capped, never filtered)
//       capped visible match window
//       "N more — type to narrow"
//       divider + "Delete column (✕)" row
//       "Show all" footer
//
// The document-level mousedown listener ignores events inside
// wrapperRef (button AND panel): a mousedown on a panel row
// must not close before the row's change dispatches, and
// clicking the search input to re-focus it must not close
// either. mousedown (not click) so closing removes the
// listener before the same interaction's click arrives and
// re-toggles the button.

import { useEffect, useRef, useState } from 'react'

// Maximum visible-field matches rendered at once (ChipPicker's
// RENDER_CAP idea). The pinned section is never capped, so the
// cap can never make a hidden field unreachable.
const RENDER_CAP = 8

interface ColumnsPickerProps {
  fields: { name: string, label: string }[] // schema order
  hidden: string[]          // hidden field names
  showDeleteEntry: boolean  // anyDeletable — page truth
  deleteColumnShown: boolean
  open: boolean
  onOpenChange: (open: boolean) => void
  onToggleField: (name: string) => void
  onToggleDeleteColumn: () => void
  onShowAll: () => void
}

function ColumnsPicker({ fields, hidden, showDeleteEntry,
                         deleteColumnShown, open, onOpenChange,
                         onToggleField, onToggleDeleteColumn,
                         onShowAll }: ColumnsPickerProps) {
  const [query, setQuery] = useState('')
  const wrapRef = useRef<HTMLDivElement | null>(null)

  // Close on any mousedown outside this picker's wrapper
  // (button + panel), registered while open, removed on close.
  useEffect(() => {
    if (!open) return
    const onMouseDown = (e: MouseEvent) => {
      if (wrapRef.current
          && !wrapRef.current.contains(e.target as Node)) {
        onOpenChange(false)
      }
    }
    document.addEventListener('mousedown', onMouseDown)
    return () => {
      document.removeEventListener('mousedown', onMouseDown)
    }
  }, [open, onOpenChange])

  const trimmed = query.trim().toLowerCase()
  const hiddenSet = new Set(hidden)

  // Pinned section: hidden fields in schema order — never
  // filtered, never capped, rendered unchecked (checked means
  // visible). Schema order everywhere: the table's column
  // order is the user's mental model (ChipPicker's
  // alphabetical sort serves value sets, which have no
  // canonical order; columns have one).
  const pinned = fields.filter(f => hiddenSet.has(f.name))

  // Match window: visible fields, schema order, a
  // case-insensitive substring on the label with the field
  // name as a second match surface ("created" finds the
  // Created label, "created-at" finds the key), then capped.
  const unhidden = fields.filter(f => !hiddenSet.has(f.name))
  const matching = trimmed
    ? unhidden.filter(f =>
        f.label.toLowerCase().includes(trimmed)
        || f.name.toLowerCase().includes(trimmed))
    : unhidden
  const visible = matching.slice(0, RENDER_CAP)
  const more = matching.length - visible.length

  const onSearchKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Escape') {
      onOpenChange(false)
    }
  }

  const rowStyle: React.CSSProperties = {
    display: 'flex', gap: '0.35rem', alignItems: 'center',
    whiteSpace: 'nowrap', cursor: 'pointer'
  }

  // Checked = visible. Toggling never closes the popover —
  // the checkbox delta ChipPicker's close-on-toggle contract
  // is forked for.
  const row = (name: string, label: string,
               checked: boolean) => (
    <label key={name} style={rowStyle}>
      <input
        type="checkbox"
        checked={checked}
        onChange={() => onToggleField(name)}
      />
      {label}
    </label>
  )

  return (
    <div ref={wrapRef} style={{ position: 'relative' }}>
      <button
        type="button"
        title="Columns"
        aria-label="Columns"
        aria-expanded={open ? 'true' : 'false'}
        onClick={() => onOpenChange(!open)}
        style={{ padding: '0.25rem 0.5rem' }}
      >▦</button>
      {open && (
        <div
          style={{
            position: 'absolute',
            top: '100%',
            left: 0,
            zIndex: 10,
            marginTop: '0.25rem',
            padding: '0.5rem',
            border: '1px solid var(--border)',
            borderRadius: '6px',
            background: 'var(--bg)',
            display: 'flex',
            flexDirection: 'column',
            gap: '0.25rem',
            minWidth: '14rem',
            maxHeight: '18rem',
            overflowY: 'auto',
            boxShadow: '0 2px 8px rgba(0,0,0,0.2)'
          }}
        >
          <div style={{ color: 'var(--muted)' }}>Columns</div>
          <input
            type="text"
            // eslint-disable-next-line jsx-a11y/no-autofocus
            autoFocus
            value={query}
            onChange={e => setQuery(e.target.value)}
            onKeyDown={onSearchKeyDown}
            placeholder="Search…"
            style={{ padding: '0.35rem 0.5rem' }}
          />
          {pinned.length > 0 && (
            <div style={{
              display: 'flex', flexDirection: 'column',
              gap: '0.25rem'
            }}>
              {pinned.map(f => row(f.name, f.label, false))}
            </div>
          )}
          <div style={{
            display: 'flex', flexDirection: 'column',
            gap: '0.25rem'
          }}>
            {visible.map(f => row(f.name, f.label, true))}
          </div>
          {more > 0 && (
            <div style={{
              color: 'var(--muted)', fontSize: '0.85em'
            }}>
              {more} more — type to narrow
            </div>
          )}
          {matching.length === 0 && (
            <div style={{
              color: 'var(--muted)', fontSize: '0.85em'
            }}>
              No matching columns
            </div>
          )}
          {/* The ✕ entry: its own bottom group, never filtered
              by the search, never touched by "Show all" (App's
              key separation enforces the amendment — the reset
              governs field columns only). Gated on the same
              page-level truth the column itself uses. */}
          {showDeleteEntry && (
            <>
              <div style={{
                borderTop: '1px solid var(--border)',
                margin: '0.25rem 0'
              }} />
              <label style={rowStyle}>
                <input
                  type="checkbox"
                  checked={deleteColumnShown}
                  onChange={() => onToggleDeleteColumn()}
                />
                Delete column (✕)
              </label>
            </>
          )}
          {hidden.length > 0 && (
            <button
              type="button"
              onClick={onShowAll}
              style={{
                alignSelf: 'flex-start',
                padding: '0.25rem 0.5rem'
              }}
            >
              Show all
            </button>
          )}
        </div>
      )}
    </div>
  )
}

export default ColumnsPicker
