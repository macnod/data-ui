// ChipPicker — searchable chip-filter picker for list views.
//
// Cell chips only exist for values on the current page; this
// control is how you name a value that is not on the page. One
// compact icon per chip-eligible field in the filter bar; click
// expands a searchable dropdown over the field's full palette
// (=allowed-values= from /api/list — the same palette the
// edit-form CheckboxList uses).
//
// Purely presentational, same split as CheckboxList: props in,
// no fetch. Toggling a value calls the caller's onToggle (App
// passes toggleChipFilter); the refetch flows through the
// existing listFilters effect.
//
// Component structure (click-outside contract):
//
//   wrapper (position: relative)          ← wrapRef
//     button (icon)                       ← the rest control
//     panel (position: absolute)          ← panelRef
//       input (Search…)
//       pinned selected rows (never capped, never filtered)
//       capped unselected match window
//       "N more — type to narrow"
//
// The document-level mousedown listener ignores events inside
// wrapperRef (button AND panel): a mousedown on a panel row
// must not close before the row's click dispatches, and
// clicking the search input to re-focus it must not close
// either. mousedown (not click) so closing removes the
// listener before the same interaction's click arrives and
// re-toggles the button.

import { useEffect, useRef, useState } from 'react'

// Maximum unselected matches rendered at once — the
// alphabetical head of the match set (CheckboxList's RENDER_CAP
// idea, smaller because this is a dropdown, not a form
// section). A constant, not a prop and not a model key.
const RENDER_CAP = 8

interface ChipPickerProps {
  label: string
  options: string[]
  selected: string[]
  open: boolean
  onOpenChange: (open: boolean) => void
  onToggle: (value: string) => void
}

function ChipPicker({ label, options, selected, open,
                      onOpenChange, onToggle }: ChipPickerProps) {
  const [query, setQuery] = useState('')
  const wrapRef = useRef<HTMLDivElement | null>(null)

  // Close on any mousedown outside this picker's wrapper
  // (button + panel; the panel is a different element from the
  // button, so a button-only stopPropagation would not cover
  // it). Registered while open, removed on close.
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
  const selectedSet = new Set(selected)

  // Pinned section: selected values in full — never filtered,
  // never capped (CheckboxList's rule). A selected value
  // alphabetically past the unselected window must still be
  // clickable here, or toggle-off fails for exactly the values
  // the cap hides.
  const pinned = [...selected].sort((a, b) => a.localeCompare(b))

  // Unselected window: case-insensitive substring match on the
  // raw label, sorted (localeCompare), then capped.
  const unselected = options
    .filter(o => !selectedSet.has(o))
    .sort((a, b) => a.localeCompare(b))
  const matching = trimmed
    ? unselected.filter(o => o.toLowerCase().includes(trimmed))
    : unselected
  const visible = matching.slice(0, RENDER_CAP)
  const hidden = matching.length - visible.length

  // Enter adds, never removes: toggle the first visible
  // unselected match, and only on a non-empty query — an empty
  // query plus a fat-fingered Enter must not silently filter
  // to whichever value sorts first. Deliberately not
  // Enter-reachable on the pinned section; mouse on the pinned
  // row or the filter-bar × un-picks.
  const onSearchKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Escape') {
      onOpenChange(false)
      return
    }
    if (e.key === 'Enter') {
      if (trimmed && visible.length > 0) {
        onToggle(visible[0])
        onOpenChange(false)
      }
    }
  }

  const row = (val: string, isPinned: boolean) => (
    <button
      key={val}
      type="button"
      className={isPinned ? 'chip chip-active' : 'chip'}
      onClick={() => {
        onToggle(val)
        onOpenChange(false)
      }}
      style={{ textAlign: 'left' }}
    >{val}</button>
  )

  return (
    <div ref={wrapRef} style={{ position: 'relative' }}>
      <button
        type="button"
        title={`Filter by ${label}`}
        aria-label={`Filter by ${label}`}
        aria-expanded={open ? 'true' : 'false'}
        onClick={() => onOpenChange(!open)}
        style={{ padding: '0.25rem 0.5rem' }}
      >🏷</button>
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
          <div style={{ color: 'var(--muted)' }}>{label}</div>
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
              display: 'flex', flexWrap: 'wrap', gap: '0.25rem'
            }}>
              {pinned.map(v => row(v, true))}
            </div>
          )}
          <div style={{
            display: 'flex', flexWrap: 'wrap', gap: '0.25rem'
          }}>
            {visible.map(v => row(v, false))}
          </div>
          {hidden > 0 && (
            <div style={{
              color: 'var(--muted)', fontSize: '0.85em'
            }}>
              {hidden} more — type to narrow
            </div>
          )}
          {matching.length === 0 && (
            <div style={{
              color: 'var(--muted)', fontSize: '0.85em'
            }}>
              No matching values
            </div>
          )}
        </div>
      )}
    </div>
  )
}

export default ChipPicker
