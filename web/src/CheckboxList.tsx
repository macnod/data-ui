// CheckboxList — shared multi-select rendering for every
// checkbox-list field (M2M tags, roles, permissions, ...).
//
// Purely presentational: {options, selected, onToggle} props plus
// an internal client-side search query. No server round-trip.
//
// - Selected items always render in full — never filtered, never
//   capped. Values on the record but absent from options still
//   render checked (replaces the old extraRoles seeding for roles).
// - A search box appears when the option set (selected ∪
//   unselected) reaches SEARCH_THRESHOLD. Below it, all options
//   render flat.
// - Unselected options render inside a compact scroll box (~5
//   visible rows, capped at RENDER_CAP rendered rows); typing
//   narrows (case-insensitive substring on the raw label). The box
//   shrink-wraps to its content so the scrollbar sits beside the
//   list, not at the screen edge. When rows are cut by the cap, a
//   muted "N more — type to narrow" hint shows below.
// - Labels render raw (e.g. alice:exclusive displays as-is).
// - Both sections sort (localeCompare), so every checkbox list
//   renders sorted.

import { useState } from 'react'

// Search box appears when the option set (selected ∪ unselected)
// reaches this size. Below it, every option renders flat.
const SEARCH_THRESHOLD = 10
// Maximum unselected options rendered at once. Typing narrows.
const RENDER_CAP = 100
// Scroll box height, sized to show ~5 rows (rows render at
// roughly 22-24px: default font, one checkbox + label per row).
const SCROLL_MAX_HEIGHT = '7.5rem'

interface CheckboxListProps {
  options: string[]
  selected: string[]
  onToggle: (value: string, checked: boolean) => void
}

function CheckboxList({ options, selected, onToggle }: CheckboxListProps) {
  const [query, setQuery] = useState('')
  const trimmed = query.trim().toLowerCase()
  const selectedSet = new Set(selected)

  // Selected items always render in full — never filtered, never
  // capped. Values on the record but absent from options still
  // render checked (replaces the old extraRoles seeding).
  const selectedItems = [...selected].sort((a, b) => a.localeCompare(b))

  // Sort before the cap slice so the visible window is the
  // alphabetical head of the match set.
  const unselected = options
    .filter(o => !selectedSet.has(o))
    .sort((a, b) => a.localeCompare(b))
  const matching = trimmed
    ? unselected.filter(o => o.toLowerCase().includes(trimmed))
    : unselected
  const visible = matching.slice(0, RENDER_CAP)
  const hidden = matching.length - visible.length

  const showSearch =
    selectedSet.size + unselected.length >= SEARCH_THRESHOLD

  const row = (val: string, checked: boolean) => (
    <label key={val} style={{ display: 'block', marginLeft: '1rem' }}>
      <input
        type="checkbox"
        checked={checked}
        onChange={e => onToggle(val, e.target.checked)}
      />
      {val}
    </label>
  )

  return (
    <div>
      {selectedItems.map(v => row(v, true))}
      {showSearch && (
        <div style={{ marginLeft: '1rem', marginTop: '0.25rem' }}>
          <input
            type="text"
            value={query}
            onChange={e => setQuery(e.target.value)}
            placeholder="Search..."
            style={{ width: '200px' }}
          />
        </div>
      )}
      {/* Shrink-wrapped (fit-content) so the scrollbar sits right
          beside the rows instead of at the screen edge; minWidth
          matches the search input so a short list still looks
          intentional. */}
      <div style={{
        maxHeight: SCROLL_MAX_HEIGHT,
        overflowY: 'auto',
        width: 'fit-content',
        minWidth: '200px'
      }}>
        {visible.map(v => row(v, false))}
      </div>
      {hidden > 0 && (
        <div style={{
          marginLeft: '1rem',
          color: 'var(--muted)',
          fontSize: '0.85em'
        }}>
          {hidden} more — type to narrow
        </div>
      )}
    </div>
  )
}

export default CheckboxList
