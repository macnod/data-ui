// SearchSelect — searchable single-select picker for :target
// (single-record relation) fields with :widget :search-select.
// The CheckboxList twin: same selected-above / search /
// unselected-below anatomy, with radios (mutually exclusive
// options) in place of checkboxes and the selected section
// capped at one row. Picking an option below moves it to the top
// and drops the old selection back into the list.
//
// - The current selection always renders above the list as a
//   checked radio row — never filtered, even when absent from
//   the options palette (the CheckboxList contract for
//   palette-absent values). Clicking that row (or its ✕) clears
//   the value to null, which submits JSON null and lands as SQL
//   NULL. Only a nullable field is clearable (clearable=false on
//   :not-null — the wire :not-null flag gates it; required
//   targets keep today's un-clearable behavior). React note:
//   clicking an already-checked radio fires no change event, so
//   the clear rides onClick (readOnly silences the controlled
//   checked-without-onChange warning).
// - A search box appears when the option set reaches
//   SEARCH_THRESHOLD. Below it, all options render flat.
// - Unselected options filter case-insensitively by substring in
//   a scroll box capped at RENDER_CAP rendered rows ("N more —
//   type to narrow"). Clicking an option selects it and clears
//   the query.
// - Keyboard support is native only (Tab / Space); proper
//   combobox keyboard navigation for the shared picker anatomy
//   is tracked as a Post MVP backlog item.

import { useState } from 'react'
import {
  SEARCH_THRESHOLD, RENDER_CAP
} from './CheckboxList'

const SCROLL_MAX_HEIGHT = '7.5rem'

interface SearchSelectProps {
  options: string[]
  value: string | null
  clearable: boolean
  onChange: (v: string | null) => void
}

function SearchSelect({
  options, value, clearable, onChange
}: SearchSelectProps) {
  const [query, setQuery] = useState('')
  const trimmed = query.trim().toLowerCase()

  // Sort before the cap slice so the visible window is the
  // alphabetical head of the match set.
  const candidates = options
    .filter(o => o !== value)
    .sort((a, b) => a.localeCompare(b))
  const matching = trimmed
    ? candidates.filter(
        o => o.toLowerCase().includes(trimmed))
    : candidates
  const visible = matching.slice(0, RENDER_CAP)
  const hidden = matching.length - visible.length

  const showSearch =
    (value ? 1 : 0) + candidates.length >= SEARCH_THRESHOLD

  const pick = (val: string) => {
    onChange(val)
    setQuery('')
  }

  const clear = () => {
    if (clearable) onChange(null)
  }

  return (
    <div>
      {/* Current selection: at most one radio row, always
          rendered in full — even when absent from the options
          palette. Clicking the checked radio clears (nullable
          fields only), the CheckboxList toggle-off counterpart. */}
      {value && (
        <div style={{ display: 'flex', alignItems: 'center' }}>
          <label style={{ display: 'block', marginLeft: '1rem' }}>
            <input
              type="radio"
              checked
              readOnly
              onClick={clear}
            />
            {value}
          </label>
          {clearable && (
            <button
              type="button"
              title="Clear"
              aria-label="Clear selection"
              onClick={clear}
              style={{ marginLeft: '0.5rem', padding: '0 0.4rem' }}
            >
              ✕
            </button>
          )}
        </div>
      )}
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
        {visible.map(val => (
          <label
            key={val}
            style={{ display: 'block', marginLeft: '1rem' }}
          >
            <input
              type="radio"
              checked={false}
              onChange={() => pick(val)}
            />
            {val}
          </label>
        ))}
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

export default SearchSelect
