// SavedViewsBar — the saved-list-view controls for a record list.
//
//   SavedViewsRow     the names of the current type's saved views,
//                     each a chip that is a real link, with its ×
//   SaveViewControl   the "Save view" button and its popover
//
// Purely presentational: props in, no fetches. App owns the rows,
// the writes, the derived highlight, and which popover is open (the
// one-popover-at-a-time contract with the chip pickers and the
// Columns control).
//
// Each name is a real link to the view's hash. Applying a view is
// the browser following a fragment link: it pushes one history entry
// and fires hashchange, and the existing listener performs the whole
// assignment — the same path a pasted or hand-edited URL takes. The
// app writes no hash here (urlState.ts's writeUrl stays the only
// writer), and Copy link address, open-in-new-tab, and Back come
// with the link.
//
// The highlight is derived by the caller, never stored: a view is
// selected while its query equals the live list query. Changing a
// chip, the sort, the search, the scope, or hide-exclusive clears it
// by construction; paging keeps it.

import { useEffect, useRef, useState } from 'react'
import { planSave } from './savedViews'
import type { SavedView } from './savedViews'

export interface SavedViewLink {
  id: string
  name: string
  href: string
  // selected: the view's query equals the live view-query (the
  // canonical state without the open record and the page).
  selected: boolean
  // current: the view's query equals the live state in full — the
  // link's target is the hash already in the address bar.
  current: boolean
}

export function SavedViewsRow({ views, onDelete }: {
  views: SavedViewLink[]
  onDelete: (id: string) => void
}) {
  return (
    <div style={{
      display: 'flex', flexWrap: 'wrap', alignItems: 'center',
      gap: '0.35rem', marginBottom: '0.5rem'
    }}>
      {views.map(v => (
        <span key={v.id}
          style={{ display: 'inline-flex', alignItems: 'center' }}>
          {/* A real <a>: modifier-click, middle-click, and
              open-in-new-tab keep native behavior. Only an
              unmodified left click on the view already applied is
              intercepted — a no-op, so no canonically equal entry
              that differs only in filter key order is pushed. */}
          <a
            className={v.selected ? 'chip chip-active' : 'chip'}
            href={v.href}
            title={v.name}
            aria-current={v.selected ? 'true' : undefined}
            style={{ textDecoration: 'none' }}
            onClick={e => {
              if (!v.current) return
              if (e.metaKey || e.ctrlKey || e.shiftKey || e.altKey
                  || e.button !== 0) return
              e.preventDefault()
            }}
          >
            {v.name}
          </a>
          <button
            type="button"
            className="chip-x"
            aria-label={'Delete view ' + v.name}
            title={'Delete view ' + v.name}
            style={{ fontSize: '0.85em' }}
            onClick={() => onDelete(v.id)}
          >×</button>
        </span>
      ))}
    </div>
  )
}

// SaveViewControl — the ColumnsPicker anatomy: a relative wrapper
// holding the button and, while open, an absolutely positioned
// panel; a document mousedown outside the wrapper closes (the
// listener lives only while open). The panel is a form with one
// input: Enter submits, Escape closes. The input starts empty each
// time the panel opens.
//
// The submit button reads Replace when the typed name is one of
// the current type's views (an exact match) and Save otherwise,
// and is disabled for a blank name or while a save is in flight.
// App's write and this label read the same plan (planSave).

export function SaveViewControl({ views, open, onOpenChange, onSave }: {
  views: SavedView[]          // the current type's views
  open: boolean
  onOpenChange: (open: boolean) => void
  onSave: (rawName: string) => Promise<void>
}) {
  const [name, setName] = useState('')
  const [saving, setSaving] = useState(false)
  const wrapRef = useRef<HTMLDivElement | null>(null)

  useEffect(() => {
    if (open) setName('')
  }, [open])

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

  const plan = planSave(views, name)

  const onSubmit = (e: React.FormEvent) => {
    e.preventDefault()
    if (plan.kind === 'none' || saving) return
    setSaving(true)
    // onSave reports its own failures (an alert) and never leaves
    // the panel stuck: the in-flight flag clears either way.
    Promise.resolve(onSave(name))
      .catch(() => {})
      .finally(() => setSaving(false))
  }

  return (
    <div ref={wrapRef} style={{ position: 'relative' }}>
      <button
        type="button"
        aria-expanded={open ? 'true' : 'false'}
        onClick={() => onOpenChange(!open)}
        style={{ padding: '0.25rem 0.5rem' }}
      >Save view</button>
      {open && (
        <form
          onSubmit={onSubmit}
          style={{
            position: 'absolute',
            top: '100%',
            right: 0,
            zIndex: 10,
            marginTop: '0.25rem',
            padding: '0.5rem',
            border: '1px solid var(--border)',
            borderRadius: '6px',
            background: 'var(--bg)',
            display: 'flex',
            alignItems: 'center',
            gap: '0.35rem',
            boxShadow: '0 2px 8px rgba(0,0,0,0.2)'
          }}
        >
          <input
            type="text"
            // eslint-disable-next-line jsx-a11y/no-autofocus
            autoFocus
            value={name}
            maxLength={80}
            placeholder="View name"
            aria-label="View name"
            onChange={e => setName(e.target.value)}
            onKeyDown={e => {
              if (e.key === 'Escape') onOpenChange(false)
            }}
            style={{ padding: '0.35rem 0.5rem', width: '12rem' }}
          />
          <button
            type="submit"
            disabled={plan.kind === 'none' || saving}
          >
            {plan.kind === 'update' ? 'Replace' : 'Save'}
          </button>
          <button type="button" onClick={() => onOpenChange(false)}>
            Cancel
          </button>
        </form>
      )}
    </div>
  )
}
