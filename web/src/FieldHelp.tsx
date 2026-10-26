import { useEffect, useId, useRef, useState } from 'react'
import { Markdown } from './Markdown'

// Per-field help (:ui :help) toggle for the Add / Edit / View
// forms: a small "?" button after the field label, collapsed by
// default, content rendered with the shared Markdown component.
// Component-local state (one open panel per mounted field row;
// closing the form unmounts it). Escape closes the open panel;
// click-outside closes it but must ignore the "?" button's own
// node (the toggle in its handler already handled that click).
export function FieldHelp({ text, label }: {
  text: string
  label: string
}) {
  const [open, setOpen] = useState(false)
  const btnRef = useRef<HTMLButtonElement>(null)
  const panelId = useId()
  const ariaLabel = open ? `Hide help for ${label}`
    : `Help for ${label}`

  useEffect(() => {
    if (!open) return
    const onKey = (e: KeyboardEvent) => {
      if (e.key === 'Escape') setOpen(false)
    }
    const onClick = (e: MouseEvent) => {
      if (btnRef.current && btnRef.current.contains(e.target as Node))
        return
      setOpen(false)
    }
    document.addEventListener('keydown', onKey)
    document.addEventListener('mousedown', onClick)
    return () => {
      document.removeEventListener('keydown', onKey)
      document.removeEventListener('mousedown', onClick)
    }
  }, [open])

  return (
    <span className="field-help">
      <button
        ref={btnRef}
        type="button"
        className="field-help-toggle"
        aria-expanded={open}
        aria-controls={panelId}
        aria-label={open ? `Hide help for ${label}`
          : `Help for ${label}`}
        title={ariaLabel ? ariaLabel : undefined}
        onClick={() => setOpen(!open)}
      >?</button>
      {open && (
        <div id={panelId} className="field-help-panel">
          <Markdown text={text} />
        </div>
      )}
    </span>
  )
}
