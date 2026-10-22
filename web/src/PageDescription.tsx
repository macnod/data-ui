import { Markdown } from './Markdown'

// Type-level intro block for the list page (:page-description).
// Controlled: open state lives in App (per-type map) so list
// refetches do not snap it back to the model default. Collapsed
// pill carries the affordance in visible text, not just a hover
// title; aria-expanded and the title flip with the state while
// the label stays stable.
export function PageDescription({ text, open, onToggle }: {
  text: string
  open: boolean
  onToggle: () => void
}) {
  return (
    <div style={{ marginBottom: '0.75rem' }}>
      <button
        type="button"
        aria-expanded={open}
        title={open ? 'Hide page description'
          : 'Show page description'}
        onClick={onToggle}
        style={{
          padding: '0.15rem 0.6rem',
          border: '1px solid var(--border)',
          borderRadius: '999px',
          background: 'var(--tab-inactive-bg)',
          cursor: 'pointer',
          fontSize: '0.9rem'
        }}
      >ℹ️ About this page</button>
      {open && (
        <div className="page-description"
          style={{
            padding: '0.5rem 0.75rem',
            marginTop: '0.25rem',
            marginBottom: '0.25rem',
            border: '1px solid var(--border)',
            borderRadius: '4px',
            background: 'var(--tab-inactive-bg)'
          }}>
          <Markdown text={text} />
        </div>
      )}
    </div>
  )
}
