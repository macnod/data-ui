import ReactMarkdown from 'react-markdown'
import remarkGfm from 'remark-gfm'

// Shared markdown renderer: the :markdown widget now, :page-description
// and :help later. No rehype-raw and no custom urlTransform —
// react-markdown's defaults escape raw HTML and strip dangerous href
// schemes. The wrapper div carries styling (v9+ dropped className on
// the component).
export function Markdown({ text }: { text: string }) {
  return (
    <div style={{ lineHeight: 1.5 }}>
      <ReactMarkdown
        remarkPlugins={[remarkGfm]}
        components={{
          a: ({ node, ...props }) => (
            <a {...props}
              target="_blank"
              rel="noopener noreferrer" />
          )
        }}
      >
        {text}
      </ReactMarkdown>
    </div>
  )
}
