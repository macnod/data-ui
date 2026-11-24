import { useEffect, useRef, useState } from 'react'
import { Markdown } from './Markdown'

// View-form / page-body markdown with a table of contents
// (backlog "View-form TOC"). Wraps the shared Markdown
// component in a ref'd container and, after every commit,
// reads the *rendered* h2/h3/h4 elements out of the DOM —
// the parser's output is the truth, so a fenced "# not a
// heading" (renders as text) never makes a row and a setext
// "Text"/"---" (CommonMark §4.3: h2) does. Markdown.tsx
// itself is untouched: every other call site keeps today's
// behavior because the shared component never learns about
// the TOC.
//
// State holds plain {level, text, index} descriptors, never
// element references. React can replace a heading's DOM node
// without this wrapper knowing (react-markdown reparses on
// every render and its generated keys are tag name +
// per-parent occurrence), so a stored el would go stale and
// detached. Click-time resolution cannot: each row's onClick
// re-queries the container and scrolls the element at the
// stored index. Both the effect and the click handler run the
// identical query + filter (tocHeadings), so stored indices
// and click-time indices refer to the same sequence —
// empty-textContent headings are skipped on both sides.
//
// Delivery to state is effect-based and equality-preserving:
// setHeadings(prev => sameHeadings(prev, next) ? prev : next).
// Identical content returns the previous state reference,
// React bails out of the re-render, no new commit fires the
// effect again — the loop settles structurally (first mount:
// empty, then populated). No setter ever runs during render.
//
// Empty heading list renders no nav at all (a short field is
// just Markdown). The hash router is never touched: TOC rows
// are buttons, not links — writeUrl stays the only hash
// writer and the shareable record URL survives every click.

interface TocHeading {
  level: number
  text: string
  index: number
}

// The one shared query: h2/h3/h4 under the container,
// nonempty textContent only. Used by the effect (build the
// descriptors) and by every row click (resolve the element),
// so the index sequences can never disagree about the filter.
function tocHeadings(container: HTMLElement): HTMLElement[] {
  const els = container.querySelectorAll<HTMLElement>(
    'h2, h3, h4')
  const out: HTMLElement[] = []
  els.forEach(el => {
    if (el.textContent && el.textContent.trim() !== '') {
      out.push(el)
    }
  })
  return out
}

function sameHeadings(a: TocHeading[], b: TocHeading[]): boolean {
  if (a.length !== b.length) return false
  return a.every((h, i) => h.level === b[i].level
    && h.text === b[i].text
    && h.index === b[i].index)
}

export function MarkdownWithToc({ text }: { text: string }) {
  const containerRef = useRef<HTMLDivElement>(null)
  const [headings, setHeadings] = useState<TocHeading[]>([])

  useEffect(() => {
    const container = containerRef.current
    if (!container) return
    const els = tocHeadings(container)
    const next: TocHeading[] = els.map((el, i) => ({
      level: Number(el.tagName.slice(1)),
      text: (el.textContent || '').trim(),
      index: i
    }))
    // Equality-guarded: identical descriptors return the
    // previous state reference (contents compared, never
    // array identity — an identity compare would reintroduce
    // the render loop in new clothes), React bails out, and
    // the effect/commit cycle settles.
    setHeadings(prev =>
      sameHeadings(prev, next) ? prev : next)
  })

  const scrollTo = (index: number) => {
    const container = containerRef.current
    if (!container) return
    const els = tocHeadings(container)
    els[index]?.scrollIntoView({ behavior: 'smooth' })
  }

  if (headings.length === 0) {
    return (
      <div ref={containerRef}>
        <Markdown text={text} />
      </div>
    )
  }

  return (
    <div ref={containerRef} className="markdown-toc-row">
      <Markdown text={text} />
      <nav className="toc" aria-label="Table of contents">
        {headings.map(h => (
          <button
            key={h.index}
            type="button"
            className={`toc-l${h.level}`}
            onClick={() => scrollTo(h.index)}
          >{h.text}</button>
        ))}
      </nav>
    </div>
  )
}
