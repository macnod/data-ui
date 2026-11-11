// A chrome slot (banner / footer) from an /api/info result, or
// null. A non-string or blank value is null so a removed slot
// clears on the next fetch.
export function slotFromInfo(result: unknown, key: string): string | null {
  if (result == null || typeof result !== 'object') return null
  const v = (result as Record<string, unknown>)[key]
  return typeof v === 'string' && v.trim() !== '' ? v : null
}

// The heading slot's style declarations as a React style object, or
// null. The slot value is a single styled span (sanitized at write
// and again at read); lift its style attribute, split declarations,
// kebab-case to camelCase. Anything unexpected is null — a broken
// row must not take the page down. A fragment with more than one
// styled span is non-canonical but legal; the first style
// attribute wins.
export function headingStyleFromInfo(
  html: string | null
): React.CSSProperties | null {
  if (typeof html !== 'string') return null
  const m = html.match(/style="([^"]*)"/)
  if (m == null || m[1].trim() === '') return null
  const style: React.CSSProperties = {}
  for (const decl of m[1].split(';')) {
    const i = decl.indexOf(':')
    if (i < 1) continue
    const prop = decl.slice(0, i).trim()
    const value = decl.slice(i + 1).trim()
    if (prop === '' || value === '') continue
    const key = prop.replace(/-([a-z])/g, (_, c) => c.toUpperCase())
    ;(style as Record<string, string>)[key] = value
  }
  return Object.keys(style).length > 0 ? style : null
}
