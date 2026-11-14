// Column visibility — pure decisions for the list Columns
// control. Storage side effects stay in App.tsx; this module
// is the testable core.

export const columnsStorageKey =
  (model: string, type: string) =>
    `data-ui-hidden-columns:${model}:${type}`

export const deleteColumnStorageKey =
  (model: string, type: string) =>
    `data-ui-delete-column:${model}:${type}`

// Absent / corrupt / non-array → [] (all visible). String
// members only; duplicates pass through — the render's Set
// makes them harmless. Callers intersect with the live
// listFields, so names no longer in the schema are ignored
// by construction.
export const parseHiddenColumns =
  (raw: string | null): string[] => {
    if (!raw) return []
    try {
      const parsed = JSON.parse(raw)
      if (!Array.isArray(parsed)) return []
      return parsed.filter(
        (x: unknown) => typeof x === 'string')
    } catch {
      return []
    }
  }

export const visibleListFields = (
  listFields: string[], hidden: string[]
): string[] => {
  const hiddenSet = new Set(hidden)
  return listFields.filter(f => !hiddenSet.has(f))
}

// Form presence is a rollup *proxy*, not an ordinary-list
// signal: rollups emit only list-form (the compiler forbids
// the other forms on :rollup types), but an ordinary type
// may also legally declare no forms — the two shapes are
// wire-identical and no response key distinguishes them.
// The proxy never includes a rollup (forms are never present
// there) but over-excludes form-less ordinary types —
// accepted v1 scope (declare a :view-form to opt back in).
// Page-type responses also carry forms but never reach the
// list path (the dead-shell return renders them first).
export const supportsColumnVisibility = (
  result: Record<string, any> | undefined
): boolean =>
  !!(result?.['add-form'] || result?.['update-form']
     || result?.['view-form'])
