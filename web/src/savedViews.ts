// Saved list views — the list logic beside urlState.ts's format
// helpers. One row of the hidden built-in saved-views type is one
// named, per-user list query: { id, name, listType, query }. The
// query is the canonical hash query string without the "#/app?"
// prefix (serializeUrlState's output), so applying a view is
// following a link to that hash.
//
// This module imports nothing: the Node test transpiles it and
// imports it from a data URL (the chipNav.test.mjs pattern). The
// URL format itself stays in urlState.ts — nothing here parses or
// serializes a query.

export interface SavedView {
  id: string
  name: string
  listType: string
  query: string
}

// What one Save resolves to: nothing to save (an empty name), an
// update of the view that already holds that exact name (Replace),
// or a new row. The Save control's button label and App's write
// both read the same plan, so they cannot drift.
export type SavePlan =
  | { kind: 'none' }
  | { kind: 'insert', name: string }
  | { kind: 'update', id: string, name: string }

// Read one wire record (a /api/list row: id, name, list-type,
// query). Null unless all four are non-empty strings — a row
// failing any check has nothing to show or to apply.
export function viewFromRecord(rec: any): SavedView | null {
  if (rec == null || typeof rec !== 'object') return null
  const id = rec.id
  const name = rec.name
  const listType = rec['list-type']
  const query = rec.query
  if (typeof id !== 'string' || id === ''
      || typeof name !== 'string' || name === ''
      || typeof listType !== 'string' || listType === ''
      || typeof query !== 'string' || query === '') {
    return null
  }
  return { id, name, listType, query }
}

// The current type's views, sorted by name. Case-sensitive on
// both counts: the server's identity is (user, list type, name)
// behind a case-sensitive index. Returns a new array; the input
// is neither filtered in place nor reordered.
export function viewsForType<T extends SavedView>(
  views: T[], type: string): T[] {
  return views
    .filter(v => v.listType === type)
    .sort((a, b) => a.name < b.name ? -1 : a.name > b.name ? 1 : 0)
}

// Trim, and nothing else: the name is the user's own, case and
// inner whitespace included.
export function normalizeViewName(raw: string): string {
  return raw.trim()
}

// Resolve a Save against the current type's views. The name match
// is exact, and only within that type — identity is per user, per
// list type, per name, so "Active" on one list says nothing about
// "Active" on another.
export function planSave(
  views: SavedView[], rawName: string): SavePlan {
  const name = normalizeViewName(rawName)
  if (name === '') return { kind: 'none' }
  const match = views.find(v => v.name === name)
  if (match != null) return { kind: 'update', id: match.id, name }
  return { kind: 'insert', name }
}
