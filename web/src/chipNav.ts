// Chip-to-record navigation: pure decisions (todo
// "Chip-to-record navigation"). Tested by chipNav.test.mjs.
export type ChipClick = 'filter' | 'unfilter' | 'navigate'

// Unhighlighted filters; highlighted navigates only with a
// target in hand; otherwise today's unfilter.
export function chipClickAction(
  active: boolean,
  nav: { type: string, id: string } | null
): ChipClick {
  if (!active) return 'filter'
  return nav ? 'navigate' : 'unfilter'
}

// Reference family: the row's "<field>-id" sibling plus
// the wire target, gated on visibility (the view-form
// FK link rule, carried to the cell). Row readability is
// deliberately NOT checked (Donnie, 2026-11-12): an
// unreadable target row still shows the arrow, and the
// open lands on the record-not-found error — exactly the
// view form's target-link behavior.
export function refNavTarget(
  target: unknown, sibling: unknown, value: unknown,
  visibleTypes: string[]
): { type: string, id: string } | null {
  if (typeof target !== 'string' || target === '')
    return null
  if (!visibleTypes.includes(target.toLowerCase()))
    return null
  if (typeof sibling !== 'string'
      || sibling.trim() === '') return null
  if (typeof value !== 'string'
      || value.trim() === '') return null
  return { type: target, id: sibling }
}

// View-form chip navigation (chip-to-record's third surface):
// an M2M field's read-only chips can open their records when
// the field's lookup facts and an open handler are in hand
// and the target type is visible (lowercase names —
// viewTypeList). Bare :list fields (no meta entry, or M2M
// with no lookup facts on the wire) and surfaces without an
// open handler stay inert spans. Row readability is NOT
// pre-checked — the lazy /api/value-id resolves what it can
// and a miss no-ops (the M2M id must resolve before there is
// anything to open, unlike the reference family's sibling
// id, which navigates and lets the target surface the
// error).
export function m2mViewNav(
  meta: unknown, open: unknown, visibleTypes: string[]
): { type: string, field: string } | null {
  const m = meta as { m2m?: boolean,
    lookup?: { type: string, field: string } } | undefined
  if (!m || m.m2m !== true || !m.lookup) return null
  if (typeof open !== 'function') return null
  if (!visibleTypes.includes(m.lookup.type.toLowerCase()))
    return null
  return m.lookup
}

export function lookupKey(epoch: number, type: string,
  field: string, value: string): string {
  return JSON.stringify([epoch, type, field, value])
}

// /api/value-id body -> id | null. A miss is
// {"result":[]} (nil -> []): never truthy-test it.
export function parseValueIdResult(
  json: unknown
): string | null {
  const r = (json as any)?.result
  return typeof r === 'string' && r.trim() !== ''
    ? r : null
}

// A multi-click on a cell chip must act as one click
// (Donnie, 2026-11-12): the 2nd+ click of a multi-click
// (event.detail > 1) is ignored, and — because the
// filter's re-fetch can remount the button, resetting the
// browser's own click count — so is a navigate or
// unfilter click within CHIP_DBLCLICK_MS of that chip's
// filter click. Keyboard Enter (detail 0) is never a
// multi-click: Enter filters, a second Enter after the
// window navigates.
export const CHIP_DBLCLICK_MS = 500

export function suppressChipClick(
  detail: number, msSinceFilter: number | null
): boolean {
  if (detail > 1) return true
  return msSinceFilter !== null
    && msSinceFilter < CHIP_DBLCLICK_MS
}
