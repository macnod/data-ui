// A chrome slot (banner / footer) from an /api/info result, or
// null. A non-string or blank value is null so a removed slot
// clears on the next fetch.
export function slotFromInfo(result: unknown, key: string): string | null {
  if (result == null || typeof result !== 'object') return null
  const v = (result as Record<string, unknown>)[key]
  return typeof v === 'string' && v.trim() !== '' ? v : null
}
