// Best Link helpers (todo "Bot-crawlable public pages", child 3).
// The public page URL comes from the backend wire record's
// `public-url` key (omitted, never null, when the record has no
// public page — see docs/rest.md → Public pages).

// Today's Best Link hash: origin, pathname, #/app?type=<type>&id=<id>,
// no mode, no list params. Reproduces the exact string the button
// copied before public pages existed (encodeURIComponent on type and
// id included), so a hash copy is byte-identical to the old behavior.
export function hashPermalink(
  origin: string,
  pathname: string,
  type: string,
  id: string
): string {
  return `${origin}${pathname}#/app?type=${encodeURIComponent(type)}` +
    `&id=${encodeURIComponent(id)}`
}

// The public page URL when it is a nonempty string, else the hash.
// A missing, empty, or non-string publicUrl falls back (the backend
// never emits a present nil — plist-to-json would render it as [],
// which is truthy in JS — but the guard is cheap and defensive).
export function bestLinkUrl(
  publicUrl: unknown,
  hashUrl: string
): string {
  return typeof publicUrl === 'string' && publicUrl !== ''
    ? publicUrl
    : hashUrl
}
