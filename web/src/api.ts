// Session token management with localStorage persistence
// ("stay logged in across page refresh"). The stored blob is
// the single source for restore; module vars are the live copy
// every request reads. All persistence is best-effort: a
// storage failure (private mode, quota) logs and degrades to
// the old behavior — logged out on reload — never a failed
// login or a failed rotation.
let accessToken: string | null = null
let refreshToken: string | null = null
// The username the tokens belong to. The API never returns it
// (/api/login takes it as input; /api/refresh returns an access
// token only), so persisting it is the only way to restore the
// logged-in header / exclusive-role filter. Seeded by
// setTokens / restoreSession; nulled by clearTokens;
// read by refreshAccessToken's blob rewrite. Invariant: only a
// non-empty string or null.
let username: string | null = null
let authFailureCallback: (() => void) | null = null
let tokenRefreshedCallback: (() => void) | null = null

const TOKEN_STORAGE_KEY = 'data-ui-session'

export function onAuthFailure(cb: () => void) {
  authFailureCallback = cb
}

// Single-slot registration, same shape as onAuthFailure: the
// setter replaces (never pushes) — a second registration
// silently drops the first. Accepts null and returns the
// callback it displaced so a caller can restore the slot on
// unmount (save / set / restore).
export function onTokenRefreshed(
  cb: (() => void) | null
): (() => void) | null {
  const displaced = tokenRefreshedCallback
  tokenRefreshedCallback = cb
  return displaced
}

// The blob's validity contract: access, refresh, and username
// all non-empty strings. Non-empty is load-bearing on the
// username: "" would restore a blank header and a broken
// `${user}:exclusive` list filter. The rewrite guard in
// refreshAccessToken enforces the identical contract.
function readStoredSession(): {
  access: string; refresh: string; username: string
} | null {
  try {
    const raw = localStorage.getItem(TOKEN_STORAGE_KEY)
    if (!raw) return null
    const blob = JSON.parse(raw)
    if (typeof blob !== 'object' || blob === null) return null
    const { access, refresh, username } = blob
    if (typeof access !== 'string' || access === '') return null
    if (typeof refresh !== 'string' || refresh === '') return null
    if (typeof username !== 'string' || username === '') {
      return null
    }
    return { access, refresh, username }
  } catch {
    // Malformed JSON or a storage failure: treat as absent.
    console.warn('data-ui: ignoring unreadable stored session')
    return null
  }
}

function writeStoredSession(
  access: string, refresh: string, user: string
): void {
  try {
    localStorage.setItem(TOKEN_STORAGE_KEY, JSON.stringify({
      access, refresh, username: user
    }))
  } catch {
    // Private mode / full quota throws on setItem; persistence
    // is best-effort.
    console.warn('data-ui: could not persist session')
  }
}

function removeStoredSession(): void {
  try {
    localStorage.removeItem(TOKEN_STORAGE_KEY)
  } catch { /* best-effort */ }
}

export function setTokens(
  access: string, refresh: string, user: string
) {
  accessToken = access
  refreshToken = refresh
  username = user
  writeStoredSession(access, refresh, user)
}

export function clearTokens() {
  accessToken = null
  refreshToken = null
  // Without nulling the module username, a later
  // refreshAccessToken rewrite could resurrect the stored
  // session this call just removed (stale-username rebuild).
  username = null
  removeStoredSession()
}

export function getAccessToken(): string | null {
  return accessToken
}

export function isAuthenticated(): boolean {
  return !!accessToken
}

// Restore the module vars from localStorage on app mount.
// Idempotent (same blob, same seeds). Returns the session for
// the caller (App sets its loggedInUser state) or null when no
// valid stored session exists.
export function restoreSession(): {
  access: string; refresh: string; username: string
} | null {
  const session = readStoredSession()
  if (!session) return null
  accessToken = session.access
  refreshToken = session.refresh
  username = session.username
  return session
}

async function refreshAccessToken(): Promise<boolean> {
  if (!refreshToken) return false

  try {
    const res = await fetch('/api/refresh', {
      method: 'POST',
      headers: {
        'Authorization': `Bearer ${refreshToken}`
      }
    })

    if (!res.ok) return false

    const json = await res.json()
    const access = json.result?.['access-token']
    if (json.status === 'success' && access) {
      accessToken = access
      // Persist the rotated access token (same refresh token —
      // /api/refresh returns an access token only, never
      // rotates the refresh token). Build from module state,
      // not read-modify-write of the blob: a malformed stored
      // value would silently skip the rewrite and burn a
      // refresh on every reload.
      if (typeof username === 'string' && username !== ''
          && refreshToken) {
        writeStoredSession(access, refreshToken, username)
      } else {
        // Unreachable under the module contract (username is a
        // non-empty string whenever a refresh can run). Say so,
        // but do not fail the rotation over persistence.
        console.error(
          'data-ui: token rotated without a valid session username;'
          + ' stored session not rewritten')
      }
      // Synchronous, after module var + stored blob are both
      // updated, before returning: consumers (image URLs built
      // from getAccessToken()) re-render off this bump while
      // the retry request is still in flight.
      if (tokenRefreshedCallback) tokenRefreshedCallback()
      return true
    }
    return false
  } catch {
    return false
  }
}

export async function apiFetch(url: string, options: RequestInit = {}): Promise<Response> {
  if (!accessToken) {
    throw new Error('Not authenticated')
  }

  const headers = new Headers(options.headers)
  headers.set('Authorization', `Bearer ${accessToken}`)

  // Do not set Content-Type for FormData: the browser must set the
  // multipart/form-data boundary automatically.
  if (
    !headers.has('Content-Type') &&
    options.body &&
    !(options.body instanceof FormData)
  ) {
    headers.set('Content-Type', 'application/json')
  }

  const res = await fetch(url, { ...options, headers })

  if (res.status === 401 && refreshToken) {
    const refreshed = await refreshAccessToken()
    if (refreshed) {
      // Retry with new access token
      const retryHeaders = new Headers(options.headers)
      retryHeaders.set('Authorization', `Bearer ${accessToken}`)
      if (
        !retryHeaders.has('Content-Type') &&
        options.body &&
        !(options.body instanceof FormData)
      ) {
        retryHeaders.set('Content-Type', 'application/json')
      }
      return fetch(url, { ...options, headers: retryHeaders })
    }
    // Refresh failed — session is dead
    clearTokens()
    if (authFailureCallback) authFailureCallback()
  }

  return res
}
