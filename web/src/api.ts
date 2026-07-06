let accessToken: string | null = null
let refreshToken: string | null = null

export function setTokens(access: string, refresh: string) {
  accessToken = access
  refreshToken = refresh
}

export function clearTokens() {
  accessToken = null
  refreshToken = null
}

export function getAccessToken(): string | null {
  return accessToken
}

export function isAuthenticated(): boolean {
  return !!accessToken
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
    if (json.status === 'success' && json.result?.['access-token']) {
      accessToken = json.result['access-token']
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
  }

  return res
}
