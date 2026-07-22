import { useEffect, useRef, useState } from 'react'
import { apiFetch, setTokens, clearTokens, getAccessToken,
         onAuthFailure } from './api'
import StarRating from './StarRating'

// Apply CSS-relevant settings values to the document.
// Currently only dark-mode (boolean → toggle .dark class on body).
// Future fields just add one line each here.
function applyCssVariables(vars: Record<string, unknown>) {
  const body = document.body
  if (vars['dark-mode']) {
    body.classList.add('dark')
  } else {
    body.classList.remove('dark')
  }
}

// Fetch CSS variables from the backend and apply them.
async function fetchAndApplyCssVariables() {
  try {
    const res = await apiFetch('/api/css-variables')
    if (res.ok) {
      const json = await res.json()
      if (json?.result) {
        applyCssVariables(json.result)
      }
    }
  } catch {
    /* non-fatal — default light theme stays */
  }
}

// Extract the backend's error message from a failed response, falling back
// to a generic message if the body can't be parsed.
async function errorMessage(res: Response, fallback: string): Promise<string> {
  try {
    const body = await res.json()
    if (body?.error) return body.error
  } catch {
    /* keep fallback */
  }
  return fallback
}

interface Field {
  label: string
  'input-type': string
  path?: boolean
  'render-as'?: string
  table?: string
  precision?: number
}

interface ListResponse {
  status: string
  result: {
    'type-key': string
    'list-form': Record<string, Field>
    'add-form': Record<string, Field>
    'update-form': Record<string, Field>
    records: any[]
    'allowed-values'?: Record<string, string[]>
    'type-roles'?: string[]
    create?: boolean
    delete?: boolean
    update?: boolean
  }
}

interface TypeInfo {
  name: string
  category: 'user' | 'system' | 'settings'
}

type ViewMode = 'app' | 'admin' | 'settings'

// Format a number according to the field's :precision UI hint.
// Returns the original value untouched if it's not a number or
// no precision is specified.
function formatNumber(
  val: any, field: Field
): string {
  if (typeof val !== 'number' || field.precision == null)
    return val != null ? String(val) : ''
  return val.toFixed(field.precision)
}

// --- Render-as dispatch ---
//
// Each function handles one rendering context (list cell vs form).
// To add a new render-as value, add a case here. 'text' is the default.

function renderCellValue(
  val: any, field: Field
): React.ReactNode {
  const renderAs = field['render-as'] || 'text'
  const text = Array.isArray(val) ? val.join(', ')
    : formatNumber(val, field)

  switch (renderAs) {
    case 'code':
      return (
        <pre style={{
          margin: 0,
          maxHeight: '4.5em',
          overflow: 'hidden',
          whiteSpace: 'pre-wrap',
          fontSize: '0.85em',
          fontFamily: 'monospace'
        }}>
          {text}
        </pre>
      )
    case 'image-list': {
      const paths: string[] = Array.isArray(val) ? val : []
      if (paths.length === 0) return text || ''
      return (
        <ThumbnailGrid
          type={field.table || ''} paths={paths} size={40}
        />
      )
    }
    case 'image': {
      const path = typeof val === 'string' ? val : ''
      if (!path) return text || ''
      return (
        <ThumbnailGrid
          type={field.table || ''} paths={[path]} size={40}
        />
      )
    }
    case 'stars': {
      const num = typeof val === 'number' ? val
        : val ? Number(val) : null
      if (num == null || isNaN(num)) return ''
      return <StarRating value={num} />
    }
    default:
      return text
  }
}

function ImagePreview({
  type, path
}: {
  type: string
  path: string
}) {
  const [open, setOpen] = useState(false)
  const name = path.split('/').pop() || path
  const src = fileUrl(type, path)

  return (
    <>
      <img
        src={src}
        alt={name}
        onClick={() => setOpen(true)}
        onError={e => {
          (e.target as HTMLImageElement).style
            .display = 'none'
        }}
        style={{
          width: '60px',
          height: '60px',
          objectFit: 'cover',
          flexShrink: 0,
          cursor: 'pointer'
        }}
      />
      {open && (
        <ImageModal
          images={[{ src, filename: name }]}
          index={0}
          onClose={() => setOpen(false)}
          onNavigate={() => {}}
        />
      )}
    </>
  )
}

function renderFormField(
  field: Field, value: any,
  onChange: (v: string) => void
): React.ReactNode {
  const renderAs = field['render-as'] || 'text'

  // For now only 'code' gets special form treatment.
  // Future render-as values (rating, image, etc.) will add cases.
  if (renderAs === 'stars') {
    const num = value ? Number(value) : null
    return (
      <StarRating
        value={num}
        interactive={true}
        onChange={onChange}
      />
    )
  }

  if (renderAs === 'code') {
    return (
      <textarea
        value={value || ''}
        onChange={e => onChange(e.target.value)}
        rows={12}
        style={{
          width: '100%',
          fontFamily: 'monospace',
          fontSize: '0.95em'
        }}
      />
    )
  }

  if (renderAs === 'image' && field.table) {
    return (
      <div style={{ display: 'flex', gap: '0.5rem',
        alignItems: 'flex-start' }}>
        <input
          type="text"
          value={value || ''}
          onChange={e => onChange(e.target.value)}
        />
        {value && (
          <ImagePreview
            type={field.table}
            path={String(value)}
          />
        )}
      </div>
    )
  }

  return (
    <input
      type="text"
      value={value || ''}
      onChange={e => onChange(e.target.value)}
    />
  )
}

// --- Image helpers ---

function fileUrl(type: string, path: string): string {
  const token = getAccessToken()
  const base = `/api/file?type=${encodeURIComponent(type)}`
    + `&path=${encodeURIComponent(path)}`
  return token ? `${base}&token=${encodeURIComponent(token)}` : base
}

function ImageModal({
  images, index, onClose, onNavigate
}: {
  images: { src: string; filename: string }[]
  index: number
  onClose: () => void
  onNavigate: (index: number) => void
}) {
  const current = images[index]
  const hasPrev = images.length > 1
  const hasNext = images.length > 1

  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      if (e.key === 'ArrowLeft')
        onNavigate(
          (index - 1 + images.length) % images.length
        )
      else if (e.key === 'ArrowRight')
        onNavigate((index + 1) % images.length)
      else if (e.key === 'Escape')
        onClose()
    }
    window.addEventListener('keydown', handler)
    return () => window.removeEventListener(
      'keydown', handler
    )
  }, [index, images.length, onNavigate, onClose])

  return (
    <div
      onClick={onClose}
      style={{
        position: 'fixed',
        inset: 0,
        background: 'var(--overlay-bg)',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        justifyContent: 'center',
        zIndex: 1000,
        cursor: 'pointer'
      }}
    >
      <div style={{
        display: 'flex',
        gap: '1rem',
        marginBottom: '1rem',
        alignItems: 'center',
        cursor: 'default'
      }}>
        <button
          type="button"
          onClick={e => {
            e.stopPropagation()
            onNavigate(
              (index - 1 + images.length) % images.length
            )
          }}
          disabled={!hasPrev}
        >
          ‹ Prev
        </button>
        <span style={{ color: 'var(--muted)', fontSize: '0.85rem' }}>
          {index + 1} / {images.length}
        </span>
        <button
          type="button"
          onClick={e => {
            e.stopPropagation()
            onNavigate((index + 1) % images.length)
          }}
          disabled={!hasNext}
        >
          Next ›
        </button>
        <span style={{ width: '1rem' }} />
        <a
          href={current.src}
          download={current.filename}
          onClick={e => e.stopPropagation()}
          style={modalLinkStyle}
        >
          Download
        </a>
        <a
          href={current.src}
          target="_blank"
          rel="noopener noreferrer"
          onClick={e => e.stopPropagation()}
          style={modalLinkStyle}
        >
          Open in new tab
        </a>
        <button type="button" onClick={onClose}>Close</button>
      </div>
      <img
        src={current.src}
        alt={current.filename}
        onClick={e => e.stopPropagation()}
        style={{
          maxWidth: '90vw',
          maxHeight: '80vh',
          objectFit: 'contain',
          cursor: 'default'
        }}
      />
    </div>
  )
}

const modalLinkStyle: React.CSSProperties = {
  color: 'var(--link)',
  textDecoration: 'underline',
  cursor: 'pointer'
}

function ThumbnailGrid({
  type, paths, size
}: {
  type: string
  paths: string[]
  size?: number
}) {
  const [modalIndex, setModalIndex] = useState<number | null>(
    null
  )
  const thumbSize = size || 80

  if (!paths || paths.length === 0)
    return <span style={{ color: 'var(--muted-2)' }}>—</span>

  const modalImages = paths.map(p => ({
    src: fileUrl(type, p),
    filename: p.split('/').pop() || p
  }))

  return (
    <>
      <div style={{
        display: 'flex',
        flexWrap: 'wrap',
        gap: '0.5rem'
      }}>
        {paths.map((p, i) => {
          const name = p.split('/').pop() || p
          return (
            <div
              key={p}
              onClick={() => setModalIndex(i)}
              style={{
                cursor: 'pointer',
                textAlign: 'center',
                width: `${thumbSize}px`
              }}
            >
              <img
                src={fileUrl(type, p)}
                alt={name}
                style={{
                  width: `${thumbSize}px`,
                  height: `${thumbSize}px`,
                  objectFit: 'cover',
                  display: 'block'
                }}
              />
              <div style={{
                fontSize: '0.7rem',
                overflow: 'hidden',
                textOverflow: 'ellipsis',
                whiteSpace: 'nowrap'
              }}>
                {name}
              </div>
            </div>
          )
        })}
      </div>
      {modalIndex !== null && (
        <ImageModal
          images={modalImages}
          index={modalIndex}
          onClose={() => setModalIndex(null)}
          onNavigate={setModalIndex}
        />
      )}
    </>
  )
}

// --- Read-only field rendering ---

function renderReadOnlyField(
  field: Field,
  value: any
): React.ReactNode {
  const renderAs = field['render-as'] || 'text'

  if (renderAs === 'image-list') {
    const paths: string[] = Array.isArray(value) ? value : []
    return (
      <ThumbnailGrid
        type={field.table || ''} paths={paths}
      />
    )
  }

  if (renderAs === 'stars') {
    const num = typeof value === 'number' ? value
      : value ? Number(value) : null
    if (num == null || isNaN(num))
      return <div style={{ color: 'var(--muted-2)' }}>—</div>
    return <StarRating value={num} />
  }

  // Default read-only: plain text display
  const text = Array.isArray(value) ? value.join(', ')
    : formatNumber(value, field)
  return (
    <div style={{
      padding: '0.3rem 0',
      color: 'var(--label)',
      minHeight: '1.2em'
    }}>
      {text || '—'}
    </div>
  )
}

function App() {
  const [data, setData] = useState<ListResponse | null>(null)
  const [types, setTypes] = useState<TypeInfo[]>([])
  const [viewMode, setViewMode] = useState<ViewMode>('app')
  const [type, setType] = useState('__init__')
  const [showAddForm, setShowAddForm] = useState(false)
  const [formValues, setFormValues] = useState<Record<string, any>>({})
  const [selectedIds, setSelectedIds] = useState<string[]>([])
  const [editRecord, setEditRecord] = useState<any>(null)
  const [listError, setListError] = useState<string | null>(null)
  const [title, setTitle] = useState('Data UI')
  const [extraRoles, setExtraRoles] = useState<string[]>([])
  const [userSearch, setUserSearch] = useState('')
  const [userSearchResults, setUserSearchResults] = useState<string[]>([])
  const [userSearchLoading, setUserSearchLoading] = useState(false)
  const [pendingActions, setPendingActions] = useState<Set<string>>(new Set())
  const [elapsed, setElapsed] = useState<number | null>(null)
  const runningStartRef = useRef<number | null>(null)

  // Auth state
  const [username, setUsername] = useState('')
  const [password, setPassword] = useState('')
  const [loginError, setLoginError] = useState('')
  const [loggedIn, setLoggedIn] = useState(false)
  const [loggedInUser, setLoggedInUser] = useState('')

  // When token refresh fails, force return to login screen
  useEffect(() => {
    onAuthFailure(() => {
      setLoggedIn(false)
      setLoggedInUser('')
      setData(null)
    })
  }, [])

  // Fetch the app title on mount (pre-login, no auth required)
  useEffect(() => {
    fetch('/api/public-info')
      .then(r => r.ok ? r.json() : null)
      .then(json => {
        if (json?.result?.['title']) {
          setTitle(String(json.result['title']))
        }
      })
      .catch(() => {})
  }, [])

  const isEditMode = !!editRecord

  const userTypes = types.filter(t => t.category === 'user')
  const systemTypes = types.filter(t => t.category === 'system')
  const settingsTypes = types.filter(t => t.category === 'settings')

  const activeTypes = viewMode === 'admin' ? systemTypes
    : viewMode === 'settings' ? settingsTypes
    : userTypes

  const fetchList = () => {
    setListError(null)
    apiFetch(`/api/list?type=${type}`)
      .then(res => {
        if (!res.ok) {
          throw new Error(`Request failed (${res.status})`)
        }
        return res.json()
      })
      .then(setData)
      .catch(err => {
        setData(null)
        setListError(err.message || 'Failed to load data')
      })
  }

  const changeType = (newType: string) => {
    setType(newType)
    setShowAddForm(false)
    setEditRecord(null)
    setFormValues({})
    setExtraRoles([])
    setUserSearch('')
    setUserSearchResults([])
  }

  const switchViewMode = (mode: ViewMode) => {
    setViewMode(mode)
    setShowAddForm(false)
    setEditRecord(null)
    setFormValues({})
    // Select first type in the new mode
    if (mode === 'app') {
      const first = userTypes[0]
      if (first) setType(first.name)
    } else if (mode === 'admin') {
      const first = systemTypes[0]
      if (first) setType(first.name)
    } else if (mode === 'settings') {
      const first = settingsTypes[0]
      if (first) setType(first.name)
    }
  }

  const openEditForm = async (record: any) => {
    // Fetch the full record from /api/item with update-form so that
    // fields absent from list-form (e.g. deploy-status, my-rating)
    // are populated correctly in the edit form.
    let fullRecord = record
    try {
      const res = await apiFetch(
        `/api/item?type=${type}&id=${record.id}&form=update-form`
      )
      if (res.ok) {
        const json = await res.json()
        fullRecord = json?.result?.record || record
      }
    } catch {
      // Fall back to list record on error
    }
    setEditRecord(fullRecord)
    // Filter out the current user's own exclusive role from the
    // roles — it's injected automatically by the backend and should
    // never appear as a manual checkbox or be sent back.
    const myExclusive = `${loggedInUser}:exclusive`
    const cleanRoles = (fullRecord.roles || []).filter(
      (r: string) => r !== myExclusive
    )
    setFormValues({ ...fullRecord, roles: cleanRoles })
    setShowAddForm(false)
    // Populate extraRoles with any roles on the record that aren't
    // in the backend's allowed-values (e.g. other users' exclusive
    // roles from prior sharing)
    const allowedRoles = data?.result?.['allowed-values']?.['roles'] || []
    const extra = cleanRoles.filter(
      (r: string) => !allowedRoles.includes(r)
    )
    setExtraRoles(extra)
    setUserSearch('')
    setUserSearchResults([])
  }

  const closeForm = () => {
    setEditRecord(null)
    setShowAddForm(false)
    setFormValues({})
    setExtraRoles([])
    setUserSearch('')
    setUserSearchResults([])
  }

  // Return to the landing page (used by settings Submit/Cancel).
  const returnToLanding = () => {
    closeForm()
    fetchAndApplyCssVariables()
    apiFetch('/api/info')
      .then(r => r.json())
      .then(info => {
        const t = info.result?.['title']
        if (t) setTitle(String(t))
        const lp = info.result?.['landing-page']
        if (lp) {
          setViewMode('app')
          setType(String(lp))
        }
      })
      .catch(() => {
        // Fallback: first user type
        setViewMode('app')
        const first = userTypes[0]
        if (first) setType(first.name)
      })
  }

  // Cancel button handler: returns to landing in settings mode,
  // otherwise just closes the form.
  const handleCancel = () => {
    if (viewMode === 'settings') {
      returnToLanding()
    } else {
      closeForm()
    }
  }

  const handleLogin = async () => {
    setLoginError('')
    try {
      const res = await fetch('/api/login', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ username, password })
      })
      const json = await res.json()
      if (json.status === 'success' && json.result) {
        const access = json.result['access-token']
        const refresh = json.result['refresh-token']
        if (access && refresh) {
          setTokens(access, refresh)
          setLoggedIn(true)
          setLoggedInUser(username)
          setUsername('')
          setPassword('')
          fetchAndApplyCssVariables()
        } else {
          setLoginError('Invalid response from server')
        }
      } else {
        setLoginError(json.error || 'Login failed')
      }
    } catch (e) {
      setLoginError('Network error')
    }
  }

  // Debounced user search for role sharing
  const userSearchTimer = useRef<ReturnType<typeof setTimeout> | null>(null)

  const handleUserSearch = (query: string) => {
    setUserSearch(query)
    if (userSearchTimer.current) clearTimeout(userSearchTimer.current)
    if (query.trim().length === 0) {
      setUserSearchResults([])
      return
    }
    setUserSearchLoading(true)
    userSearchTimer.current = setTimeout(() => {
      apiFetch(`/api/users?q=${encodeURIComponent(query.trim())}`)
        .then(res => res.ok ? res.json() : null)
        .then(json => {
          setUserSearchResults(json?.result || [])
        })
        .catch(() => setUserSearchResults([]))
        .finally(() => setUserSearchLoading(false))
    }, 300)
  }

  const addUserRole = (username: string) => {
    const role = `${username}:exclusive`
    // Add to extra roles so it shows as a checkbox
    if (!extraRoles.includes(role)) {
      setExtraRoles([...extraRoles, role])
    }
    // Check it in form values
    const currentRoles = formValues.roles || []
    if (!currentRoles.includes(role)) {
      setFormValues({ ...formValues, roles: [...currentRoles, role] })
    }
    // Clear search
    setUserSearch('')
    setUserSearchResults([])
  }

  const handleAction = async (fieldKey: string) => {
    if (!editRecord) return
    const id = editRecord.id
    setPendingActions(prev => new Set(prev).add(fieldKey))
    try {
      const res = await apiFetch('/api/actions', {
        method: 'POST',
        body: JSON.stringify({
          type,
          id,
          field: fieldKey
        })
      })
      if (!res.ok) {
        alert(await errorMessage(res, 'Action failed'))
      }
      // Reload list, then re-open edit form with updated record
      // so the status field refreshes.
      const listRes = await apiFetch(`/api/list?type=${type}`)
      if (listRes.ok) {
        const json = await listRes.json()
        setData(json)
        const updated = json?.result?.records?.find(
          (r: any) => r.id === id
        )
        if (updated) openEditForm(updated)
      }
    } catch {
      alert('Network error during action')
    } finally {
      setPendingActions(prev => {
        const next = new Set(prev)
        next.delete(fieldKey)
        return next
      })
    }
  }

  const submitForm = async () => {
    // Find file field if present
    const formDef = isEditMode ? data!.result['update-form'] : data!.result['add-form']
    const fileField = Object.keys(formDef).find(f => formDef[f]['input-type'] === 'file')
    const fileValue = fileField ? formValues[fileField] : null

    if (fileField && fileValue instanceof File) {
      // Two-phase upload.
      //
      // POST 1: multipart/form-data to /api/upload with all add-form
      // fields, including the file field. The browser sets the
      // multipart boundary automatically (see apiFetch).
      const formData = new FormData()
      formData.append('type', type)
      for (const f of Object.keys(formDef)) {
        const value = formValues[f]
        if (value === undefined || value === null) continue
        if (f === fileField) {
          formData.append(f, value)
        } else if (Array.isArray(value)) {
          value.forEach(v => formData.append(f, v))
        } else {
          formData.append(f, value)
        }
      }
      if (formValues.roles) {
        // roles may be array or single value; send as-is
        const roles = formValues.roles
        if (Array.isArray(roles)) {
          roles.forEach(r => formData.append('roles', r))
        } else {
          formData.append('roles', roles)
        }
      }

      const uploadRes = await apiFetch('/api/upload', {
        method: 'POST',
        body: formData
      })

      if (!uploadRes.ok) {
        let detail = `${uploadRes.status} ${uploadRes.statusText}`.trim()
        const errText = await uploadRes.text().catch(() => '')
        if (errText) {
          try {
            const errJson = JSON.parse(errText)
            detail =
              errJson?.result?.message ||
              errJson?.message ||
              errJson?.error ||
              detail
          } catch {
            // Body wasn't JSON; use the raw text.
            detail = errText
          }
        }
        alert(`File upload failed: ${detail}`)
        return
      }

      // Extract the file-token returned by the upload.
      const uploadJson = await uploadRes.json()
      const fileToken =
        uploadJson?.['file-token'] ?? uploadJson?.result?.['file-token']

      if (!fileToken) {
        alert('File upload did not return a file-token')
        return
      }

      // POST 2: JSON to /api/insert with the add-form fields plus a
      // top-level file-token (sibling of type). The file field itself
      // is omitted from the metadata payload.
      const { roles, [fileField]: _omit, ...rest } = formValues
      const filteredRest = Object.fromEntries(
        Object.entries(rest).filter(([, v]) => typeof v !== 'string' || v.trim() !== '')
      )
      const payload: any = { type, 'file-token': fileToken, data: filteredRest }
      if (roles) payload.roles = Array.isArray(roles) ? roles : [roles]

      const res = await apiFetch('/api/insert', {
        method: 'POST',
        body: JSON.stringify(payload)
      })

      if (res.ok) {
        closeForm()
        fetchList()
      } else {
        alert(await errorMessage(res, 'Failed to insert'))
      }
      return
    }

    // Normal (no file) path
    const { roles, ...rest } = formValues
    const filteredRest = Object.fromEntries(
      Object.entries(rest).filter(([, v]) => typeof v !== 'string' || v.trim() !== '')
    )
    const payload: any = { type, data: filteredRest }
    if (roles) payload.roles = roles

    let res
    if (isEditMode) {
      payload.filters = editRecord.id
      res = await apiFetch('/api/update', {
        method: 'POST',
        body: JSON.stringify(payload)
      })
    } else {
      res = await apiFetch('/api/insert', {
        method: 'POST',
        body: JSON.stringify(payload)
      })
    }

    if (res.ok) {
      if (viewMode === 'settings') {
        returnToLanding()
      } else {
        closeForm()
        fetchList()
      }
    } else {
      alert(await errorMessage(res, isEditMode ? 'Failed to update' : 'Failed to insert'))
    }
  }

  const toggleSelect = (id: string) => {
    if (selectedIds.includes(id)) {
      setSelectedIds(selectedIds.filter(x => x !== id))
    } else {
      setSelectedIds([...selectedIds, id])
    }
  }

  const deleteSelected = async () => {
    if (selectedIds.length === 0) return

    const toDelete = records.filter((r: any) => selectedIds.includes(r.id))
    const nameField = listFields.includes('name') ? 'name' : listFields[0]
    const names = toDelete.map((r: any) => r[nameField] || r.id).join(', ')

    if (!confirm(`Delete ${names}?`)) return

    for (const id of selectedIds) {
      const payload = { type, filters: id }
      await apiFetch('/api/delete', {
        method: 'POST',
        body: JSON.stringify(payload)
      })
    }
    setSelectedIds([])
    fetchList()
  }

  useEffect(() => {
    if (!loggedIn) return
    // Fetch landing page + types in parallel on login
    Promise.all([
      apiFetch('/api/info').then(r => r.json()),
      apiFetch('/api/types').then(r => r.json())
    ]).then(([info, typesJson]) => {
      const typeInfos: TypeInfo[] =
        (typesJson.result || []).map((t: any) => ({
          name: typeof t === 'string' ? t : t.name,
          category: typeof t === 'string'
            ? 'user' : t.category
        }))
      setTypes(typeInfos)
      const t = info.result?.['title']
      if (t) setTitle(String(t))
      const lp = info.result?.['landing-page']
      if (lp) {
        setType(String(lp))
      } else {
        const firstUser = typeInfos.find(
          t => t.category === 'user'
        )
        if (firstUser) setType(firstUser.name)
        else if (typeInfos.length > 0)
          setType(typeInfos[0].name)
      }
      // else: no types at all; leave __init__ (empty app)
    }).catch(() => {
      setTypes([])
    })
  }, [loggedIn])

  useEffect(() => {
    if (!loggedIn || type === '__init__') return
    fetchList()
  }, [loggedIn, type])

  // Poll for status updates when a button field's status starts
  // with "running". Re-fetches the record via /api/item and
  // updates editRecord + formValues so the status text updates
  // live in the open form. Prefix match on "running" means
  // intermediate statuses like "running: building image" keep
  // polling automatically.
  //
  // Also tracks elapsed time since running started, with a 1s
  // ticker for the display.
  const editRecordId = editRecord?.id
  const hasRunningStatus = Object.keys(editRecord || {}).some(
    k => k.endsWith('-status') &&
         String(editRecord?.[k]).startsWith('running')
  )

  // Manage the running start timestamp
  useEffect(() => {
    if (hasRunningStatus && runningStartRef.current === null) {
      runningStartRef.current = Date.now()
      setElapsed(0)
    } else if (!hasRunningStatus) {
      runningStartRef.current = null
      setElapsed(null)
    }
  }, [hasRunningStatus])

  // 1s ticker for elapsed display
  useEffect(() => {
    if (!hasRunningStatus) return
    const ticker = setInterval(() => {
      if (runningStartRef.current !== null) {
        setElapsed(Math.floor(
          (Date.now() - runningStartRef.current) / 1000
        ))
      }
    }, 1000)
    return () => clearInterval(ticker)
  }, [hasRunningStatus])

  // 3s poll for record updates
  useEffect(() => {
    if (!editRecordId || !type || !hasRunningStatus) return

    const interval = setInterval(async () => {
      try {
        const res = await apiFetch(
          `/api/item?type=${type}&id=${editRecordId}` +
          `&form=update-form`
        )
        if (!res.ok) return
        const json = await res.json()
        const updated = json?.result?.record
        if (!updated) return
        setEditRecord(updated)
        // Preserve any unsaved form edits except status fields,
        // which come from the server.
        setFormValues(prev => {
          const next = { ...prev }
          for (const k of Object.keys(updated)) {
            if (k.endsWith('-status')) {
              next[k] = updated[k]
            }
          }
          return next
        })
        // Also refresh the list so list-view statuses stay fresh
        fetchList()
      } catch {
        // Network errors during polling are non-fatal
      }
    }, 3000)

    return () => clearInterval(interval)
  }, [editRecordId, hasRunningStatus, type])

  // In settings mode, auto-enter edit mode with the single record.
  // Depends on `type` too: when switching to settings, viewMode changes
  // first (firing this effect with stale data), then type changes and
  // fetchList delivers the correct record. Without `type` in the deps,
  // the stale-data call sets isEditMode=true and the guard prevents the
  // correct record from ever loading.
  useEffect(() => {
    if (viewMode !== 'settings') return
    if (!data?.result?.records?.length) return
    if (data.result['type-key'] !== type) return
    if (isEditMode) return
    openEditForm(data.result.records[0])
  }, [data, viewMode, type])

  if (!loggedIn) {
    return (
      <div style={{ maxWidth: 320, margin: '100px auto', padding: 20 }}>
        <h1>{title}</h1>
        <h2>Login</h2>
        <input
          type="text"
          placeholder="Username"
          value={username}
          onChange={e => setUsername(e.target.value)}
          style={{ width: '100%', marginBottom: 8 }}
        />
        <input
          type="password"
          placeholder="Password"
          value={password}
          onChange={e => setPassword(e.target.value)}
          style={{ width: '100%', marginBottom: 8 }}
          onKeyDown={e => e.key === 'Enter' && handleLogin()}
        />
        <button onClick={handleLogin} style={{ width: '100%' }}>Login</button>
        {loginError && <p style={{ color: 'var(--error-bright)' }}>{loginError}</p>}
      </div>
    )
  }

  if (!data || !data.result || Array.isArray(data.result) || !data.result['list-form']) {
    return (
      <div>
        <div style={{ position: 'relative' }}>
          <h1 style={{ margin: 0 }}>
            <a href="#" onClick={(e) => {
              e.preventDefault()
              returnToLanding()
            }} style={{ textDecoration: 'none', color: 'inherit' }}>
              {title}
            </a>
          </h1>
          <div style={{
            position: 'absolute',
            left: '50%',
            top: '50%',
            transform: 'translate(-50%, -50%)',
            fontSize: '1.1rem',
            fontWeight: 'bold'
          }}>
            {type}
          </div>
          <div style={{ position: 'absolute', right: 0, top: '50%', transform: 'translateY(-50%)', display: 'flex', alignItems: 'center', gap: '0.5rem' }}>
            <button
              onClick={() => returnToLanding()}
              style={{
                fontWeight: viewMode === 'app' ? 'bold' : 'normal',
                background: viewMode === 'app' ? 'var(--hover-bg)' : ''
              }}
            >
              🏠 Home
            </button>
            {settingsTypes.length > 0 && (
              <button
                onClick={() => switchViewMode('settings')}
                style={{
                  fontWeight: viewMode === 'settings' ? 'bold' : 'normal',
                  background: viewMode === 'settings' ? 'var(--hover-bg)' : ''
                }}
              >
                ⚙ Settings
              </button>
            )}
            {systemTypes.length > 0 && (
              <button
                onClick={() => switchViewMode('admin')}
                style={{
                  fontWeight: viewMode === 'admin' ? 'bold' : 'normal',
                  background: viewMode === 'admin' ? 'var(--hover-bg)' : ''
                }}
              >
                🔧 Admin
              </button>
            )}
            <span style={{ fontSize: '0.9rem' }}>{loggedInUser}</span>
            <button
              onClick={() => {
                clearTokens()
                setLoggedIn(false)
                setLoggedInUser('')
                setData(null)
                document.body.classList.remove('dark')
              }}
              style={{}}
            >
              Logout
            </button>
          </div>
        </div>
        <div style={{ marginBottom: '1rem', display: 'flex', gap: '0.25rem', borderBottom: '2px solid var(--border)' }}>
          {activeTypes.map(t => (
            <button
              key={t.name}
              onClick={() => changeType(t.name)}
              style={{
                padding: '0.5rem 1rem',
                border: 'none',
                background: t.name === type ? 'var(--tab-active-bg)' : 'var(--tab-inactive-bg)',
                borderBottom: t.name === type ? '2px solid var(--tab-border)' : 'none',
                fontWeight: t.name === type ? 'bold' : 'normal',
                cursor: 'pointer'
              }}
            >
              {t.name}
            </button>
          ))}
        </div>
        <p style={{ color: listError ? 'var(--error)' : undefined }}>
          {listError || 'No records'}
        </p>
      </div>
    )
  }

  const listFields = Object.keys(data.result['list-form'])
  const addFields = Object.keys(data.result['add-form'])
  const records = data.result.records

  return (
    <div>
      <div style={{ position: 'relative' }}>
        <h1 style={{ margin: 0 }}>
          <a href="#" onClick={(e) => {
            e.preventDefault()
            returnToLanding()
          }} style={{ textDecoration: 'none', color: 'inherit' }}>
            {title}
          </a>
        </h1>
        <div style={{
          position: 'absolute',
          left: '50%',
          top: '50%',
          transform: 'translate(-50%, -50%)',
          fontSize: '1.1rem',
          fontWeight: 'bold'
        }}>
          {type}
        </div>
        <div style={{ position: 'absolute', right: 0, top: '50%', transform: 'translateY(-50%)', display: 'flex', alignItems: 'center', gap: '0.5rem' }}>
          <button
            onClick={() => returnToLanding()}
            style={{
              fontWeight: viewMode === 'app' ? 'bold' : 'normal',
              background: viewMode === 'app' ? 'var(--hover-bg)' : ''
            }}
          >
            🏠 Home
          </button>
          {settingsTypes.length > 0 && (
            <button
              onClick={() => switchViewMode('settings')}
              style={{
                fontWeight: viewMode === 'settings' ? 'bold' : 'normal',
                background: viewMode === 'settings' ? 'var(--hover-bg)' : ''
              }}
            >
              ⚙ Settings
            </button>
          )}
          {systemTypes.length > 0 && (
            <button
              onClick={() => switchViewMode('admin')}
              style={{
                fontWeight: viewMode === 'admin' ? 'bold' : 'normal',
                background: viewMode === 'admin' ? 'var(--hover-bg)' : ''
              }}
            >
              🔧 Admin
            </button>
          )}
          <span style={{ fontSize: '0.9rem' }}>{loggedInUser}</span>
          <button
            onClick={() => {
              clearTokens()
              setLoggedIn(false)
              setLoggedInUser('')
              setData(null)
              document.body.classList.remove('dark')
            }}
            style={{}}
          >
            Logout
          </button>
        </div>
      </div>

      <div style={{ marginBottom: '1rem', display: 'flex', gap: '0.25rem', borderBottom: '2px solid var(--border)' }}>
        {activeTypes.map(t => (
          <button
            key={t.name}
            onClick={() => changeType(t.name)}
            style={{
              padding: '0.5rem 1rem',
              border: 'none',
              background: t.name === type ? 'var(--tab-active-bg)' : 'var(--tab-inactive-bg)',
              borderBottom: t.name === type ? '2px solid var(--tab-border)' : 'none',
              fontWeight: t.name === type ? 'bold' : 'normal',
              cursor: 'pointer'
            }}
          >
            {t.name}
          </button>
        ))}
      </div>

      {!(showAddForm || isEditMode) && (
        <div style={{ marginBottom: '0.5rem' }}>
          {data.result.create && (
            <button onClick={() => {
              setShowAddForm(true)
              setEditRecord(null)
              setExtraRoles([])
              setUserSearch('')
              setUserSearchResults([])
              const typeRoles = data.result['type-roles'] || []
              if (typeRoles.length > 0) {
                setFormValues({ roles: [...typeRoles] })
              } else {
                setFormValues({})
              }
            }}>
              Add
            </button>
          )}
          {data.result.delete && (
            <button onClick={deleteSelected} style={{ marginLeft: '0.5rem' }}>
              Delete Selected
            </button>
          )}
        </div>
      )}

      {(showAddForm || isEditMode) && (
        <form style={{ marginTop: '1rem', marginLeft: '1.5rem' }}>
          <h3>{isEditMode ? 'Edit' : 'Add'} {data.result['type-key']}</h3>

          <div style={{ marginBottom: '1rem' }}>
            <button type="button" onClick={submitForm}>
              {isEditMode ? 'Update' : 'Submit'}
            </button>
            <button type="button" onClick={handleCancel} style={{ marginLeft: '0.5rem' }}>
              Cancel
            </button>
          </div>

          {(isEditMode ? Object.keys(data.result['update-form']) : addFields).map(f => {
            const fieldMeta = isEditMode
              ? data.result['update-form'][f]
              : data.result['add-form'][f]
            const allowed = data.result['allowed-values']?.[f] || []
            const isCheckboxList = fieldMeta['input-type'] === 'checkbox-list'
            const isCheckBox = fieldMeta['input-type'] === 'check-box'

            if (isCheckboxList) {
              const selected = formValues[f] || []
              const isRolesField = f === 'roles'
              // Merge backend allowed-values with any extra roles
              // added via user search. The backend's selectable-roles
              // already excludes the current user's exclusive role,
              // so this filter is redundant — kept as a safety net.
              const myExclusive = `${loggedInUser}:exclusive`
              const allOptions = isRolesField
                ? [...new Set([...allowed, ...extraRoles])]
                    .filter(r => r !== myExclusive)
                    .sort((a, b) => a.localeCompare(b))
                : allowed

              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>{fieldMeta.label}</label><br />
                  {allOptions.map((val: string) => (
                    <label key={val} style={{ display: 'block', marginLeft: '1rem' }}>
                      <input
                        type="checkbox"
                        checked={selected.includes(val)}
                        onChange={e => {
                          const next = e.target.checked
                            ? [...selected, val]
                            : selected.filter((v: string) => v !== val)
                          // If unchecking an extra role, remove it
                          // from extraRoles so the checkbox disappears
                          if (!e.target.checked && extraRoles.includes(val)) {
                            setExtraRoles(extraRoles.filter(r => r !== val))
                          }
                          setFormValues({ ...formValues, [f]: next })
                        }}
                      />
                      {val}
                    </label>
                  ))}
                  {isRolesField && (
                    <div style={{ marginTop: '0.5rem', marginLeft: '1rem' }}>
                      <input
                        type="text"
                        value={userSearch}
                        onChange={e => handleUserSearch(e.target.value)}
                        placeholder="Search users to share with..."
                        style={{ width: '200px' }}
                      />
                      {userSearchLoading && (
                        <span style={{ marginLeft: '0.5rem',
                          color: 'var(--muted)', fontSize: '0.85em' }}>
                          searching...
                        </span>
                      )}
                      {userSearchResults.length > 0 && (
                        <div style={{
                          position: 'relative',
                          marginTop: '0.25rem',
                          width: '200px',
                          border: '1px solid var(--border)',
                          borderRadius: '3px',
                          background: 'var(--input-bg)',
                          zIndex: 10
                        }}>
                          {userSearchResults.map(name => (
                            <div
                              key={name}
                              onClick={() => addUserRole(name)}
                              style={{
                                padding: '0.25rem 0.5rem',
                                cursor: 'pointer',
                                borderBottom: '1px solid var(--border-light)'
                              }}
                              onMouseEnter={e =>
                                (e.currentTarget.style.background = 'var(--hover-bg)')
                              }
                              onMouseLeave={e =>
                                (e.currentTarget.style.background = 'var(--input-bg)')
                              }
                            >
                              {name}
                            </div>
                          ))}
                        </div>
                      )}
                    </div>
                  )}
                </div>
              )
            }

            if (isCheckBox) {
              const checked = !!formValues[f]
              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>
                    <input
                      type="checkbox"
                      checked={checked}
                      onChange={e => setFormValues({ ...formValues, [f]: e.target.checked })}
                    />
                    {' '}{fieldMeta.label}
                  </label>
                </div>
              )
            }

            if (fieldMeta['input-type'] === 'select') {
              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>{fieldMeta.label}</label><br />
                  <select
                    value={formValues[f] || ''}
                    onChange={e =>
                      setFormValues({ ...formValues, [f]: e.target.value })
                    }
                  >
                    <option value="" disabled>Select...</option>
                    {allowed.map((val: string) => (
                      <option key={val} value={val}>{val}</option>
                    ))}
                  </select>
                </div>
              )
            }

            if (fieldMeta['input-type'] === 'file') {
              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>{fieldMeta.label}</label><br />
                  <input
                    type="file"
                    onChange={e => {
                      const file = e.target.files?.[0] || null
                      setFormValues({ ...formValues, [f]: file })
                    }}
                  />
                </div>
              )
            }

            if (fieldMeta['input-type'] === 'read-only') {
              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>{fieldMeta.label}</label><br />
                  {renderReadOnlyField(
                    fieldMeta, formValues[f]
                  )}
                </div>
              )
            }

            if (fieldMeta['input-type'] === 'button') {
              const statusKey = `${f}-status`
              const statusVal = editRecord?.[statusKey] || ''
              const isRunning = statusVal.startsWith('running')
              const isPending = pendingActions.has(f)
              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>{fieldMeta.label}</label><br />
                  <button
                    type="button"
                    disabled={isRunning || isPending}
                    onClick={() => handleAction(f)}
                  >
                    {isPending ? 'Working...' : fieldMeta.label}
                  </button>
                  {isRunning && (
                    <span style={{
                      marginLeft: '0.5rem',
                      fontSize: '0.85em',
                      display: 'inline-flex',
                      alignItems: 'center',
                      gap: '0.4rem',
                      color: 'var(--muted)'
                    }}>
                      <span className="du-spinner" />
                      {elapsed !== null && (
                        <span>{elapsed}s</span>
                      )}
                      {statusVal !== 'running' && (
                        <span>({statusVal})</span>
                      )}
                    </span>
                  )}
                  {!isRunning && statusVal && (
                    <span style={{
                      marginLeft: '0.5rem',
                      fontSize: '0.85em',
                      color: statusVal === 'complete'
                        ? 'var(--success, green)'
                        : statusVal.startsWith('failed')
                          ? 'var(--error)'
                          : 'var(--muted)'
                    }}>
                      {statusVal}
                    </span>
                  )}
                </div>
              )
            }

            return (
              <div key={f} style={{ marginBottom: '0.5rem' }}>
                <label>{fieldMeta.label}</label><br />
                {renderFormField(fieldMeta, formValues[f], v =>
                  setFormValues({ ...formValues, [f]: v })
                )}
              </div>
            )
          })}

          <button type="button" onClick={submitForm}>
            {isEditMode ? 'Update' : 'Submit'}
          </button>
          <button type="button" onClick={handleCancel} style={{ marginLeft: '0.5rem' }}>
            Cancel
          </button>
        </form>
      )}

      {viewMode !== 'settings' && (
      <table>
        <thead>
          <tr>
            {data.result.delete && (
              <th style={{ width: '40px', textAlign: 'center', color: 'var(--error-bright)' }}>✕</th>
            )}
            {data.result.update && (
              <th style={{ width: '60px' }}></th>
            )}
            {listFields.map(f => (
              <th key={f}>{data.result['list-form'][f].label}</th>
            ))}
          </tr>
        </thead>
        <tbody>
          {records.map((rec, idx) => (
            <tr key={idx}>
              {data.result.delete && (
                <td style={{ textAlign: 'center' }}>
                  <input
                    type="checkbox"
                    checked={selectedIds.includes(rec.id)}
                    onChange={() => toggleSelect(rec.id)}
                  />
                </td>
              )}
              {data.result.update && (
                <td>
                  <button onClick={() => openEditForm(rec)}>Edit</button>
                </td>
              )}
              {listFields.map(f => {
                const field = data.result['list-form'][f]
                return (
                  <td key={f}>
                    {renderCellValue(rec[f], field)}
                  </td>
                )
              })}
            </tr>
          ))}
        </tbody>
      </table>
      )}
    </div>
  )
}

export default App