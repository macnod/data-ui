import { useEffect, useState } from 'react'
import { apiFetch, setTokens, clearTokens, getAccessToken } from './api'

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
    create?: boolean
    delete?: boolean
    update?: boolean
  }
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
    : (val !== null && val !== undefined ? String(val) : '')

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

  return (
    <div
      onClick={onClose}
      style={{
        position: 'fixed',
        inset: 0,
        background: 'rgba(0,0,0,0.8)',
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
        <span style={{ color: '#ccc', fontSize: '0.85rem' }}>
          {index + 1} / {images.length}
        </span>
        <button
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
        <button onClick={onClose}>Close</button>
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
  color: '#6cf',
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
    return <span style={{ color: '#999' }}>—</span>

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

  // Default read-only: plain text display
  const text = Array.isArray(value) ? value.join(', ')
    : (value !== null && value !== undefined ? String(value) : '')
  return (
    <div style={{
      padding: '0.3rem 0',
      color: '#555',
      minHeight: '1.2em'
    }}>
      {text || '—'}
    </div>
  )
}

function App() {
  const [data, setData] = useState<ListResponse | null>(null)
  const [types, setTypes] = useState<string[]>([])
  const [type, setType] = useState('roles')
  const [showAddForm, setShowAddForm] = useState(false)
  const [formValues, setFormValues] = useState<Record<string, any>>({})
  const [selectedIds, setSelectedIds] = useState<string[]>([])
  const [editRecord, setEditRecord] = useState<any>(null)

  // Auth state
  const [username, setUsername] = useState('')
  const [password, setPassword] = useState('')
  const [loginError, setLoginError] = useState('')
  const [loggedIn, setLoggedIn] = useState(false)
  const [loggedInUser, setLoggedInUser] = useState('')

  const isEditMode = !!editRecord

  const fetchTypes = () => {
    apiFetch('/api/types')
      .then(res => res.json())
      .then(json => setTypes(json.result || []))
      .catch(() => setTypes([]))
  }

  const fetchList = () => {
    apiFetch(`/api/list?type=${type}`)
      .then(res => res.json())
      .then(setData)
      .catch(() => setData(null))
  }

  const changeType = (newType: string) => {
    setType(newType)
    setShowAddForm(false)
    setFormValues({})
  }

  const openEditForm = (record: any) => {
    setEditRecord(record)
    setFormValues({ ...record })
    setShowAddForm(false)
  }

  const closeForm = () => {
    setEditRecord(null)
    setShowAddForm(false)
    setFormValues({})
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
      closeForm()
      fetchList()
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
    fetchTypes()
    fetchList()
  }, [loggedIn, type])

  if (!loggedIn) {
    return (
      <div style={{ maxWidth: 320, margin: '100px auto', padding: 20 }}>
        <h1>Data UI</h1>
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
        {loginError && <p style={{ color: 'red' }}>{loginError}</p>}
      </div>
    )
  }

  if (!data || !data.result || Array.isArray(data.result) || !data.result['list-form']) {
    return (
      <div>
        <div style={{ position: 'relative' }}>
          <h1 style={{ margin: 0 }}>Data UI</h1>
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
            <span style={{ fontSize: '0.9rem' }}>{loggedInUser}</span>
            <button
              onClick={() => { clearTokens(); setLoggedIn(false); setLoggedInUser(''); setData(null) }}
              style={{}}
            >
              Logout
            </button>
          </div>
        </div>
        <div style={{ marginBottom: '1rem', display: 'flex', gap: '0.25rem', borderBottom: '2px solid #ccc' }}>
          {types.map(t => (
            <button
              key={t}
              onClick={() => changeType(t)}
              style={{
                padding: '0.5rem 1rem',
                border: 'none',
                background: t === type ? '#fff' : '#f0f0f0',
                borderBottom: t === type ? '2px solid #000' : 'none',
                fontWeight: t === type ? 'bold' : 'normal',
                cursor: 'pointer'
              }}
            >
              {t}
            </button>
          ))}
        </div>
        <p>No records</p>
      </div>
    )
  }

  const listFields = Object.keys(data.result['list-form'])
  const addFields = Object.keys(data.result['add-form'])
  const records = data.result.records

  return (
    <div>
      <div style={{ position: 'relative' }}>
        <h1 style={{ margin: 0 }}>Data UI</h1>
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
          <span style={{ fontSize: '0.9rem' }}>{loggedInUser}</span>
          <button
            onClick={() => { clearTokens(); setLoggedIn(false); setLoggedInUser(''); setData(null) }}
            style={{}}
          >
            Logout
          </button>
        </div>
      </div>

      <div style={{ marginBottom: '1rem', display: 'flex', gap: '0.25rem', borderBottom: '2px solid #ccc' }}>
        {types.map(t => (
          <button
            key={t}
            onClick={() => changeType(t)}
            style={{
              padding: '0.5rem 1rem',
              border: 'none',
              background: t === type ? '#fff' : '#f0f0f0',
              borderBottom: t === type ? '2px solid #000' : 'none',
              fontWeight: t === type ? 'bold' : 'normal',
              cursor: 'pointer'
            }}
          >
            {t}
          </button>
        ))}
      </div>

      <div style={{ marginBottom: '0.5rem' }}>
        {data.result.create && (
          <button onClick={() => { setShowAddForm(!showAddForm); setEditRecord(null) }}>
            {showAddForm ? 'Cancel' : 'Add'}
          </button>
        )}
        {data.result.delete && (
          <button onClick={deleteSelected} style={{ marginLeft: '0.5rem' }}>
            Delete Selected
          </button>
        )}
      </div>

      {(showAddForm || isEditMode) && (
        <form style={{ marginTop: '1rem', marginLeft: '1.5rem' }}>
          <h3>{isEditMode ? 'Edit' : 'Add'} {data.result['type-key']}</h3>

          {(isEditMode ? Object.keys(data.result['update-form']) : addFields).map(f => {
            const fieldMeta = isEditMode
              ? data.result['update-form'][f]
              : data.result['add-form'][f]
            const allowed = data.result['allowed-values']?.[f] || []
            const isCheckboxList = fieldMeta['input-type'] === 'checkbox-list'
            const isCheckBox = fieldMeta['input-type'] === 'check-box'

            if (isCheckboxList) {
              const selected = formValues[f] || []
              return (
                <div key={f} style={{ marginBottom: '0.5rem' }}>
                  <label>{fieldMeta.label}</label><br />
                  {allowed.map((val: string) => (
                    <label key={val} style={{ display: 'block', marginLeft: '1rem' }}>
                      <input
                        type="checkbox"
                        checked={selected.includes(val)}
                        onChange={e => {
                          const next = e.target.checked
                            ? [...selected, val]
                            : selected.filter((v: string) => v !== val)
                          setFormValues({ ...formValues, [f]: next })
                        }}
                      />
                      {val}
                    </label>
                  ))}
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
          <button type="button" onClick={closeForm} style={{ marginLeft: '0.5rem' }}>
            Cancel
          </button>
        </form>
      )}

      <table>
        <thead>
          <tr>
            {data.result.delete && (
              <th style={{ width: '40px', textAlign: 'center', color: 'red' }}>✕</th>
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
    </div>
  )
}

export default App