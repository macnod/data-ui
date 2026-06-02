import { useEffect, useState } from 'react'
import { apiFetch, setTokens, clearTokens, isAuthenticated } from './api'

interface Field {
  label: string
  'input-type': string
  path?: boolean
}

interface ListResponse {
  status: string
  result: {
    type: string
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

  const submitAddForm = async () => {
    const { roles, ...rest } = formValues
    // Omit empty or whitespace-only string values from POST
    const filteredRest = Object.fromEntries(
      Object.entries(rest).filter(([, v]) => typeof v !== 'string' || v.trim() !== '')
    )
    const payload: any = {
      type,
      data: filteredRest
    }
    if (roles) payload.roles = roles

    const res = await apiFetch('/api/insert', {
      method: 'POST',
      body: JSON.stringify(payload)
    })

    if (res.ok) {
      setShowAddForm(false)
      setFormValues({})
      fetchList()
    } else {
      alert('Failed to insert')
    }
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
        setLoginError(json.result?.message || 'Login failed')
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
      if (roles) payload.roles = roles

      const res = await apiFetch('/api/insert', {
        method: 'POST',
        body: JSON.stringify(payload)
      })

      if (res.ok) {
        closeForm()
        fetchList()
      } else {
        alert('Failed to insert')
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
      alert(isEditMode ? 'Failed to update' : 'Failed to insert')
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
          <h3>{isEditMode ? 'Edit' : 'Add'} {data.result.type}</h3>

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

            return (
              <div key={f} style={{ marginBottom: '0.5rem' }}>
                <label>{fieldMeta.label}</label><br />
                <input
                  type="text"
                  value={formValues[f] || ''}
                  onChange={e => setFormValues({ ...formValues, [f]: e.target.value })}
                />
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
                const val = rec[f]
                let display = ''
                if (Array.isArray(val)) {
                  display = val.join(', ')
                } else if (val !== null && val !== undefined) {
                  display = String(val)
                }
                return <td key={f}>{display}</td>
              })}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

export default App