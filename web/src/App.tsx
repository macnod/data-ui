import { useEffect, useState } from 'react'

interface Field {
  label: string
  'input-type': string
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

  const isEditMode = !!editRecord

  const changeType = (newType: string) => {
    setType(newType)
    setShowAddForm(false)
    setFormValues({})
  }

  const submitAddForm = async () => {
    const { roles, ...rest } = formValues
    const payload: any = {
      type,
      data: rest
    }
    if (roles) payload.roles = roles

    const res = await fetch('/api/insert', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(payload)
    })

    if (res.ok) {
      setShowAddForm(false)
      setFormValues({})
      fetch(`/api/list?type=${type}`)
        .then(r => r.json())
        .then(setData)
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

  const submitForm = async () => {
    const { roles, ...rest } = formValues
    const payload: any = { type, data: rest }
    if (roles) payload.roles = roles

    let res
    if (isEditMode) {
      payload.filters = editRecord.id
      res = await fetch('/api/update', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      })
    } else {
      res = await fetch('/api/insert', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      })
    }

    if (res.ok) {
      closeForm()
      fetch(`/api/list?type=${type}`)
        .then(r => r.json())
        .then(setData)
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
      await fetch('/api/delete', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      })
    }
    setSelectedIds([])
    fetch(`/api/list?type=${type}`)
      .then(r => r.json())
      .then(setData)
  }

  useEffect(() => {
    fetch('/api/types')
      .then(res => res.json())
      .then(json => setTypes(json.result))
  }, [])

  useEffect(() => {
    fetch(`/api/list?type=${type}`)
      .then(res => res.json())
      .then(setData)
  }, [type])

  if (!data || !data.result || Array.isArray(data.result)) {
    return (
      <div>
        <h1>Data UI</h1>
        <div>
          {types.map(t => (
            <button key={t} onClick={() => changeType(t)} style={{ marginRight: 8 }}>
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
      <h1>Data UI</h1>

      <div>
        {types.map(t => (
          <button key={t} onClick={() => changeType(t)} style={{ marginRight: 8 }}>
            {t}
          </button>
        ))}
      </div>

      <h2>{data.result.type}</h2>

      <div style={{ marginBottom: '0.5rem' }}>
        <button onClick={() => { setShowAddForm(!showAddForm); setEditRecord(null) }}>
          {showAddForm ? 'Cancel' : 'Add'}
        </button>
        <button onClick={deleteSelected} style={{ marginLeft: '0.5rem' }}>
          Delete Selected
        </button>
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
            <th style={{ width: '40px', textAlign: 'center', color: 'red' }}>✕</th>
            <th style={{ width: '60px' }}></th>
            {listFields.map(f => (
              <th key={f}>{data.result['list-form'][f].label}</th>
            ))}
          </tr>
        </thead>
        <tbody>
          {records.map((rec, idx) => (
            <tr key={idx}>
              <td style={{ textAlign: 'center' }}>
                <input
                  type="checkbox"
                  checked={selectedIds.includes(rec.id)}
                  onChange={() => toggleSelect(rec.id)}
                />
              </td>
              <td>
                <button onClick={() => openEditForm(rec)}>Edit</button>
              </td>
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