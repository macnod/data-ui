import { useEffect, useState } from 'react'

interface Field {
  label: string
  'input-type': string
}

interface ListResponse {
  status: string
  result: {
    type: string
    fields: Record<string, Field>
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

  const changeType = (newType: string) => {
    setType(newType)
    setShowAddForm(false)
    setFormValues({})
  }

  const submitAddForm = async () => {
    const payload = {
      type,
      data: formValues
    }

    const res = await fetch('/api/insert', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(payload)
    })

    if (res.ok) {
      setShowAddForm(false)
      setFormValues({})
      // Refresh the list
      fetch(`/api/list?type=${type}`)
        .then(r => r.json())
        .then(setData)
    } else {
      alert('Failed to insert')
    }
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
            <button key={t} onClick={() => setType(t)} style={{ marginRight: 8 }}>
              {t}
            </button>
          ))}
        </div>
        <p>No records</p>
      </div>
    )
  }

  const fields = Object.keys(data.result.fields)
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

      <button onClick={() => setShowAddForm(!showAddForm)}>
        {showAddForm ? 'Cancel' : 'Add'}
      </button>

      {showAddForm && (
        <form style={{ marginTop: '1rem' }}>
          {fields.map(f => {
            const fieldMeta = data.result.fields[f]
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

            // Default: text input
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
          <button type="button" onClick={submitAddForm}>
            Submit
          </button>
        </form>
      )}

      <table>
        <thead>
          <tr>
            {fields.map(f => (
              <th key={f}>{data.result.fields[f].label}</th>
            ))}
          </tr>
        </thead>
        <tbody>
          {records.map((rec, idx) => (
            <tr key={idx}>
              {fields.map(f => {
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