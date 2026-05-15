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
          {fields.map(f => (
            <div key={f} style={{ marginBottom: '0.5rem' }}>
              <label>{data.result.fields[f].label}</label><br />
              <input
                type="text"
                value={formValues[f] || ''}
                onChange={e => setFormValues({ ...formValues, [f]: e.target.value })}
              />
            </div>
          ))}
          <button type="button" onClick={() => alert('Submit not wired yet')}>
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