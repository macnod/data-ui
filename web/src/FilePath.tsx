// FilePath — folder select + editable filename for :path fields
// (:widget :file-path).
//
// Splits one path string ("/docs/a.txt") into folder ("/docs/",
// a directory path ending in "/") and filename ("a.txt"), joins
// them back on every change, and calls onChange with the joined
// string — the exact value a textbox submits today. The API is
// unchanged.
//
// One fetch: GET /api/list on the directory type (the
// :directory-type :ui key). The field key flagged path in that
// type's list form is the folder column (schema-derived — nothing
// requires it to be called "name"); "name" is the fallback.
// Failure is non-fatal: "/" plus the stored folder stay offered,
// and a bad parent surfaces at insert like any textbox typo.

import { useEffect, useState } from 'react'
import { apiFetch } from './api'

interface FilePathProps {
  directoryType: string
  value: string
  onChange: (path: string) => void
  suggestedName: string
}

// Server-side list clamp (+max-limit+ in backend.lisp). A tree
// with more readable directories misses some — accepted for MVP.
const LIST_LIMIT = 200

function splitPath(path: string): { folder: string, name: string } {
  if (!path) return { folder: '/', name: '' }
  const i = path.lastIndexOf('/')
  if (i <= 0) return { folder: '/', name: path }
  return {
    folder: path.slice(0, i + 1),
    name: path.slice(i + 1)
  }
}

function joinPath(folder: string, name: string): string {
  // folder always ends in "/"; strip a leading slash the user
  // pasted into the filename so the join can never double it.
  // A slash in the middle stays — the backend rejects the path
  // (no invented client-side path grammar).
  const f = folder.endsWith('/') ? folder : folder + '/'
  const n = name.startsWith('/') ? name.slice(1) : name
  return f + n
}

async function fetchFolders(
  directoryType: string
): Promise<string[] | null> {
  try {
    const res = await apiFetch(
      `/api/list?type=${encodeURIComponent(directoryType)}`
      + `&limit=${LIST_LIMIT}`)
    if (!res.ok) return null
    const json = await res.json()
    const form = json?.result?.['list-form'] || {}
    const pathKey = Object.keys(form)
      .find(k => form[k]?.path === true) || 'name'
    const records: any[] = json?.result?.records || []
    const folders = records
      .map(r => String(r[pathKey] ?? ''))
      .filter(p => p.endsWith('/'))
      .sort((a, b) => a.localeCompare(b))
    // dedupe
    return [...new Set(folders)]
  } catch {
    return null
  }
}

function FilePath({ directoryType, value, onChange,
                    suggestedName }: FilePathProps) {
  const [fetched, setFetched] = useState<string[] | null>(null)
  const [userEditedName, setUserEditedName] = useState(false)

  useEffect(() => {
    let cancelled = false
    setFetched(null)
    fetchFolders(directoryType).then(folders => {
      if (!cancelled) setFetched(folders)
    })
    return () => { cancelled = true }
  }, [directoryType])

  const { folder, name } = splitPath(value || '')

  // Pre-fill from the picked file: only while the box is empty
  // AND the user has not edited it (clearing a suggestion counts
  // as editing). Edit forms never suggest unless a new file is
  // picked (the file-update gap is post-MVP).
  useEffect(() => {
    if (suggestedName && !userEditedName && !name) {
      onChange(joinPath(folder, suggestedName))
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [suggestedName])

  // Option list: always "/", every fetched folder, plus the
  // stored folder when it is not already present (stale parent —
  // deleted or unreadable — stays selected; the submit sends the
  // stored path unchanged until the user picks another folder).
  const options = new Set<string>(['/'])
  for (const f of fetched || []) options.add(f)
  if (folder && folder !== '/') options.add(folder)
  const sorted = [...options].sort((a, b) => a.localeCompare(b))

  return (
    <div style={{ display: 'inline-flex', gap: '0.5rem',
                  alignItems: 'center', flexWrap: 'wrap' }}>
      <select
        aria-label="Folder"
        value={folder}
        onChange={e =>
          onChange(joinPath(e.target.value, name))
        }
      >
        {sorted.map(f => (
          <option key={f} value={f}>{f}</option>
        ))}
      </select>
      <input
        type="text"
        aria-label="Filename"
        placeholder="filename"
        value={name}
        onChange={e => {
          setUserEditedName(true)
          onChange(joinPath(folder, e.target.value))
        }}
        style={{ width: '32ch' }}
      />
    </div>
  )
}

export default FilePath
