// StarRating — shared star rendering for display and input.
//
// Display mode: shows filled/hollow stars with optional fractional
// fill for averages. No interaction.
//
// Interactive mode: hover preview + click to select. Calls onChange
// with the selected value as a string (1–5), or '' to clear.

import { useState } from 'react'

interface StarRatingProps {
  value: number | null
  max?: number
  interactive?: boolean
  onChange?: (v: string) => void
}

const STAR_SIZE = 20
const STAR_COLOR = '#f5a623'
const EMPTY_COLOR = '#ddd'

// Unique ID counter for clip paths (avoids SVG ID collisions
// when multiple stars share the same fill ratio on the page).
let clipCounter = 0

function Star({
  fill, interactive, onClick, onMouseEnter
}: {
  fill: number  // 0 to 1
  interactive: boolean
  onClick?: () => void
  onMouseEnter?: () => void
}) {
  // Use a unique clip ID per render to avoid collisions.
  const clipId = `star-clip-${++clipCounter}`

  return (
    <svg
      width={STAR_SIZE}
      height={STAR_SIZE}
      viewBox="0 0 24 24"
      onClick={interactive ? onClick : undefined}
      onMouseEnter={interactive ? onMouseEnter : undefined}
      style={{
        cursor: interactive ? 'pointer' : 'default',
        flexShrink: 0
      }}
    >
      <defs>
        {fill > 0 && fill < 1 && (
          <clipPath id={clipId}>
            <rect x="0" y="0"
              width={`${fill * 24}`} height="24" />
          </clipPath>
        )}
      </defs>
      {/* Empty star (background) */}
      <path
        d="M12 2l3.09 6.26L22 9.27l-5 4.87 1.18 6.88L12 17.77l-6.18 3.25L7 14.14 2 9.27l6.91-1.01L12 2z"
        fill={EMPTY_COLOR}
        stroke={EMPTY_COLOR}
        strokeWidth="1"
      />
      {/* Filled portion */}
      {fill >= 1 && (
        <path
          d="M12 2l3.09 6.26L22 9.27l-5 4.87 1.18 6.88L12 17.77l-6.18 3.25L7 14.14 2 9.27l6.91-1.01L12 2z"
          fill={STAR_COLOR}
          stroke={STAR_COLOR}
          strokeWidth="1"
        />
      )}
      {fill > 0 && fill < 1 && (
        <path
          d="M12 2l3.09 6.26L22 9.27l-5 4.87 1.18 6.88L12 17.77l-6.18 3.25L7 14.14 2 9.27l6.91-1.01L12 2z"
          fill={STAR_COLOR}
          stroke={STAR_COLOR}
          strokeWidth="1"
          clipPath={`url(#${clipId})`}
        />
      )}
    </svg>
  )
}

export default function StarRating({
  value, max = 5, interactive = false, onChange
}: StarRatingProps) {
  const displayValue = value ?? 0

  if (!interactive) {
    // Display mode: fractional fill for averages.
    return (
      <div style={{
        display: 'flex',
        gap: '1px',
        alignItems: 'center'
      }}>
        {Array.from({ length: max }, (_, i) => {
          const fill = Math.max(0, Math.min(1,
            displayValue - i))
          return (
            <Star
              key={i}
              fill={fill}
              interactive={false}
            />
          )
        })}
        {value != null && (
          <span style={{
            marginLeft: '0.4rem',
            fontSize: '0.85em',
            color: '#666'
          }}>
            {Number.isInteger(value)
              ? value
              : value.toFixed(1)}
          </span>
        )}
      </div>
    )
  }

  // Interactive mode: hover preview + click.
  return (
    <InteractiveStars
      value={displayValue}
      max={max}
      onChange={onChange!}
    />
  )
}

function InteractiveStars({
  value, max, onChange
}: {
  value: number
  max: number
  onChange: (v: string) => void
}) {
  const [hover, setHover] = useState(0)
  const shown = hover || value

  return (
    <div
      onMouseLeave={() => setHover(0)}
      style={{
        display: 'flex',
        gap: '1px',
        alignItems: 'center'
      }}
    >
      {Array.from({ length: max }, (_, i) => {
        const starVal = i + 1
        const fill = shown >= starVal ? 1 : 0
        return (
          <Star
            key={i}
            fill={fill}
            interactive={true}
            onClick={() => onChange(String(starVal))}
            onMouseEnter={() => setHover(starVal)}
          />
        )
      })}
      <button
        type="button"
        onClick={() => onChange('')}
        style={{
          marginLeft: '0.4rem',
          fontSize: '0.75em',
          padding: '0 0.3rem',
          cursor: 'pointer'
        }}
        title="Clear rating"
      >
        ✕
      </button>
    </div>
  )
}
