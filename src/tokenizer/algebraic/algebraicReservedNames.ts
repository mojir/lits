import type { Any } from '../../interface'

export const algebraicReservedNamesRecord: Record<string, { value: Any, forbidden?: true }> = {
  'true': { value: true },
  'false': { value: false },
  'nil': { value: null },
  'null': { value: null },
  'if_let': { value: null, forbidden: true },
  'when_let': { value: null, forbidden: true },
  'when_first': { value: null, forbidden: true },
  'fn': { value: null, forbidden: true },
  'defns': { value: null, forbidden: true },
  'try': { value: null, forbidden: true },
  'recur': { value: null, forbidden: true },
  'loop': { value: null, forbidden: true },
  'time!': { value: null, forbidden: true },
  'doseq': { value: null, forbidden: true },

} as const

type AlgebraicReservedName = keyof typeof algebraicReservedNamesRecord

export const algebraicReservedNames: AlgebraicReservedName[] = Object.keys(algebraicReservedNamesRecord)
