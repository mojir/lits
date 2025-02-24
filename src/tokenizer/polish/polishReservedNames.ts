import type { Any } from '../../interface'

export const polishReservedNamesRecord: Record<string, { value: Any, forbidden?: true }> = {
  true: { value: true },
  false: { value: false },
  nil: { value: null },
  null: { value: null },
} as const

export type PolishReservedName = keyof typeof polishReservedNamesRecord

export const polishReservedNames: PolishReservedName[] = Object.keys(polishReservedNamesRecord)
