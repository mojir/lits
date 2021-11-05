import { Any } from './interface'

export type ReservedName = `true` | `false` | `nil` | `null` | `undefined`
export const reservedNamesRecord: Record<ReservedName, { value: Any }> = {
  true: { value: true },
  false: { value: false },
  nil: { value: null },
  null: { value: null },
  undefined: { value: null },
}

export const reservedNames: ReservedName[] = Object.keys(reservedNamesRecord) as ReservedName[]
