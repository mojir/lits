import { Any } from './interface'

export type ReservedName = `true` | `false` | `nil`
export const reservedNamesRecord: Record<ReservedName, { value: Any }> = {
  true: { value: true },
  false: { value: false },
  nil: { value: null },
}

export const reservedNames: ReservedName[] = Object.keys(reservedNamesRecord) as ReservedName[]
