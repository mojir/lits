import { Any } from './interface'

export type ReservedName = `true` | `false` | `null`
export const reservedNamesRecord: Record<ReservedName, { value: Any }> = {
  true: { value: true },
  false: { value: false },
  null: { value: null },
}

export const reservedNames: ReservedName[] = Object.keys(reservedNamesRecord) as ReservedName[]
