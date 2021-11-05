import { Any } from './interface'

export type ReservedName = `true` | `false` | `nil` | `null` | `undefined` | `===` | `!==` | `&&` | `||`
export const reservedNamesRecord: Record<ReservedName, { value: Any; forbidden?: true }> = {
  true: { value: true },
  false: { value: false },
  nil: { value: null },
  null: { value: null, forbidden: true },
  undefined: { value: null, forbidden: true },
  '===': { value: null, forbidden: true },
  '!==': { value: null, forbidden: true },
  '&&': { value: null, forbidden: true },
  '||': { value: null, forbidden: true },
}

export const reservedNames: ReservedName[] = Object.keys(reservedNamesRecord) as ReservedName[]
