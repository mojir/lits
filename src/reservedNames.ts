export type ReservedName = `true` | `false` | `null` | `undefined`
export const reservedNamesRecord: Record<ReservedName, { value: unknown }> = {
  true: { value: true },
  false: { value: false },
  null: { value: null },
  undefined: { value: undefined },
}

export const reservedNames: ReservedName[] = Object.keys(reservedNamesRecord) as ReservedName[]
