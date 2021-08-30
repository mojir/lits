export type ReservedName = 'true' | 'false' | 'null' | 'undefined'
export const reservedNames: Record<ReservedName, { value: unknown }> = {
  true: { value: true },
  false: { value: false },
  null: { value: null },
  undefined: { value: undefined },
}
