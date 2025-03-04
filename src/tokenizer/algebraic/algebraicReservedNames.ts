import type { Any } from '../../interface'

const validAlgebraicReservedNamesRecord = {
  true: { value: true, forbidden: false },
  false: { value: false, forbidden: false },
  nil: { value: null, forbidden: false },
  null: { value: null, forbidden: false },
  then: { value: null, forbidden: false },
  else: { value: null, forbidden: false },
  end: { value: null, forbidden: false },
  case: { value: null, forbidden: false },
  when: { value: null, forbidden: false },
  while: { value: null, forbidden: false },
  function: { value: null, forbidden: false },
  export: { value: null, forbidden: false },
} as const satisfies Record<string, { value: Any, forbidden: false }>

const forbiddenAlgebraicReservedNamesRecord = {
  fn: { value: null, forbidden: true },
} as const satisfies Record<string, { value: Any, forbidden: true }>

export const algebraicReservedNamesRecord = {
  ...validAlgebraicReservedNamesRecord,
  ...forbiddenAlgebraicReservedNamesRecord,
} as const

export type ValidAlgebraicReservedName = keyof typeof validAlgebraicReservedNamesRecord
export type AlgebraicReservedName = keyof typeof algebraicReservedNamesRecord
