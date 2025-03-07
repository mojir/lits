import type { Any } from '../../interface'

const validAlgebraicReservedNamesRecord = {
  true: { value: true, forbidden: false },
  false: { value: false, forbidden: false },
  nil: { value: null, forbidden: false },
  null: { value: null, forbidden: false },
  E: { value: Math.E, forbidden: false },
  PI: { value: Math.PI, forbidden: false },
  PHI: { value: 1.618033988749895, forbidden: false },
  POSITIVE_INFINITY: { value: Number.POSITIVE_INFINITY, forbidden: false },
  NEGATIVE_INFINITY: { value: Number.NEGATIVE_INFINITY, forbidden: false },
  MAX_SAFE_INTEGER: { value: Number.MAX_SAFE_INTEGER, forbidden: false },
  MIN_SAFE_INTEGER: { value: Number.MIN_SAFE_INTEGER, forbidden: false },
  MAX_VALUE: { value: Number.MAX_VALUE, forbidden: false },
  MIN_VALUE: { value: Number.MIN_VALUE, forbidden: false },
  EPSILON: { value: Number.EPSILON, forbidden: false },
  NaN: { value: Number.NaN, forbidden: false },
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
