import type { Any } from '../../interface'

const validAlgebraicReservedNamesRecord = {
  true: { value: true, forbidden: false },
  false: { value: false, forbidden: false },
  nil: { value: null, forbidden: false },
  null: { value: null, forbidden: false },
  then: { value: null, forbidden: false },
  else: { value: null, forbidden: false },
  end: { value: null, forbidden: false },
} as const satisfies Record<string, { value: Any, forbidden: false }>

const forbiddenAlgebraicReservedNamesRecord = {
  if_let: { value: null, forbidden: true },
  when_let: { value: null, forbidden: true },
  when_first: { value: null, forbidden: true },
  fn: { value: null, forbidden: true },
  defns: { value: null, forbidden: true },
  try: { value: null, forbidden: true },
  recur: { value: null, forbidden: true },
  loop: { value: null, forbidden: true },
  doseq: { value: null, forbidden: true },
} as const satisfies Record<string, { value: Any, forbidden: true }>

export const algebraicReservedNamesRecord = {
  ...validAlgebraicReservedNamesRecord,
  ...forbiddenAlgebraicReservedNamesRecord,
} as const

export type ValidAlgebraicReservedName = keyof typeof validAlgebraicReservedNamesRecord
export type AlgebraicReservedName = keyof typeof algebraicReservedNamesRecord
