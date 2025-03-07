import type { Any } from '../../interface'

const nonNumberReservedSymbolRecord = {
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

const phi = (1 + Math.sqrt(5)) / 2
export const numberReservedSymbolRecord = {
  'E': { value: Math.E, forbidden: false },
  '-E': { value: -Math.E, forbidden: false },
  'ε': { value: Math.E, forbidden: false },
  '-ε': { value: -Math.E, forbidden: false },
  'PI': { value: Math.PI, forbidden: false },
  '-PI': { value: -Math.PI, forbidden: false },
  'π': { value: Math.PI, forbidden: false },
  '-π': { value: -Math.PI, forbidden: false },
  'PHI': { value: phi, forbidden: false },
  '-PHI': { value: -phi, forbidden: false },
  'φ': { value: phi, forbidden: false },
  '-φ': { value: -phi, forbidden: false },
  'POSITIVE_INFINITY': { value: Number.POSITIVE_INFINITY, forbidden: false },
  '∞': { value: Number.POSITIVE_INFINITY, forbidden: false },
  'NEGATIVE_INFINITY': { value: Number.NEGATIVE_INFINITY, forbidden: false },
  '-∞': { value: Number.NEGATIVE_INFINITY, forbidden: false },
  'MAX_SAFE_INTEGER': { value: Number.MAX_SAFE_INTEGER, forbidden: false },
  'MIN_SAFE_INTEGER': { value: Number.MIN_SAFE_INTEGER, forbidden: false },
  'MAX_VALUE': { value: Number.MAX_VALUE, forbidden: false },
  'MIN_VALUE': { value: Number.MIN_VALUE, forbidden: false },
  'EPSILON': { value: Number.EPSILON, forbidden: false },
  '-EPSILON': { value: -Number.EPSILON, forbidden: false },
  'NaN': { value: Number.NaN, forbidden: false },
} as const satisfies Record<string, { value: number, forbidden: false }>

const forbiddenAlgebraicReservedSymbolRecord = {
  fn: { value: null, forbidden: true },
} as const satisfies Record<string, { value: Any, forbidden: true }>

export const algebraicReservedSymbolRecord = {
  ...nonNumberReservedSymbolRecord,
  ...numberReservedSymbolRecord,
  ...forbiddenAlgebraicReservedSymbolRecord,
} as const

export const validReservedSymbolRecord = {
  ...nonNumberReservedSymbolRecord,
  ...numberReservedSymbolRecord,
} as const

export type ValidReservedSymbol = keyof typeof validReservedSymbolRecord
export type AlgebraicReservedSymbol = keyof typeof algebraicReservedSymbolRecord

export function isNumberReservedSymbol(symbol: string): symbol is keyof typeof numberReservedSymbolRecord {
  return symbol in numberReservedSymbolRecord
}
