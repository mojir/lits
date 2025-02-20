import type { Any } from '../../interface'

export const postfixReservedNamesRecord: Record<string, { value: Any, forbidden?: true }> = {
  'true': { value: true },
  'false': { value: false },
  'nil': { value: null },
  'null': { value: null, forbidden: true },
  'undefined': { value: null, forbidden: true },
  '===': { value: null, forbidden: true },
  '!==': { value: null, forbidden: true },
  '&&': { value: null, forbidden: true },
  '||': { value: null, forbidden: true },
} as const

export type PostfixReservedName = keyof typeof postfixReservedNamesRecord

export const postfixReservedNames: PostfixReservedName[] = Object.keys(postfixReservedNamesRecord)
