import type { Any } from '../../interface'

export const prefixInfixNamesRecord: Record<string, { value: Any, forbidden?: true }> = {
  'true': { value: true },
  'false': { value: false },
  'nil': { value: null },
  'null': { value: null, forbidden: true },
  'undefined': { value: null, forbidden: true },
  '===': { value: null, forbidden: true },
  '!==': { value: null, forbidden: true },
  // 'and': { value: null, forbidden: true },
  // 'or': { value: null, forbidden: true },
  'def': { value: null, forbidden: true },
  'defs': { value: null, forbidden: true },
  'let': { value: null, forbidden: true },
  'if-let': { value: null, forbidden: true },
  'when-let': { value: null, forbidden: true },
  'when-first': { value: null, forbidden: true },
  'fn': { value: null, forbidden: true },
  'defn': { value: null, forbidden: true },
  'defns': { value: null, forbidden: true },
  'try': { value: null, forbidden: true },
  'throw': { value: null, forbidden: true },
  // 'if': { value: null, forbidden: true },
  // 'if-not': { value: null, forbidden: true },
  // 'cond': { value: null, forbidden: true },
  // 'when': { value: null, forbidden: true },
  // 'when-not': { value: null, forbidden: true },
  // 'comment': { value: null, forbidden: true },
  // 'do': { value: null, forbidden: true },
  'recur': { value: null, forbidden: true },
  'loop': { value: null, forbidden: true },
  'time!': { value: null, forbidden: true },
  'doseq': { value: null, forbidden: true },
  'for': { value: null, forbidden: true },
  // 'declared?': { value: null, forbidden: true },
  // '??': { value: null, forbidden: true },

} as const

type InfixReservedName = keyof typeof prefixInfixNamesRecord

export const infixReservedNames: InfixReservedName[] = Object.keys(prefixInfixNamesRecord)
