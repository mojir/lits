import type { Builtin } from './interface'
import { normalExpressions } from './normalExpressions'
import { andSpecialExpression } from './specialExpressions/and'
import { commentSpecialExpression } from './specialExpressions/comment'
import { condSpecialExpression } from './specialExpressions/cond'
import { declaredSpecialExpression } from './specialExpressions/declared'
import { defSpecialExpression } from './specialExpressions/def'
import { defsSpecialExpression } from './specialExpressions/defs'
import { doSpecialExpression } from './specialExpressions/do'
import { defnSpecialExpression, defnsSpecialExpression, fnSpecialExpression } from './specialExpressions/functions'
import { ifSpecialExpression } from './specialExpressions/if'
import { ifLetSpecialExpression } from './specialExpressions/if-let'
import { ifNotSpecialExpression } from './specialExpressions/if-not'
import { letSpecialExpression } from './specialExpressions/let'
import { loopSpecialExpression } from './specialExpressions/loop'
import { doseqSpecialExpression, forSpecialExpression } from './specialExpressions/loops'
import { orSpecialExpression } from './specialExpressions/or'
import { qqSpecialExpression } from './specialExpressions/qq'
import { recurSpecialExpression } from './specialExpressions/recur'
import { throwSpecialExpression } from './specialExpressions/throw'
import { timeSpecialExpression } from './specialExpressions/time'
import { trySpecialExpression } from './specialExpressions/try'
import { whenSpecialExpression } from './specialExpressions/when'
import { whenFirstSpecialExpression } from './specialExpressions/when-first'
import { whenLetSpecialExpression } from './specialExpressions/when-let'
import { whenNotSpecialExpression } from './specialExpressions/when-not'

const specialExpressions = {
  'and': andSpecialExpression,
  'comment': commentSpecialExpression,
  'cond': condSpecialExpression,
  'def': defSpecialExpression,
  'defn': defnSpecialExpression,
  'defns': defnsSpecialExpression,
  'defs': defsSpecialExpression,
  'do': doSpecialExpression,
  'doseq': doseqSpecialExpression,
  'for': forSpecialExpression,
  'fn': fnSpecialExpression,
  'if': ifSpecialExpression,
  'if-let': ifLetSpecialExpression,
  'if-not': ifNotSpecialExpression,
  'let': letSpecialExpression,
  'loop': loopSpecialExpression,
  'or': orSpecialExpression,
  'recur': recurSpecialExpression,
  'throw': throwSpecialExpression,
  'time!': timeSpecialExpression,
  'try': trySpecialExpression,
  'when': whenSpecialExpression,
  'when-first': whenFirstSpecialExpression,
  'when-let': whenLetSpecialExpression,
  'when-not': whenNotSpecialExpression,
  'declared?': declaredSpecialExpression,
  '??': qqSpecialExpression,
} as const

export type SpecialExpressionName = keyof typeof specialExpressions
export type BuiltinSpecialExpressions = typeof specialExpressions
export type BuiltinSpecialExpression = typeof specialExpressions[SpecialExpressionName]
export type SpecialExpressionNode = ReturnType<typeof specialExpressions[SpecialExpressionName]['parse']>[1]

Object.keys(specialExpressions).forEach((key) => {
  /* v8 ignore next 2 */
  if (normalExpressions[key])
    throw new Error(`Expression ${key} is defined as both a normal expression and a special expression`)
})

export const builtin: Builtin = {
  normalExpressions,
  specialExpressions,
}

export const normalExpressionKeys = Object.keys(normalExpressions)
export const specialExpressionKeys = Object.keys(specialExpressions)
