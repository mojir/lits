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
import { ifLetSpecialExpression } from './specialExpressions/if_let'
import { ifNotSpecialExpression } from './specialExpressions/if_not'
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
import { whenFirstSpecialExpression } from './specialExpressions/when_first'
import { whenLetSpecialExpression } from './specialExpressions/when_let'
import { whenNotSpecialExpression } from './specialExpressions/when_not'

const specialExpressions = {
  '&&': andSpecialExpression,
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
  'if_let': ifLetSpecialExpression,
  'if_not': ifNotSpecialExpression,
  'let': letSpecialExpression,
  'loop': loopSpecialExpression,
  '||': orSpecialExpression,
  'recur': recurSpecialExpression,
  'throw': throwSpecialExpression,
  'time!': timeSpecialExpression,
  'try': trySpecialExpression,
  'when': whenSpecialExpression,
  'when_first': whenFirstSpecialExpression,
  'when_let': whenLetSpecialExpression,
  'when_not': whenNotSpecialExpression,
  'declared?': declaredSpecialExpression,
  '??': qqSpecialExpression,
} as const

export type SpecialExpressionName = keyof typeof specialExpressions
export type CommonSpecialExpressionName = keyof Pick<
  typeof specialExpressions,
  | '??'
  | '&&'
  | 'comment'
  | 'cond'
  | 'declared?'
  | 'do'
  | 'if'
  | 'if_not'
  | '||'
  | 'time!'
  | 'when'
  | 'when_not'
  | 'throw'
>

export type BuiltinSpecialExpressions = typeof specialExpressions
export type BuiltinSpecialExpression = typeof specialExpressions[SpecialExpressionName]
export type BuiltinCommonSpecialExpression = typeof specialExpressions[CommonSpecialExpressionName]
export type GenericCommonSpecialExpressionNode<T extends CommonSpecialExpressionName> = ReturnType<typeof specialExpressions[T]['polishParse']>
export type SpecialExpressionNode = ReturnType<typeof specialExpressions[SpecialExpressionName]['polishParse']>

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
