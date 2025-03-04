import type { Builtin } from './interface'
import { normalExpressions } from './normalExpressions'
import { andSpecialExpression } from './specialExpressions/and'
import { commentSpecialExpression } from './specialExpressions/comment'
import { condSpecialExpression } from './specialExpressions/cond'
import { switchSpecialExpression } from './specialExpressions/switch'
import { declaredSpecialExpression } from './specialExpressions/declared'
import { defSpecialExpression } from './specialExpressions/def'
import { doSpecialExpression } from './specialExpressions/do'
import { defnSpecialExpression, fnSpecialExpression } from './specialExpressions/functions'
import { ifSpecialExpression } from './specialExpressions/if'
import { unlessSpecialExpression } from './specialExpressions/unless'
import { letSpecialExpression } from './specialExpressions/let'
import { loopSpecialExpression } from './specialExpressions/loop'
import { doseqSpecialExpression, forSpecialExpression } from './specialExpressions/loops'
import { orSpecialExpression } from './specialExpressions/or'
import { qqSpecialExpression } from './specialExpressions/qq'
import { recurSpecialExpression } from './specialExpressions/recur'
import { throwSpecialExpression } from './specialExpressions/throw'
import { trySpecialExpression } from './specialExpressions/try'

const specialExpressions = {
  '&&': andSpecialExpression,
  'comment': commentSpecialExpression,
  'cond': condSpecialExpression,
  'switch': switchSpecialExpression,
  'def': defSpecialExpression,
  'defn': defnSpecialExpression,
  'do': doSpecialExpression,
  'doseq': doseqSpecialExpression,
  'for': forSpecialExpression,
  'fn': fnSpecialExpression,
  'if': ifSpecialExpression,
  'unless': unlessSpecialExpression,
  'let': letSpecialExpression,
  'loop': loopSpecialExpression,
  '||': orSpecialExpression,
  'recur': recurSpecialExpression,
  'throw': throwSpecialExpression,
  'try': trySpecialExpression,
  'defined?': declaredSpecialExpression,
  '??': qqSpecialExpression,
} as const

export type SpecialExpressionName = keyof typeof specialExpressions
export type CommonSpecialExpressionName = keyof Pick<
  typeof specialExpressions,
  | '??'
  | '&&'
  | 'comment'
  | 'cond'
  | 'switch'
  | 'defined?'
  | 'do'
  | 'if'
  | 'unless'
  | '||'
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
