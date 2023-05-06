import { andSpecialExpression } from './specialExpressions/and'
import { condSpecialExpression } from './specialExpressions/cond'
import { defnSpecialExpression, defnsSpecialExpression, fnSpecialExpression } from './specialExpressions/functions'
import { defSpecialExpression } from './specialExpressions/def'
import { defsSpecialExpression } from './specialExpressions/defs'
import { doSpecialExpression } from './specialExpressions/do'
import { forSpecialExpression, doseqSpecialExpression } from './specialExpressions/loops'
import { ifLetSpecialExpression } from './specialExpressions/if-let'
import { ifNotSpecialExpression } from './specialExpressions/if-not'
import { ifSpecialExpression } from './specialExpressions/if'
import { letSpecialExpression } from './specialExpressions/let'
import { loopSpecialExpression } from './specialExpressions/loop'
import { orSpecialExpression } from './specialExpressions/or'
import { recurSpecialExpression } from './specialExpressions/recur'
import { throwSpecialExpression } from './specialExpressions/throw'
import { timeSpecialExpression } from './specialExpressions/time'
import { trySpecialExpression } from './specialExpressions/try'
import { whenFirstSpecialExpression } from './specialExpressions/when-first'
import { whenLetSpecialExpression } from './specialExpressions/when-let'
import { whenNotSpecialExpression } from './specialExpressions/when-not'
import { whenSpecialExpression } from './specialExpressions/when'
import { Builtin, BuiltinSpecialExpressions } from './interface'
import { normalExpressions } from './normalExpressions'
import { commentSpecialExpression } from './specialExpressions/comment'
import { declaredSpecialExpression } from './specialExpressions/declared'
import { qqSpecialExpression } from './specialExpressions/qq'

export const specialExpressions: BuiltinSpecialExpressions = {
  and: andSpecialExpression,
  comment: commentSpecialExpression,
  cond: condSpecialExpression,
  def: defSpecialExpression,
  defn: defnSpecialExpression,
  defns: defnsSpecialExpression,
  defs: defsSpecialExpression,
  do: doSpecialExpression,
  doseq: doseqSpecialExpression,
  for: forSpecialExpression,
  fn: fnSpecialExpression,
  if: ifSpecialExpression,
  'if-let': ifLetSpecialExpression,
  'if-not': ifNotSpecialExpression,
  let: letSpecialExpression,
  loop: loopSpecialExpression,
  or: orSpecialExpression,
  recur: recurSpecialExpression,
  throw: throwSpecialExpression,
  'time!': timeSpecialExpression,
  try: trySpecialExpression,
  when: whenSpecialExpression,
  'when-first': whenFirstSpecialExpression,
  'when-let': whenLetSpecialExpression,
  'when-not': whenNotSpecialExpression,
  'declared?': declaredSpecialExpression,
  '??': qqSpecialExpression,
}

Object.keys(specialExpressions).forEach(key => {
  /* istanbul ignore next */
  if (normalExpressions[key]) {
    throw Error(`Expression ${key} is defined as both a normal expression and a special expression`)
  }
})

export const builtin: Builtin = {
  normalExpressions,
  specialExpressions,
}

export const normalExpressionKeys = Object.keys(normalExpressions)
export const specialExpressionKeys = Object.keys(specialExpressions)
