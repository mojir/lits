import { andSpecialExpression } from './specialExpressions/and'
import { condSpecialExpression } from './specialExpressions/cond'
import { defnSpecialExpression, defnsSpecialExpression, fnSpecialExpression } from './specialExpressions/functions'
import { ifSpecialExpression } from './specialExpressions/if'
import { ifNotSpecialExpression } from './specialExpressions/if-not'
import { ifLetSpecialExpression } from './specialExpressions/if-let'
import { whenLetSpecialExpression } from './specialExpressions/when-let'
import { whenNotSpecialExpression } from './specialExpressions/when-not'
import { whenFirstSpecialExpression } from './specialExpressions/when-first'
import { letSpecialExpression } from './specialExpressions/let'
import { orSpecialExpression } from './specialExpressions/or'
import { doSpecialExpression } from './specialExpressions/do'
import { defSpecialExpression } from './specialExpressions/def'
import { defsSpecialExpression } from './specialExpressions/defs'
import { throwSpecialExpression } from './specialExpressions/throw'
import { trySpecialExpression } from './specialExpressions/try'
import { whenSpecialExpression } from './specialExpressions/when'
import { recurSpecialExpression } from './specialExpressions/recur'
import { loopSpecialExpression } from './specialExpressions/loop'
import { timeSpecialExpression } from './specialExpressions/time'
import { Builtin, BuiltinSpecialExpressions } from './interface'
import { normalExpressions } from './normalExpressions'

export const specialExpressions: BuiltinSpecialExpressions = {
  def: defSpecialExpression,
  defs: defsSpecialExpression,
  and: andSpecialExpression,
  cond: condSpecialExpression,
  defn: defnSpecialExpression,
  defns: defnsSpecialExpression,
  if: ifSpecialExpression,
  'if-not': ifNotSpecialExpression,
  'if-let': ifLetSpecialExpression,
  'when-let': whenLetSpecialExpression,
  'when-not': whenNotSpecialExpression,
  'when-first': whenFirstSpecialExpression,
  fn: fnSpecialExpression,
  let: letSpecialExpression,
  or: orSpecialExpression,
  do: doSpecialExpression,
  throw: throwSpecialExpression,
  try: trySpecialExpression,
  when: whenSpecialExpression,
  recur: recurSpecialExpression,
  loop: loopSpecialExpression,
  'time!': timeSpecialExpression,
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
