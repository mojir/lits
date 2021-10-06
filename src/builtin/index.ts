import { andSpecialExpression } from './specialExpressions/and'
import { blockSpecialExpression } from './specialExpressions/block'
import { condSpecialExpression } from './specialExpressions/cond'
import { defnSpecialExpression, defnsSpecialExpression, fnSpecialExpression } from './specialExpressions/functions'
import { ifSpecialExpression } from './specialExpressions/if'
import { letSpecialExpression } from './specialExpressions/let'
import { orSpecialExpression } from './specialExpressions/or'
import { doSpecialExpression } from './specialExpressions/do'
import { returnFromSpecialExpression } from './specialExpressions/return-from'
import { returnSpecialExpression } from './specialExpressions/return'
import { defSpecialExpression } from './specialExpressions/def'
import { defsSpecialExpression } from './specialExpressions/defs'
import { throwSpecialExpression } from './specialExpressions/throw'
import { trySpecialExpression } from './specialExpressions/try'
import { unlessSpecialExpression } from './specialExpressions/unless'
import { whenSpecialExpression } from './specialExpressions/when'
import { recurSpecialExpression } from './specialExpressions/recur'
import { loopSpecialExpression } from './specialExpressions/loop'
import { SpecialExpression } from './interface'
import { normalExpressions } from './normalExpressions'

type SpecialExpressions = Record<string, SpecialExpression>

export const specialExpressions: SpecialExpressions = {
  'return-from': returnFromSpecialExpression,
  def: defSpecialExpression,
  defs: defsSpecialExpression,
  and: andSpecialExpression,
  block: blockSpecialExpression,
  cond: condSpecialExpression,
  defn: defnSpecialExpression,
  defns: defnsSpecialExpression,
  if: ifSpecialExpression,
  fn: fnSpecialExpression,
  let: letSpecialExpression,
  or: orSpecialExpression,
  do: doSpecialExpression,
  return: returnSpecialExpression,
  throw: throwSpecialExpression,
  try: trySpecialExpression,
  unless: unlessSpecialExpression,
  when: whenSpecialExpression,
  recur: recurSpecialExpression,
  loop: loopSpecialExpression,
}

Object.keys(specialExpressions).forEach(key => {
  /* istanbul ignore next */
  if (normalExpressions[key]) {
    throw Error(`Expression ${key} is defined as both a normal expression and a special expression`)
  }
})

export const builtin = {
  normalExpressions,
  specialExpressions,
}

export const normalExpressionKeys = Object.keys(normalExpressions)
export const specialExpressionKeys = Object.keys(specialExpressions)
