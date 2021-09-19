import { ifSpecialExpression } from './specialExpressions/if'
import { letSpecialExpression } from './specialExpressions/let'
import { setqSpecialExpression } from './specialExpressions/setq'
import { andSpecialExpression } from './specialExpressions/and'
import { orSpecialExpression } from './specialExpressions/or'
import { condSpecialExpression } from './specialExpressions/cond'
import { defunSpecialExpression } from './specialExpressions/defun'
import { functionSpecialExpression } from './specialExpressions/function'
import { lambdaSpecialExpression } from './specialExpressions/lambda'
import { returnFromSpecialExpression } from './specialExpressions/return-from'
import { returnSpecialExpression } from './specialExpressions/return'
import { blockSpecialExpression } from './specialExpressions/block'
import { trySpecialExpression } from './specialExpressions/try'
import { throwSpecialExpression } from './specialExpressions/throw'
import { normalExpressions } from './normalExpressions'
import { SpecialExpression } from './interface'

type SpecialExpressions = Record<string, SpecialExpression>

export const specialExpressions: SpecialExpressions = {
  let: letSpecialExpression,
  if: ifSpecialExpression,
  setq: setqSpecialExpression,
  and: andSpecialExpression,
  or: orSpecialExpression,
  cond: condSpecialExpression,
  defun: defunSpecialExpression,
  function: functionSpecialExpression,
  lambda: lambdaSpecialExpression,
  'return-from': returnFromSpecialExpression,
  return: returnSpecialExpression,
  block: blockSpecialExpression,
  try: trySpecialExpression,
  throw: throwSpecialExpression,
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
