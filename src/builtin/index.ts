import { andSpecialExpression } from './specialExpressions/and'
import { blockSpecialExpression } from './specialExpressions/block'
import { condSpecialExpression } from './specialExpressions/cond'
import { defunSpecialExpression } from './specialExpressions/defun'
import { dolistSpecialExpression } from './specialExpressions/dolist'
import { dotimesSpecialExpression } from './specialExpressions/dotimes'
import { functionSpecialExpression } from './specialExpressions/function'
import { ifSpecialExpression } from './specialExpressions/if'
import { lambdaSpecialExpression } from './specialExpressions/lambda'
import { letSpecialExpression } from './specialExpressions/let'
import { loopSpecialExpression } from './specialExpressions/loop'
import { orSpecialExpression } from './specialExpressions/or'
import { prognSpecialExpression } from './specialExpressions/progn'
import { returnFromSpecialExpression } from './specialExpressions/return-from'
import { returnSpecialExpression } from './specialExpressions/return'
import {
  setqSpecialExpression,
  setqConstantSpecialExpression,
  setqLocalConstantSpecialExpression,
  setqLocalSpecialExpression,
} from './specialExpressions/setq'
import { throwSpecialExpression } from './specialExpressions/throw'
import { trySpecialExpression } from './specialExpressions/try'
import { unlessSpecialExpression } from './specialExpressions/unless'
import { whenSpecialExpression } from './specialExpressions/when'
import { whileSpecialExpression } from './specialExpressions/while'
import { SpecialExpression } from './interface'
import { normalExpressions } from './normalExpressions'

type SpecialExpressions = Record<string, SpecialExpression>

export const specialExpressions: SpecialExpressions = {
  'return-from': returnFromSpecialExpression,
  'setq-constant': setqConstantSpecialExpression,
  'setq-local-constant': setqLocalConstantSpecialExpression,
  'setq-local': setqLocalSpecialExpression,
  and: andSpecialExpression,
  block: blockSpecialExpression,
  cond: condSpecialExpression,
  defun: defunSpecialExpression,
  dolist: dolistSpecialExpression,
  dotimes: dotimesSpecialExpression,
  function: functionSpecialExpression,
  if: ifSpecialExpression,
  lambda: lambdaSpecialExpression,
  let: letSpecialExpression,
  loop: loopSpecialExpression,
  or: orSpecialExpression,
  progn: prognSpecialExpression,
  return: returnSpecialExpression,
  setq: setqSpecialExpression,
  throw: throwSpecialExpression,
  try: trySpecialExpression,
  unless: unlessSpecialExpression,
  when: whenSpecialExpression,
  while: whileSpecialExpression,
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
