/* istanbul ignore file */

import { ifSpecialExpression } from './if'
import { letSpecialExpression } from './let'
import { setqSpecialExpression } from './setq'
import { SpecialExpression } from '../interface'
import { andSpecialExpression } from './and'
import { orSpecialExpression } from './or'
import { condSpecialExpression } from './cond'
import { defunSpecialExpression } from './defun'
import { functionSpecialExpression } from './function'
import { lambdaSpecialExpression } from './lambda'

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
}
