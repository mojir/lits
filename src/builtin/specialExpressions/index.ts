import { ifSpecialExpression } from './if'
import { letSpecialExpression } from './let'
import { setqSpecialExpression } from './setq'
import { SpecialExpression } from '../interface'
import { andSpecialExpression } from './and'
import { orSpecialExpression } from './or'

type SpecialExpressions = Record<string, SpecialExpression>

export const specialExpressions: SpecialExpressions = {
  let: letSpecialExpression,
  if: ifSpecialExpression,
  setq: setqSpecialExpression,
  and: andSpecialExpression,
  or: orSpecialExpression,
}
