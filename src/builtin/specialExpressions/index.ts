import { ifSpecialExpression } from './if'
import { letSpecialExpression } from './let'
import { setqSpecialExpression } from './setq'
import { SpecialExpression } from '../interface'

type SpecialExpressions = Record<string, SpecialExpression>

export const specialExpressions: SpecialExpressions = {
  let: letSpecialExpression,
  if: ifSpecialExpression,
  setq: setqSpecialExpression,
}
