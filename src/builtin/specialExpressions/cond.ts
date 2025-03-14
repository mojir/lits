import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { arrayToPairs } from '../../utils'
import type { BuiltinSpecialExpression } from '../interface'

export interface CondNode extends CommonSpecialExpressionNode<'cond'> {}

export const condSpecialExpression: BuiltinSpecialExpression<Any, CondNode> = {
  paramCount: { even: true },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    for (const [test, form] of arrayToPairs(node.p)) {
      const value = evaluateAstNode(test!, contextStack)
      if (!value)
        continue

      return evaluateAstNode(form!, contextStack)
    }
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => getUndefinedSymbols(node.p, contextStack, builtin),
}
