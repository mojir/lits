import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/types'
import { arrayToPairs } from '../../utils'
import type { BuiltinSpecialExpression } from '../interface'

export interface SwitchNode extends CommonSpecialExpressionNode<'switch'> {}

export const switchSpecialExpression: BuiltinSpecialExpression<Any, SwitchNode> = {
  paramCount: { odd: true },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const switchValue = evaluateAstNode(node.params[0]!, contextStack)
    for (const [test, form] of arrayToPairs(node.params.slice(1))) {
      const value = evaluateAstNode(test!, contextStack)
      if (value === switchValue) {
        return evaluateAstNode(form!, contextStack)
      }
    }
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => getUndefinedSymbols(node.params, contextStack, builtin),
}
