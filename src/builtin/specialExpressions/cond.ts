import type { Any } from '../../interface'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type CondNode = SpecialExpressionNode<[typeof specialExpressionTypes['cond'], [Node, Node][]]>

export const condSpecialExpression: BuiltinSpecialExpression<Any, CondNode> = {
  paramCount: { even: true },
  evaluate: (node, contextStack, { evaluateNode }) => {
    const params = node[1][1]
    for (const [test, form] of params) {
      const value = evaluateNode(test, contextStack)
      if (!value)
        continue

      return evaluateNode(form, contextStack)
    }
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols(node[1][1].flat(), contextStack, builtin, evaluateNode),
}
