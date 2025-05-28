import type { Any } from '../../interface'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type UnlessNode = SpecialExpressionNode<[typeof specialExpressionTypes['unless'], [Node, Node, Node?]]>

export const unlessSpecialExpression: BuiltinSpecialExpression<Any, UnlessNode> = {
  arity: {},
  evaluate: (node, contextStack, { evaluateNode }) => {
    const [conditionNode, trueNode, falseNode] = node[1][1]
    if (!evaluateNode(conditionNode, contextStack)) {
      return evaluateNode(trueNode, contextStack)
    }
    else if (falseNode) {
      return evaluateNode(falseNode, contextStack)
    }
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) =>
    getUndefinedSymbols(node[1][1].filter(n => !!n), contextStack, builtin, evaluateNode),
}
