import type { Any } from '../../interface'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import { isSymbolNode } from '../../typeGuards/astNode'
import { assertAny } from '../../typeGuards/lits'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type QqNode = SpecialExpressionNode<[typeof specialExpressionTypes['??'], [Node, Node | undefined]]>

export const qqSpecialExpression: BuiltinSpecialExpression<Any, QqNode> = {
  paramCount: { min: 1, max: 2 },
  evaluate: (node, contextStack, { evaluateNode }) => {
    const [firstNode, secondNode] = node[1][1]

    if (isSymbolNode(firstNode)) {
      if (contextStack.lookUp(firstNode) === null)
        return secondNode ? evaluateNode(secondNode, contextStack) : null
    }
    assertAny(firstNode, node[2])
    const firstResult = evaluateNode(firstNode, contextStack)
    return firstResult ?? (secondNode ? evaluateNode(secondNode, contextStack) : null)
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols(node[1][1].filter(n => !!n), contextStack, builtin, evaluateNode),
}
