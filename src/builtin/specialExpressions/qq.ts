import type { Any } from '../../interface'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import { isUserDefinedSymbolNode } from '../../typeGuards/astNode'
import { asAny, assertAny } from '../../typeGuards/lits'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type QqNode = SpecialExpressionNode<[typeof specialExpressionTypes['??'], [Node, Node | undefined]]>

export const qqSpecialExpression: BuiltinSpecialExpression<Any, QqNode> = {
  arity: { min: 1, max: 2 },
  evaluate: (node, contextStack, { evaluateNode }) => {
    const [firstNode, secondNode] = node[1][1]

    if (isUserDefinedSymbolNode(firstNode) && contextStack.lookUp(firstNode) === null) {
      return secondNode ? evaluateNode(secondNode, contextStack) : null
    }
    assertAny(firstNode, node[2])
    const firstResult = evaluateNode(firstNode, contextStack)
    return firstResult ?? (secondNode ? evaluateNode(secondNode, contextStack) : null)
  },

  evaluateAsNormalExpression: (params, sourceCodeInfo) => {
    const firstParam = asAny(params[0], sourceCodeInfo)
    const secondParam = params[1] !== undefined ? asAny(params[1], sourceCodeInfo) : null
    return firstParam ?? secondParam
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols(node[1][1].filter(n => !!n), contextStack, builtin, evaluateNode),
}
