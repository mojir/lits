import type { SpecialExpressionNode, SymbolNode } from '../../parser/types'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type DefinedNode = SpecialExpressionNode<[typeof specialExpressionTypes['defined?'], SymbolNode]>

export const declaredSpecialExpression: BuiltinSpecialExpression<boolean, DefinedNode> = {
  paramCount: 1,
  evaluate: (node, contextStack) => {
    const lookUpResult = contextStack.lookUp(node[1][1])
    return lookUpResult !== null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols([node[1][1]], contextStack, builtin, evaluateNode),
}
