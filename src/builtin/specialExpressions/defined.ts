import type { SpecialExpressionNode, SymbolNode } from '../../parser/types'
import { assertSymbolNode, isUserDefinedSymbolNode } from '../../typeGuards/astNode'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type DefinedNode = SpecialExpressionNode<[typeof specialExpressionTypes['defined?'], SymbolNode]>

export const definedSpecialExpression: BuiltinSpecialExpression<boolean, DefinedNode> = {
  paramCount: 1,
  evaluate: (node, contextStack) => {
    const symbolNode = node[1][1]
    assertSymbolNode(symbolNode)
    if (!isUserDefinedSymbolNode(symbolNode)) {
      return true // If the symbol is not a user defined symbol, it is defined. normal or special builtin
    }
    const lookUpResult = contextStack.lookUp(symbolNode)
    return lookUpResult !== null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols([node[1][1]], contextStack, builtin, evaluateNode),
}
