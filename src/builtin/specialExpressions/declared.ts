import type { CommonSpecialExpressionNode, SymbolNode } from '../../parser/types'
import type { BuiltinSpecialExpression } from '../interface'

export interface DeclaredNode extends CommonSpecialExpressionNode<'defined?'> {}

export const declaredSpecialExpression: BuiltinSpecialExpression<boolean, DeclaredNode> = {
  paramCount: 1,
  evaluate: (node, contextStack) => {
    const lookUpResult = contextStack.lookUp(node.params[0] as SymbolNode)
    return lookUpResult !== null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateAstNode }) => getUndefinedSymbols(node.params, contextStack, builtin, evaluateAstNode),
}
