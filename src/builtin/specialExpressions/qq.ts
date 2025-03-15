import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/types'
import { tokenSourceCodeInfo } from '../../tokenizer/token'
import { isSymbolNode } from '../../typeGuards/astNode'
import { assertAny } from '../../typeGuards/lits'
import type { BuiltinSpecialExpression } from '../interface'

export interface QqNode extends CommonSpecialExpressionNode<'??'> {}

export const qqSpecialExpression: BuiltinSpecialExpression<Any, QqNode> = {
  paramCount: { min: 1, max: 2 },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const [firstNode, secondNode] = node.params

    if (isSymbolNode(firstNode)) {
      if (contextStack.lookUp(firstNode) === null)
        return secondNode ? evaluateAstNode(secondNode, contextStack) : null
    }
    assertAny(firstNode, tokenSourceCodeInfo(node.token))
    const firstResult = evaluateAstNode(firstNode, contextStack)
    return firstResult ?? (secondNode ? evaluateAstNode(secondNode, contextStack) : null)
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => getUndefinedSymbols(node.params, contextStack, builtin),
}
