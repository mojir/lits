import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/types'
import { asAstNode } from '../../typeGuards/astNode'
import type { BuiltinSpecialExpression } from '../interface'

export interface IfNode extends CommonSpecialExpressionNode<'if'> {}

export const ifSpecialExpression: BuiltinSpecialExpression<Any, IfNode> = {
  paramCount: { min: 2, max: 3 },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const sourceCodeInfo = node.sourceCodeInfo

    const [conditionNode, trueNode, falseNode] = node.params
    if (evaluateAstNode(asAstNode(conditionNode, sourceCodeInfo), contextStack)) {
      return evaluateAstNode(asAstNode(trueNode, sourceCodeInfo), contextStack)
    }
    else {
      if (node.params.length === 3)
        return evaluateAstNode(asAstNode(falseNode, sourceCodeInfo), contextStack)
      else
        return null
    }
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateAstNode }) => getUndefinedSymbols(node.params, contextStack, builtin, evaluateAstNode),
}
