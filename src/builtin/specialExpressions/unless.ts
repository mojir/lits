import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/types'
import { getTokenDebugData } from '../../tokenizer/token'
import { asAstNode } from '../../typeGuards/astNode'
import type { BuiltinSpecialExpression } from '../interface'

export interface UnlessNode extends CommonSpecialExpressionNode<'unless'> {}

export const unlessSpecialExpression: BuiltinSpecialExpression<Any, UnlessNode> = {
  paramCount: { min: 2, max: 3 },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const sourceCodeInfo = getTokenDebugData(node.token)?.sourceCodeInfo

    const [conditionNode, trueNode, falseNode] = node.p
    if (!evaluateAstNode(asAstNode(conditionNode, sourceCodeInfo), contextStack)) {
      return evaluateAstNode(asAstNode(trueNode, sourceCodeInfo), contextStack)
    }
    else {
      if (node.p.length === 3)
        return evaluateAstNode(asAstNode(falseNode, sourceCodeInfo), contextStack)
      else
        return null
    }
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => getUndefinedSymbols(node.p, contextStack, builtin),
}
