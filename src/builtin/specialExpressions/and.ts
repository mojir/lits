import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/types'
import type { BuiltinSpecialExpression } from '../interface'

export interface AndNode extends CommonSpecialExpressionNode<'&&'> {}

export const andSpecialExpression: BuiltinSpecialExpression<Any, AndNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    let value: Any = true

    for (const param of node.p) {
      value = evaluateAstNode(param, contextStack)
      if (!value)
        break
    }

    return value
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => getUndefinedSymbols(node.p, contextStack, builtin),
}
