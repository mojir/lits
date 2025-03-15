import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/types'
import type { BuiltinSpecialExpression } from '../interface'

export interface OrNode extends CommonSpecialExpressionNode<'||'> {}

export const orSpecialExpression: BuiltinSpecialExpression<Any, OrNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    let value: Any = false

    for (const param of node.params) {
      value = evaluateAstNode(param, contextStack)
      if (value)
        break
    }

    return value
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => getUndefinedSymbols(node.params, contextStack, builtin),
}
