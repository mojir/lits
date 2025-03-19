import { RecurSignal } from '../../errors'
import type { CommonSpecialExpressionNode } from '../../parser/types'
import type { BuiltinSpecialExpression } from '../interface'

export interface RecurNode extends CommonSpecialExpressionNode<'recur'> {}

export const recurSpecialExpression: BuiltinSpecialExpression<null, RecurNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))
    throw new RecurSignal(params)
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateAstNode }) => getUndefinedSymbols(node.params, contextStack, builtin, evaluateAstNode),
}
