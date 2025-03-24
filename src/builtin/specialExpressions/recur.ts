import { RecurSignal } from '../../errors'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type RecurNode = SpecialExpressionNode<[typeof specialExpressionTypes['recur'], Node[]]>

export const recurSpecialExpression: BuiltinSpecialExpression<null, RecurNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateNode }) => {
    const params = node[1][1]
    const evaluatedParams = params.map(paramNode => evaluateNode(paramNode, contextStack))
    throw new RecurSignal(evaluatedParams)
  },
  evaluateAsNormalExpression: (params) => {
    throw new RecurSignal(params)
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) =>
    getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode),
}
