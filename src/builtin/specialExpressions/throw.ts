import { UserDefinedError } from '../../errors'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import { asString } from '../../typeGuards/string'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type ThrowNode = SpecialExpressionNode<[typeof specialExpressionTypes['throw'], Node]>

export const throwSpecialExpression: BuiltinSpecialExpression<null, ThrowNode> = {
  paramCount: 1,
  evaluate: (node, contextStack, { evaluateNode }) => {
    const message = asString(evaluateNode(node[1][1], contextStack), node[2], {
      nonEmpty: true,
    })
    throw new UserDefinedError(message, node[2])
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols([node[1][1]], contextStack, builtin, evaluateNode),
}
