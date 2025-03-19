import { UserDefinedError } from '../../errors'
import type { CommonSpecialExpressionNode } from '../../parser/types'
import { asString } from '../../typeGuards/string'
import type { BuiltinSpecialExpression } from '../interface'

export interface ThrowNode extends CommonSpecialExpressionNode<'throw'> {}

export const throwSpecialExpression: BuiltinSpecialExpression<null, ThrowNode> = {
  paramCount: 1,
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const message = asString(evaluateAstNode(node.params[0]!, contextStack), node.sourceCodeInfo, {
      nonEmpty: true,
    })
    throw new UserDefinedError(message, node.sourceCodeInfo)
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateAstNode }) => getUndefinedSymbols(node.params, contextStack, builtin, evaluateAstNode),
}
