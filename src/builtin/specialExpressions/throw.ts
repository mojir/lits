import { UserDefinedError } from '../../errors'
import type { CommonSpecialExpressionNode } from '../../parser/types'
import { tokenSourceCodeInfo } from '../../tokenizer/token'
import { asString } from '../../typeGuards/string'
import type { BuiltinSpecialExpression } from '../interface'

export interface ThrowNode extends CommonSpecialExpressionNode<'throw'> {}

export const throwSpecialExpression: BuiltinSpecialExpression<null, ThrowNode> = {
  paramCount: 1,
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const message = asString(evaluateAstNode(node.params[0]!, contextStack), tokenSourceCodeInfo(node.token), {
      nonEmpty: true,
    })
    throw new UserDefinedError(message, tokenSourceCodeInfo(node.token))
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => getUndefinedSymbols(node.params, contextStack, builtin),
}
