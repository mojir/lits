import { UserDefinedError } from '../../errors'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { getTokenDebugData } from '../../tokenizer/token'
import { asString } from '../../typeGuards/string'
import type { BuiltinSpecialExpression } from '../interface'

export interface ThrowNode extends CommonSpecialExpressionNode<'throw'> {}

export const throwSpecialExpression: BuiltinSpecialExpression<null, ThrowNode> = {
  paramCount: 1,
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const message = asString(evaluateAstNode(node.p[0]!, contextStack), getTokenDebugData(node.token)?.sourceCodeInfo, {
      nonEmpty: true,
    })
    throw new UserDefinedError(message, getTokenDebugData(node.token)?.sourceCodeInfo)
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => getUndefinedSymbols(node.p, contextStack, builtin),
}
