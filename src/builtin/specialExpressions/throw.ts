import { UserDefinedError } from '../../errors'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertNumberOfParams } from '../../typeGuards'
import { asString } from '../../typeGuards/string'
import type { BuiltinSpecialExpression } from '../interface'
import { getCommonParser } from './commonParser'

export interface ThrowNode extends CommonSpecialExpressionNode<'throw'> {}

export const throwSpecialExpression: BuiltinSpecialExpression<null, ThrowNode> = {
  parse: getCommonParser('throw'),
  validateParameterCount: node => assertNumberOfParams(1, node),
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const message = asString(evaluateAstNode(node.p[0]!, contextStack), getTokenDebugData(node.token)?.sourceCodeInfo, {
      nonEmpty: true,
    })
    throw new UserDefinedError(message, getTokenDebugData(node.token)?.sourceCodeInfo)
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
