import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertNumberOfParams } from '../../typeGuards'
import { assertAstNode } from '../../typeGuards/astNode'
import type { BuiltinSpecialExpression } from '../interface'
import { getCommonPolishSpecialExpressionParser } from './commonParser'

export interface WhenNode extends CommonSpecialExpressionNode<'when'> {}

export const whenSpecialExpression: BuiltinSpecialExpression<Any, WhenNode> = {
  polishParse: getCommonPolishSpecialExpressionParser('when'),
  validateParameterCount: node => assertNumberOfParams({ min: 1 }, node),
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const [whenExpression, ...body] = node.p
    assertAstNode(whenExpression, getTokenDebugData(node.token)?.sourceCodeInfo)

    if (!evaluateAstNode(whenExpression, contextStack))
      return null

    let result: Any = null
    for (const form of body)
      result = evaluateAstNode(form, contextStack)

    return result
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
