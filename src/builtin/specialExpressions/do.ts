import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import type { BuiltinSpecialExpression } from '../interface'
import { getCommonPolishSpecialExpressionParser } from './commonParser'

export interface DoNode extends CommonSpecialExpressionNode<'do'> {}

export const doSpecialExpression: BuiltinSpecialExpression<Any, DoNode> = {
  polishParse: getCommonPolishSpecialExpressionParser('do'),
  validateParameterCount: () => undefined,
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const newContext: Context = {}

    const newContextStack = contextStack.create(newContext)
    let result: Any = null
    for (const form of node.p)
      result = evaluateAstNode(form, newContextStack)

    return result
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => {
    return findUnresolvedIdentifiers(node.p, contextStack.create({}), builtin)
  },
}
