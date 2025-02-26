import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import type { BuiltinSpecialExpression } from '../interface'
import { getCommonParser } from './commonParser'

export interface DoNode extends CommonSpecialExpressionNode<'do'> {}

export const doSpecialExpression: BuiltinSpecialExpression<Any, DoNode> = {
  parse: getCommonParser('do'),
  validateParameterCount: () => undefined,
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const newContext: Context = {}

    const newContextStack = contextStack.create(newContext)
    let result: Any = null
    for (const form of node.p)
      result = evaluateAstNode(form, newContextStack)

    return result
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
