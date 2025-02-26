import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import type { BuiltinSpecialExpression } from '../interface'
import { getCommonParser } from './commonParser'

export interface OrNode extends CommonSpecialExpressionNode<'or'> {}

export const orSpecialExpression: BuiltinSpecialExpression<Any, OrNode> = {
  parse: getCommonParser('or'),
  validateParameterCount: () => undefined,
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    let value: Any = false

    for (const param of node.p) {
      value = evaluateAstNode(param, contextStack)
      if (value)
        break
    }

    return value
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
