import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import type { BuiltinSpecialExpression } from '../interface'
import { getCommonPolishSpecialExpressionParser } from './commonParser'

export interface OrNode extends CommonSpecialExpressionNode<'||'> {}

export const orSpecialExpression: BuiltinSpecialExpression<Any, OrNode> = {
  polishParse: getCommonPolishSpecialExpressionParser('||'),
  paramCount: {},
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    let value: Any = false

    for (const param of node.p) {
      value = evaluateAstNode(param, contextStack)
      if (value)
        break
    }

    return value
  },
  findUnresolvedSymbols: (node, contextStack, { findUnresolvedSymbols, builtin }) => findUnresolvedSymbols(node.p, contextStack, builtin),
}
