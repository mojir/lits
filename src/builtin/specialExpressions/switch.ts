import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { arrayToPairs } from '../../utils'
import type { BuiltinSpecialExpression } from '../interface'
import { getCommonPolishSpecialExpressionParser } from './commonParser'

export interface SwitchNode extends CommonSpecialExpressionNode<'switch'> {}

export const switchSpecialExpression: BuiltinSpecialExpression<Any, SwitchNode> = {
  polishParse: getCommonPolishSpecialExpressionParser('switch'),
  paramCount: { odd: true },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const switchValue = evaluateAstNode(node.p[0]!, contextStack)
    for (const [test, form] of arrayToPairs(node.p.slice(1))) {
      const value = evaluateAstNode(test!, contextStack)
      if (value === switchValue) {
        return evaluateAstNode(form!, contextStack)
      }
    }
    return null
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
