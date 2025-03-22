import type { Any } from '../../interface'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type SwitchNode = SpecialExpressionNode<[typeof specialExpressionTypes['switch'], Node, [Node, Node][]]>

export const switchSpecialExpression: BuiltinSpecialExpression<Any, SwitchNode> = {
  paramCount: { odd: true },
  evaluate: (node, contextStack, { evaluateNode }) => {
    const [, switchValueNode, cases] = node[1]
    const switchValue = evaluateNode(switchValueNode, contextStack)
    for (const [test, form] of cases) {
      const value = evaluateNode(test, contextStack)
      if (value === switchValue) {
        return evaluateNode(form, contextStack)
      }
    }
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols([node[1][1], ...node[1][2].flat()], contextStack, builtin, evaluateNode),
}
