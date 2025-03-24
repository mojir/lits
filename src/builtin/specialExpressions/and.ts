import type { Any } from '../../interface'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import { asAny } from '../../typeGuards/lits'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type AndNode = SpecialExpressionNode<[typeof specialExpressionTypes['&&'], Node[]]>

export const andSpecialExpression: BuiltinSpecialExpression<Any, AndNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateNode }) => {
    let value: Any = true

    for (const param of node[1][1]) {
      value = evaluateNode(param, contextStack)
      if (!value)
        break
    }

    return value
  },
  evaluateAsNormalExpression: (params, sourceCodeInfo) => {
    let value: Any = true
    for (const param of params) {
      value = asAny(param, sourceCodeInfo)
      if (!value)
        break
    }
    return value
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode),
}
