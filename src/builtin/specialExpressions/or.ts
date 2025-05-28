import type { Any } from '../../interface'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import { asAny } from '../../typeGuards/lits'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type OrNode = SpecialExpressionNode<[typeof specialExpressionTypes['||'], Node[]]>

export const orSpecialExpression: BuiltinSpecialExpression<Any, OrNode> = {
  arity: {},
  evaluate: (node, contextStack, { evaluateNode }) => {
    let value: Any = false

    for (const param of node[1][1]) {
      value = evaluateNode(param, contextStack)
      if (value)
        break
    }

    return value
  },
  evaluateAsNormalExpression: (params, sourceCodeInfo) => {
    let value: Any = false
    for (const param of params) {
      value = asAny(param, sourceCodeInfo)
      if (value)
        break
    }
    return value
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode),
}
