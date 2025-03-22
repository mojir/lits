import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type DoNode = SpecialExpressionNode<[typeof specialExpressionTypes['do'], Node[]]>

export const doSpecialExpression: BuiltinSpecialExpression<Any, DoNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateNode }) => {
    const newContext: Context = {}

    const newContextStack = contextStack.create(newContext)
    let result: Any = null
    for (const form of node[1][1])
      result = evaluateNode(form, newContextStack)

    return result
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    return getUndefinedSymbols(node[1][1], contextStack.create({}), builtin, evaluateNode)
  },
}
