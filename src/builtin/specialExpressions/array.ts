import { LitsError } from '../../errors'
import type { Any, Arr } from '../../interface'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import { isSpreadNode } from '../../typeGuards/astNode'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type ArrayNode = SpecialExpressionNode<[typeof specialExpressionTypes['array'], Node[]]>

export const arraySpecialExpression: BuiltinSpecialExpression<Any, ArrayNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateNode }) => {
    const result: Arr = []

    for (const param of node[1][1]) {
      if (isSpreadNode(param)) {
        const spreadValue = evaluateNode(param[1], contextStack)
        if (!Array.isArray(spreadValue)) {
          throw new LitsError('Spread value is not an array', param[2])
        }
        result.push(...spreadValue)
      }
      else {
        result.push(evaluateNode(param, contextStack))
      }
    }

    return result
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode),
}
