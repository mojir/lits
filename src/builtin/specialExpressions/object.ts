import { LitsError } from '../../errors'
import type { Any, Obj } from '../../interface'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import { isUnknownRecord } from '../../typeGuards'
import { isSpreadNode } from '../../typeGuards/astNode'
import { assertString } from '../../typeGuards/string'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type ObjectNode = SpecialExpressionNode<[typeof specialExpressionTypes['object'], Node[]]>

export const objectSpecialExpression: BuiltinSpecialExpression<Any, ObjectNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateNode }) => {
    const result: Obj = {}

    const params = node[1][1]
    for (let i = 0; i < params.length; i += 2) {
      const keyNode = params[i]!
      if (isSpreadNode(keyNode)) {
        const spreadObject = evaluateNode(keyNode[1], contextStack)
        if (!isUnknownRecord(spreadObject)) {
          throw new LitsError('Spread value is not an object', keyNode[2])
        }
        Object.assign(result, spreadObject)
        i -= 1
      }
      else {
        const key = evaluateNode(keyNode, contextStack)
        const value = evaluateNode(params[i + 1]!, contextStack)
        assertString(key, keyNode[2])
        result[key] = value
      }
    }
    return result
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode),
}
