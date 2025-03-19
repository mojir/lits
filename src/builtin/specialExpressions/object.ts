import { LitsError } from '../../errors'
import type { Any, Obj } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/types'
import { isUnknownRecord } from '../../typeGuards'
import { asAstNode } from '../../typeGuards/astNode'
import { assertString } from '../../typeGuards/string'
import type { BuiltinSpecialExpression } from '../interface'

export interface ObjectNode extends CommonSpecialExpressionNode<'object'> {}

export const objectSpecialExpression: BuiltinSpecialExpression<Any, ObjectNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const result: Obj = {}

    for (let i = 0; i < node.params.length; i += 2) {
      const keyNode = asAstNode(node.params[i])
      if (keyNode?.type === 'Spread') {
        const spreadObject = evaluateAstNode(keyNode.value, contextStack)
        if (!isUnknownRecord(spreadObject)) {
          throw new LitsError('Spread value is not an object', keyNode.sourceCodeInfo)
        }
        Object.assign(result, spreadObject)
        i -= 1
      }
      else {
        const key = evaluateAstNode(keyNode, contextStack)
        const value = evaluateAstNode(node.params[i + 1]!, contextStack)
        assertString(key, keyNode.sourceCodeInfo)
        result[key] = value
      }
    }
    return result
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateAstNode }) => getUndefinedSymbols(node.params, contextStack, builtin, evaluateAstNode),
}
