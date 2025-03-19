import { LitsError } from '../../errors'
import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/types'
import type { BuiltinSpecialExpression } from '../interface'

export interface ArrayNode extends CommonSpecialExpressionNode<'array'> {}

export const arraySpecialExpression: BuiltinSpecialExpression<Any, ArrayNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const result: Any[] = []

    for (const param of node.params) {
      if (param.type === 'Spread') {
        const spreadValue = evaluateAstNode(param.value, contextStack)
        if (!Array.isArray(spreadValue)) {
          throw new LitsError('Spread value is not an array', param.sourceCodeInfo)
        }
        result.push(...spreadValue as Any[])
      }
      else {
        result.push(evaluateAstNode(param, contextStack))
      }
    }

    return result
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateAstNode }) => getUndefinedSymbols(node.params, contextStack, builtin, evaluateAstNode),
}
