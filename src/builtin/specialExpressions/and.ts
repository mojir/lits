import { SpecialExpressionNode } from '../../parser/interface'
import { BuiltinSpecialExpression } from '../interface'

interface AndSpecialExpressionNode extends SpecialExpressionNode {
  name: `and`
}

export const andSpecialExpression: BuiltinSpecialExpression = {
  parse: (tokens, position, { parseTokens }) => {
    const [newPosition, params] = parseTokens(tokens, position)
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name: `and`,
        params,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castAndExpressionNode(node)
    let value: unknown = true

    for (const param of node.params) {
      value = evaluateAstNode(param, contextStack)
      if (!value) {
        break
      }
    }

    return value
  },
}

function castAndExpressionNode(_node: SpecialExpressionNode): asserts _node is AndSpecialExpressionNode {
  return
}
