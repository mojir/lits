import { SpecialExpressionNode } from '../../parser/interface'
import { SpecialExpression } from '../interface'

interface OrSpecialExpressionNode extends SpecialExpressionNode {
  name: `and`
}

export const orSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseTokens }) => {
    const [newPosition, params] = parseTokens(tokens, position)
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name: `or`,
        params,
      },
    ]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castOrExpressionNode(node)
    let value: unknown = false

    for (const param of node.params) {
      value = evaluateAstNode(param, contextStack)
      if (value) {
        break
      }
    }

    return value
  },
}

function castOrExpressionNode(_node: SpecialExpressionNode): asserts _node is OrSpecialExpressionNode {
  return
}
