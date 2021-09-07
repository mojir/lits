import { SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined } from '../../utils'
import { SpecialExpression } from '../interface'

interface AndSpecialExpressionNode extends SpecialExpressionNode {
  name: 'and'
}

export const andSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseParams }) => {
    const { inputPosition } = asNotUndefined(tokens[position])
    const [newPosition, params] = parseParams(tokens, position)
    return [
      newPosition + 1,
      {
        type: 'SpecialExpression',
        name: 'and',
        params,
        inputPosition,
      },
    ]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
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
