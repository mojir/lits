import { SpecialExpressionNode } from '../../parser/interface'
import { assertLength, assertNotUndefined } from '../../utils'
import { SpecialExpression } from '../interface'

interface UnlessSpecialExpressionNode extends SpecialExpressionNode {
  name: `unless`
}

export const unlessSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseParams }) => {
    const [newPosition, params] = parseParams(tokens, position)
    const node: UnlessSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `unless`,
      params,
    }

    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castUnlessExpressionNode(node)

    const [unlessExpression, ...body] = node.params
    assertNotUndefined(unlessExpression)

    if (evaluateAstNode(unlessExpression, contextStack)) {
      return undefined
    }

    let result: unknown = undefined
    for (const form of body) {
      result = evaluateAstNode(form, contextStack)
    }
    return result
  },
  validate: node => assertLength({ min: 1 }, node),
}

function castUnlessExpressionNode(_node: SpecialExpressionNode): asserts _node is UnlessSpecialExpressionNode {
  return
}
