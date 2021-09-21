import { SpecialExpressionNode } from '../../parser/interface'
import { assertLength, assertNotUndefined } from '../../utils'
import { SpecialExpression } from '../interface'

interface WhenSpecialExpressionNode extends SpecialExpressionNode {
  name: 'when'
}

export const whenSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseParams }) => {
    const [newPosition, params] = parseParams(tokens, position)
    const node: WhenSpecialExpressionNode = {
      type: 'SpecialExpression',
      name: 'when',
      params,
    }

    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castWhenExpressionNode(node)

    const [whenExpression, ...body] = node.params
    assertNotUndefined(whenExpression)

    if (!evaluateAstNode(whenExpression, contextStack)) {
      return undefined
    }

    let result: unknown = undefined
    for (const form of body) {
      result = evaluateAstNode(form, contextStack)
    }
    return result
  },
  validate: node => assertLength({ min: 1 }, node.params),
}

function castWhenExpressionNode(_node: SpecialExpressionNode): asserts _node is WhenSpecialExpressionNode {
  return
}
