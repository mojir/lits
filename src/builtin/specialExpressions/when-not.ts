import { Any } from '../../interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { assertLength, assertNotUndefined } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface WhenNotSpecialExpressionNode extends SpecialExpressionNode {
  name: `when-not`
}

export const whenNotSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens }) => {
    const [newPosition, params] = parseTokens(tokens, position)
    const node: WhenNotSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `when-not`,
      params,
    }

    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castWhenNotExpressionNode(node)

    const [whenExpression, ...body] = node.params
    assertNotUndefined(whenExpression)

    if (evaluateAstNode(whenExpression, contextStack)) {
      return null
    }

    let result: Any = null
    for (const form of body) {
      result = evaluateAstNode(form, contextStack)
    }
    return result
  },
  validate: node => assertLength({ min: 1 }, node),
}

function castWhenNotExpressionNode(_node: SpecialExpressionNode): asserts _node is WhenNotSpecialExpressionNode {
  return
}