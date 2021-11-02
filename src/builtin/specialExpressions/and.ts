import { Any } from '../../interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

interface AndSpecialExpressionNode extends SpecialExpressionNode {
  name: `and`
}

export const andSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    const [newPosition, params] = parseTokens(tokens, position)
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name: `and`,
        params,
        token: firstToken,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castAndExpressionNode(node)
    let value: Any = true

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
