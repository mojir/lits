import { Any } from '../../interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, asNotUndefined, assertLength } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface IfSpecialExpressionNode extends SpecialExpressionNode {
  name: `if`
}

export const ifSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = asNotUndefined(tokens[position])
    const [newPosition, params] = parseTokens(tokens, position)
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name: `if`,
        params,
        token: firstToken,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castIfExpressionNode(node)

    const [conditionNode, trueNode, falseNode] = node.params
    if (evaluateAstNode(asAstNode(conditionNode), contextStack)) {
      return evaluateAstNode(asAstNode(trueNode), contextStack)
    } else {
      if (node.params.length === 3) {
        return evaluateAstNode(asAstNode(falseNode), contextStack)
      } else {
        return null
      }
    }
  },
  validate: node => assertLength({ min: 2, max: 3 }, node),
}

function castIfExpressionNode(_node: SpecialExpressionNode): asserts _node is IfSpecialExpressionNode {
  return
}
