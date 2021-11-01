import { Any } from '../../interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, asNotUndefined, assertLength } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface IfNotSpecialExpressionNode extends SpecialExpressionNode {
  name: `if-not`
}

export const ifNotSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = asNotUndefined(tokens[position], `EOF`)
    const [newPosition, params] = parseTokens(tokens, position)
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name: `if-not`,
        params,
        token: firstToken,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castIfNotExpressionNode(node)
    const meta = node.token.meta

    const [conditionNode, trueNode, falseNode] = node.params
    if (!evaluateAstNode(asAstNode(conditionNode, meta), contextStack)) {
      return evaluateAstNode(asAstNode(trueNode, meta), contextStack)
    } else {
      if (node.params.length === 3) {
        return evaluateAstNode(asAstNode(falseNode, meta), contextStack)
      } else {
        return null
      }
    }
  },
  validate: node => assertLength({ min: 2, max: 3 }, node),
}

function castIfNotExpressionNode(_node: SpecialExpressionNode): asserts _node is IfNotSpecialExpressionNode {
  return
}
