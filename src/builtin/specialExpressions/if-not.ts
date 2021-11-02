import { Any } from '../../interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { assertLength } from '../../utils'
import { astNode, token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

interface IfNotSpecialExpressionNode extends SpecialExpressionNode {
  name: `if-not`
}

export const ifNotSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
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
    const sourceCodeInfo = node.token.sourceCodeInfo

    const [conditionNode, trueNode, falseNode] = node.params
    if (!evaluateAstNode(astNode.as(conditionNode, sourceCodeInfo), contextStack)) {
      return evaluateAstNode(astNode.as(trueNode, sourceCodeInfo), contextStack)
    } else {
      if (node.params.length === 3) {
        return evaluateAstNode(astNode.as(falseNode, sourceCodeInfo), contextStack)
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
