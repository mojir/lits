import { Any } from '../../interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { assertNumberOfParams, astNode, token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

interface WhenSpecialExpressionNode extends SpecialExpressionNode {
  name: `when`
}

export const whenSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    const [newPosition, params] = parseTokens(tokens, position)
    const node: WhenSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `when`,
      params,
      token: firstToken,
    }

    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castWhenExpressionNode(node)

    const [whenExpression, ...body] = node.params
    astNode.assert(whenExpression, node.token.sourceCodeInfo)

    if (!evaluateAstNode(whenExpression, contextStack)) {
      return null
    }

    let result: Any = null
    for (const form of body) {
      result = evaluateAstNode(form, contextStack)
    }
    return result
  },
  validate: node => assertNumberOfParams({ min: 1 }, node),
}

function castWhenExpressionNode(_node: SpecialExpressionNode): asserts _node is WhenSpecialExpressionNode {
  return
}
