import { Any } from '../../interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { assertNumberOfParams, astNode, token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

export const whenSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    const [newPosition, params] = parseTokens(tokens, position)
    const node: SpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `when`,
      params,
      token: firstToken.debugInfo ? firstToken : undefined,
    }

    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const [whenExpression, ...body] = node.params
    astNode.assert(whenExpression, node.token?.debugInfo)

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
  analyze: (node, contextStack, { analyzeAst, builtin }) => analyzeAst(node.params, contextStack, builtin),
}
