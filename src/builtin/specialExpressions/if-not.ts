import { Any } from '../../interface'
import { assertNumberOfParams, astNode, token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

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
        token: firstToken.debugInfo ? firstToken : undefined,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const debugInfo = node.token?.debugInfo

    const [conditionNode, trueNode, falseNode] = node.params
    if (!evaluateAstNode(astNode.as(conditionNode, debugInfo), contextStack)) {
      return evaluateAstNode(astNode.as(trueNode, debugInfo), contextStack)
    } else {
      if (node.params.length === 3) {
        return evaluateAstNode(astNode.as(falseNode, debugInfo), contextStack)
      } else {
        return null
      }
    }
  },
  validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  analyze: (node, contextStack, { analyzeAst, builtin }) => analyzeAst(node.params, contextStack, builtin),
}
