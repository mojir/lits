import { Any } from '../../interface'
import { token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

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
        token: firstToken.debugInfo ? firstToken : undefined,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    let value: Any = true

    for (const param of node.params) {
      value = evaluateAstNode(param, contextStack)
      if (!value) {
        break
      }
    }

    return value
  },
  analyze: (node, contextStack, { analyzeAst, builtin }) => analyzeAst(node.params, contextStack, builtin),
}
