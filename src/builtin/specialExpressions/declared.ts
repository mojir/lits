import { SpecialExpressionNode } from '../../parser/interface'
import { assertNumberOfParams, token, nameNode } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

export const declaredSpecialExpression: BuiltinSpecialExpression<boolean> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    const [newPosition, params] = parseTokens(tokens, position)
    const node: SpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `declared?`,
      params,
      token: firstToken.debugInfo ? firstToken : undefined,
    }

    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, { lookUp }) => {
    const [astNode] = node.params
    nameNode.assert(astNode, node.token?.debugInfo)

    const lookUpResult = lookUp(astNode, contextStack)
    return !!(lookUpResult.builtinFunction || lookUpResult.contextEntry || lookUpResult.specialExpression)
  },
  validate: node => assertNumberOfParams(1, node),
  analyze: (node, contextStack, { analyzeAst, builtin }) => analyzeAst(node.params, contextStack, builtin),
}
