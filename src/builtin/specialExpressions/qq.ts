import { Any } from '../../interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { assertNumberOfParams, token, nameNode, any } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

export const qqSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    const [newPosition, params] = parseTokens(tokens, position)
    const node: SpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `??`,
      params,
      token: firstToken.debugInfo ? firstToken : undefined,
    }

    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, { lookUp, evaluateAstNode }) => {
    const [firstNode, secondNode] = node.params

    if (nameNode.is(firstNode)) {
      const lookUpResult = lookUp(firstNode, contextStack)
      if (!(lookUpResult.builtinFunction || lookUpResult.contextEntry || lookUpResult.specialExpression)) {
        return secondNode ? evaluateAstNode(secondNode, contextStack) : null
      }
    }
    any.assert(firstNode, node.token?.debugInfo)
    const firstResult = evaluateAstNode(firstNode, contextStack)
    return firstResult ? firstResult : secondNode ? evaluateAstNode(secondNode, contextStack) : firstResult
  },
  validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1, max: 2 }, arity, `declared?`, debugInfo),
  findUndefinedSymbols: (node, contextStack, { findUndefinedSymbols, builtin }) =>
    findUndefinedSymbols(node.params, contextStack, builtin),
}
