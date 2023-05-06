import { Any } from '../../interface'
import { assertNumberOfParams, astNode, string, token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'
import { assertNameNotDefined } from '../utils'

export const defsSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    const [newPosition, params] = parseTokens(tokens, position)
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name: `defs`,
        params,
        token: firstToken.debugInfo ? firstToken : undefined,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode, builtin }) => {
    const debugInfo = node.token?.debugInfo
    const name = evaluateAstNode(astNode.as(node.params[0], debugInfo), contextStack)
    string.assert(name, debugInfo)

    assertNameNotDefined(name, contextStack, builtin, node.token?.debugInfo)

    const value = evaluateAstNode(astNode.as(node.params[1], debugInfo), contextStack)

    contextStack.globalContext[name] = { value }

    return value
  },
  validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `defs`, debugInfo),
  findUndefinedSymbols: (node, contextStack, { findUndefinedSymbols, builtin }) => {
    const subNode = astNode.as(node.params[1], node.token?.debugInfo)
    return findUndefinedSymbols(subNode, contextStack, builtin)
  },
}
