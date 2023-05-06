import { Type } from '../../types/Type'
import { Any } from '../../interface'
import { assertNumberOfParams, astNode, token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

export const ifSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    const [newPosition, params] = parseTokens(tokens, position)
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name: `if`,
        params,
        token: firstToken.debugInfo ? firstToken : undefined,
      },
    ]
  },

  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const debugInfo = node.token?.debugInfo

    const [conditionNode, trueNode, falseNode] = node.params
    const conditionValue = evaluateAstNode(astNode.as(conditionNode, debugInfo), contextStack)
    if ((Type.isType(conditionNode) && conditionNode.is(Type.truthy)) || !!conditionValue) {
      return evaluateAstNode(astNode.as(trueNode, debugInfo), contextStack)
    } else if ((Type.isType(conditionNode) && conditionNode.is(Type.falsy)) || !conditionValue) {
      if (node.params.length === 3) {
        return evaluateAstNode(astNode.as(falseNode, debugInfo), contextStack)
      } else {
        return null
      }
    } else {
      const trueBranchValue = evaluateAstNode(astNode.as(trueNode, debugInfo), contextStack)
      const falseBranchValue = evaluateAstNode(astNode.as(falseNode, debugInfo), contextStack)
      return Type.or(Type.of(trueBranchValue), Type.of(falseBranchValue))
    }
  },

  validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2, max: 3 }, arity, `if`, debugInfo),

  findUndefinedSymbols: (node, contextStack, { findUndefinedSymbols, builtin }) =>
    findUndefinedSymbols(node.params, contextStack, builtin),
}
