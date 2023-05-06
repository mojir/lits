import { Type } from '../../types/Type'
import { Any } from '../../interface'
import { asValue, token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

export const orSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    const [newPosition, params] = parseTokens(tokens, position)
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name: `or`,
        params,
        token: firstToken.debugInfo ? firstToken : undefined,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const possibleValues: Any[] = []
    let value: Any = false

    if (node.params.length === 0) {
      return false
    }

    for (const param of node.params) {
      value = evaluateAstNode(param, contextStack)
      if ((Type.isType(value) && value.is(Type.truthy)) || value) {
        possibleValues.push(value)
        break
      } else if (Type.isType(value) && value.intersects(Type.truthy)) {
        possibleValues.push(value)
      }
    }

    if (possibleValues.length === 0) {
      return value
    } else if (possibleValues.length === 1) {
      return asValue(possibleValues[0])
    } else {
      return Type.or(...possibleValues.map(Type.of))
    }
  },

  validateArity: () => undefined,

  findUndefinedSymbols: (node, contextStack, { findUndefinedSymbols, builtin }) =>
    findUndefinedSymbols(node.params, contextStack, builtin),
}
