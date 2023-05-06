import { UserDefinedError } from '../../errors'
import { AstNode, SpecialExpressionNode } from '../../parser/interface'
import { string, token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

type ThrowNode = SpecialExpressionNode & {
  messageNode: AstNode
}

export const throwSpecialExpression: BuiltinSpecialExpression<null> = {
  parse: (tokens, position, { parseToken }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    const [newPosition, messageNode] = parseToken(tokens, position)
    position = newPosition

    token.assert(tokens[position], `EOF`, { type: `paren`, value: `)` })
    position += 1

    const node: ThrowNode = {
      type: `SpecialExpression`,
      name: `throw`,
      params: [],
      messageNode,
      token: firstToken.debugInfo ? firstToken : undefined,
    }
    return [position, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const message = string.as(evaluateAstNode((node as ThrowNode).messageNode, contextStack), node.token?.debugInfo, {
      nonEmpty: true,
    })
    throw new UserDefinedError(message, node.token?.debugInfo)
  },
  validateArity: () => undefined,
  findUndefinedSymbols: (node, contextStack, { findUndefinedSymbols, builtin }) =>
    findUndefinedSymbols((node as ThrowNode).messageNode, contextStack, builtin),
}
