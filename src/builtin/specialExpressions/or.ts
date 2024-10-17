import { AstNodeType, TokenType } from '../../constants/constants'
import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { asToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'

export interface OrNode extends CommonSpecialExpressionNode<'or'> {}

export const orSpecialExpression: BuiltinSpecialExpression<Any, OrNode> = {
  parse: (tokenStream, position, firstToken, { parseTokensUntilClosingBracket }) => {
    const [newPosition, params] = parseTokensUntilClosingBracket(tokenStream, position)
    const lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' })

    const node: OrNode = {
      t: AstNodeType.SpecialExpression,
      n: 'or',
      p: params,
      debugData: firstToken.debugData && {
        token: firstToken,
        lastToken,
      },
    }

    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    let value: Any = false

    for (const param of node.p) {
      value = evaluateAstNode(param, contextStack)
      if (value)
        break
    }

    return value
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
