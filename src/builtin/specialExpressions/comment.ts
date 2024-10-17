import { AstNodeType, TokenType } from '../../constants/constants'
import type { AstNode, CommonSpecialExpressionNode } from '../../parser/interface'
import { asToken, isToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'

export interface CommentExpressionNode extends CommonSpecialExpressionNode<'comment'> {}

export const commentSpecialExpression: BuiltinSpecialExpression<null, CommentExpressionNode> = {
  parse: (tokenStream, position, firstToken, { parseToken }) => {
    const node: CommentExpressionNode = {
      t: AstNodeType.SpecialExpression,
      n: 'comment',
      p: [],
      debugData: undefined,
    } satisfies CommentExpressionNode

    let tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
    while (!isToken(tkn, { type: TokenType.Bracket, value: ')' })) {
      let bodyNode: AstNode
      ;[position, bodyNode] = parseToken(tokenStream, position)
      node.p.push(bodyNode)
      tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
    }

    node.debugData = firstToken.debugData
      ? {
          token: firstToken,
          lastToken: tkn,
        }
      : undefined

    return [position + 1, node]
  },
  evaluate: () => null,
  findUnresolvedIdentifiers: () => new Set(),
}
