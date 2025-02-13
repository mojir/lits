import { AstNodeType } from '../../constants/constants'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { asToken, isToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'

export interface CommentExpressionNode extends CommonSpecialExpressionNode<'comment'> {}

export const commentSpecialExpression: BuiltinSpecialExpression<null, CommentExpressionNode> = {
  parse: (tokenStream, parseState, firstToken, { parseToken }) => {
    const node: CommentExpressionNode = {
      t: AstNodeType.SpecialExpression,
      n: 'comment',
      p: [],
      debugData: undefined,
    }

    let tkn = asToken(tokenStream.tokens[parseState.position], tokenStream.filePath)
    while (!isToken(tkn, { type: 'Bracket', value: ')' })) {
      node.p.push(parseToken(tokenStream, parseState))
      tkn = asToken(tokenStream.tokens[parseState.position], tokenStream.filePath)
    }
    parseState.position += 1

    node.debugData = firstToken.debugData
      ? {
          token: firstToken,
          lastToken: tkn,
        }
      : undefined

    return node
  },
  evaluate: () => null,
  findUnresolvedIdentifiers: () => new Set(),
}
