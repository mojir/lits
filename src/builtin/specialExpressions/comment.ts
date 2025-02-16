import { AstNodeType } from '../../constants/constants'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { asToken, getTokenDebugData, isRParenToken } from '../../tokenizer/Token'
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

    let tkn = asToken(tokenStream.tokens[parseState.position])
    while (!isRParenToken(tkn)) {
      node.p.push(parseToken(tokenStream, parseState))
      tkn = asToken(tokenStream.tokens[parseState.position])
    }
    parseState.position += 1

    node.debugData = getTokenDebugData(firstToken) && {
      token: firstToken,
      lastToken: tkn,
    }

    return node
  },
  evaluate: () => null,
  findUnresolvedIdentifiers: () => new Set(),
}
