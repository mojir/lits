import { isA_CommentToken, isA_MultiLineCommentToken, isA_WhitespaceToken } from './algebraic/algebraicTokens'
import type { TokenStream } from './interface'
import { isP_CommentToken, isP_WhitespaceToken } from './polish/polishTokens'

export function minifyTokenStream(tokenStream: TokenStream): TokenStream {
  const tokens = tokenStream.tokens.filter((token) => {
    if (
      isP_CommentToken(token)
      || isA_CommentToken(token)
      || isA_MultiLineCommentToken(token)
      || isA_WhitespaceToken(token)
      || isP_WhitespaceToken(token)) {
      return false
    }
    return true
  })

  return { ...tokenStream, tokens }
}
