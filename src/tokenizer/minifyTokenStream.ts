import type { TokenStream } from './interface'
import { isA_CommentToken, isA_MultiLineCommentToken, isA_WhitespaceToken } from './tokens'

export function minifyTokenStream(tokenStream: TokenStream, { removeWhiteSpace }: { removeWhiteSpace: boolean }): TokenStream {
  const tokens = tokenStream.tokens
    .filter((token) => {
      if (isA_CommentToken(token)
        || isA_MultiLineCommentToken(token)
        || (removeWhiteSpace && isA_WhitespaceToken(token))) {
        return false
      }
      return true
    })

  return { ...tokenStream, tokens }
}
