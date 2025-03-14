import type { TokenStream } from './interface'
import { isA_CommentToken, isMultiLineCommentToken, isWhitespaceToken } from './tokens'

export function minifyTokenStream(tokenStream: TokenStream, { removeWhiteSpace }: { removeWhiteSpace: boolean }): TokenStream {
  const tokens = tokenStream.tokens
    .filter((token) => {
      if (isA_CommentToken(token)
        || isMultiLineCommentToken(token)
        || (removeWhiteSpace && isWhitespaceToken(token))) {
        return false
      }
      return true
    })

  return { ...tokenStream, tokens }
}
