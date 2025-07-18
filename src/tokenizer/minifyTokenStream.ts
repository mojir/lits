import type { TokenStream } from './tokenize'
import { isMultiLineCommentToken, isShebangToken, isSingleLineCommentToken, isWhitespaceToken } from './token'

export function minifyTokenStream(tokenStream: TokenStream, { removeWhiteSpace }: { removeWhiteSpace: boolean }): TokenStream {
  const tokens = tokenStream.tokens
    .filter((token) => {
      if (isSingleLineCommentToken(token)
        || isMultiLineCommentToken(token)
        || isShebangToken(token)
        || (removeWhiteSpace && isWhitespaceToken(token))) {
        return false
      }
      return true
    })

  return { ...tokenStream, tokens }
}
