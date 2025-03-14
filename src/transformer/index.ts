import type { TokenStream } from '../tokenizer/tokenize'
import { isSymbolToken } from '../tokenizer/token'

export function transformTokens(tokenStram: TokenStream, transformer: (name: string) => string): TokenStream {
  return {
    ...tokenStram,
    tokens: tokenStram.tokens.map(token => isSymbolToken(token)
      ? [token[0], transformer(token[1])]
      : token),
  }
}
