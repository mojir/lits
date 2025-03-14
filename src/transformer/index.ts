import type { TokenStream } from '../tokenizer/tokenize'
import { isSymbolToken } from '../tokenizer/token'

export function transformSymbolTokens(tokenStram: TokenStream, transformer: (symbol: string) => string): TokenStream {
  return {
    ...tokenStram,
    tokens: tokenStram.tokens.map(token => isSymbolToken(token)
      ? [token[0], transformer(token[1])]
      : token),
  }
}
