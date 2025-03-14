import type { TokenStream } from '../tokenizer/interface'
import { isSymbolToken } from '../tokenizer/tokens'

export function transformTokens(tokenStram: TokenStream, transformer: (name: string) => string): TokenStream {
  return {
    ...tokenStram,
    tokens: tokenStram.tokens.map(token => isSymbolToken(token)
      ? [token[0], transformer(token[1])]
      : token),
  }
}
