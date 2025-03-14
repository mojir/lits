import type { TokenStream } from '../tokenizer/interface'
import { isA_SymbolToken } from '../tokenizer/tokens'

export function transformTokens(tokenStram: TokenStream, transformer: (name: string) => string): TokenStream {
  return {
    ...tokenStram,
    tokens: tokenStram.tokens.map(token => isA_SymbolToken(token)
      ? [token[0], transformer(token[1])]
      : token),
  }
}
