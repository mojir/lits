import { isA_SymbolToken } from '../tokenizer/algebraic/algebraicTokens'
import type { TokenStream } from '../tokenizer/interface'
import { isP_SymbolToken } from '../tokenizer/polish/polishTokens'

export function transformTokens(tokenStram: TokenStream, transformer: (name: string) => string): TokenStream {
  return {
    ...tokenStram,
    tokens: tokenStram.tokens.map(token => isA_SymbolToken(token) || isP_SymbolToken(token)
      ? [token[0], transformer(token[1])]
      : token),
  }
}
