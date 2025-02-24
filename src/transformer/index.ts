import type { TokenStream } from '../tokenizer/interface'
import { isP_SymbolToken } from '../tokenizer/polish/polishTokens'

export function transformTokens(tokenStram: TokenStream, transformer: (name: string) => string): TokenStream {
  return {
    ...tokenStram,
    tokens: tokenStram.tokens.map(token => !isP_SymbolToken(token) ? token : [token[0], transformer(token[1])]),
  }
}
