import type { TokenStream } from '../tokenizer/interface'
import { isPF_SymbolToken } from '../tokenizer/postfix/postfixTokens'

export function transformTokens(tokenStram: TokenStream, transformer: (name: string) => string): TokenStream {
  return {
    ...tokenStram,
    tokens: tokenStram.tokens.map(token => !isPF_SymbolToken(token) ? token : [token[0], transformer(token[1])]),
  }
}
