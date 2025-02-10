import type { TokenStream } from '../tokenizer/interface'

export function transformTokens(tokenStram: TokenStream, transformer: (name: string) => string): TokenStream {
  return {
    ...tokenStram,
    tokens: tokenStram.tokens.map((token) => {
      return {
        ...token,
        v: transformer(token.v),
      }
    }),

  }
}
