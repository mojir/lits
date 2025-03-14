import type { TokenStream } from '../tokenizer/tokenize'

export function untokenize(tokenStream: TokenStream): string {
  return tokenStream.tokens.reduce((acc: string, token) => {
    return `${acc}${token[1]}`
  }, '')
}
