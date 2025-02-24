import type { TokenStream } from '../tokenizer/interface'
import { type Token, assertSimpleToken, isValueToken } from '../tokenizer/tokens'

export function untokenize(tokenStream: TokenStream): string {
  return tokenStream.tokens.reduce((acc: string, token) => {
    return `${acc}${untokenizeToken(token)}`
  }, '')
}

function untokenizeToken(token: Token): string {
  if (isValueToken(token)) {
    return token[1]
  }
  assertSimpleToken(token)
  const tokenType = token[0]
  switch (tokenType) {
    case 'LParen': return '('
    case 'RParen': return ')'
    case 'LBracket': return '['
    case 'RBracket': return ']'
    case 'LBrace': return '{'
    case 'RBrace': return '}'
    case 'P_Algebraic': return '$'
    case 'A_Polish': return '@'
    case 'P_FnShorthand': return '#'

    /* v8 ignore next 2 */
    default:
      throw new Error(`Unknown token type: ${tokenType satisfies never}`)
  }
}
