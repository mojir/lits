import { TokenType } from '../constants/constants'
import type { Token, TokenStream } from '../tokenizer/interface'

function isNoSpaceNeededBefore(token: Token): boolean {
  switch (token.t) {
    case TokenType.Bracket:
      return [')', ']'].includes(token.v)
    case TokenType.CollectionAccessor:
      return true
    case TokenType.NewLine:
      return true
    default:
      return false
  }
}

function isNoSpaceNeededAfter(token: Token): boolean {
  switch (token.t) {
    case TokenType.Bracket:
      return ['(', '['].includes(token.v)
    case TokenType.CollectionAccessor:
      return true
    case TokenType.FnShorthand:
      return true
    case TokenType.NewLine:
      return true
    case TokenType.RegexpShorthand:
      return true
    default:
      return false
  }
}

export function untokenize(tokenStream: TokenStream): string {
  let lastToken: Token | undefined
  return tokenStream.tokens.reduce((acc: string, token) => {
    const joiner = !lastToken || isNoSpaceNeededAfter(lastToken) || isNoSpaceNeededBefore(token) ? '' : ' '
    lastToken = token
    return `${acc}${joiner}${untokenizeToken(token)}`
  }, '')
}

function untokenizeToken(token: Token): string {
  switch (token.t) {
    case TokenType.String:
      return `"${token.v}"`
    default:
      return token.v
  }
}
