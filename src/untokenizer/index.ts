import type { Token, TokenStream } from '../tokenizer/interface'

function isNoSpaceNeededBefore(token: Token): boolean {
  switch (token.t) {
    case 'RParen':
    case 'RBracket':
    case 'CollectionAccessor':
    case 'NewLine':
      return true
    default:
      return false
  }
}

function isNoSpaceNeededAfter(token: Token): boolean {
  switch (token.t) {
    case 'LParen':
    case 'LBracket':
    case 'CollectionAccessor':
    case 'FnShorthand':
    case 'NewLine':
    case 'RegexpShorthand':
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
    case 'String':
      return `"${token.v}"`
    default:
      return token.v
  }
}
