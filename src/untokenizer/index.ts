import type { TokenStream } from '../tokenizer/interface'
import { type Token, assertSimpleToken, isValueToken } from '../tokenizer/Token'

function isNoSpaceNeededBefore(token: Token): boolean {
  switch (token[0]) {
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
  switch (token[0]) {
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
    case 'NewLine': return ''
    case 'Infix': return '$'
    case 'Postfix': return '@'
    case 'FnShorthand': return '#'

    default:
      throw new Error(`Unknown token type: ${tokenType satisfies never}`)
  }
}
