import type { TokenStream } from '../tokenizer/interface'
import type { RegexpShorthandToken, Token } from '../tokenizer/Token'

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
  const tokenType = token[0]
  const value = token[1] as string
  switch (tokenType) {
    case 'Number':
      return value
    case 'Symbol':
      return value
    case 'String':
      return `"${value}"`
    case 'StringShorthand':
      return `:${value}`
    case 'LParen':
      return '('
    case 'RParen':
      return ')'
    case 'LBracket':
      return '['
    case 'RBracket':
      return ']'
    case 'LBrace':
      return '{'
    case 'RBrace':
      return '}'
    case 'CollectionAccessor':
      return value
    case 'FnShorthand':
      return value
    case 'RegexpShorthand': {
      const [, , options] = token as RegexpShorthandToken
      return `#"${value}"${options.g ? 'g' : ''}${options.i ? 'i' : ''}${options.m ? 'm' : ''}`
    }
    case 'NewLine':
      return ''
    case 'Comment':
      return `${value}\n`
    case 'Modifier':
      return value
    case 'ReservedSymbol':
      return value
    case 'Infix':
      return '$'
    case 'Postfix':
      return '@'
    case 'InfixOperator':
      return value

    default:
      throw new Error(`Unknown token type: ${tokenType satisfies never}`)
  }
}
