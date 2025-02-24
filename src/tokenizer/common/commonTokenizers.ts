import { LitsError } from '../../errors'
import type { TokenDescriptor, Tokenizer } from '../interface'
import type { SimpleToken } from '../tokens'
import type { LBraceToken, LBracketToken, LParenToken, RBraceToken, RBracketToken, RParenToken, StringToken } from './commonTokens'

export const NO_MATCH: TokenDescriptor<never> = [0]

export function isNoMatch(tokenDescriptor: TokenDescriptor<any>): tokenDescriptor is TokenDescriptor<never> {
  return tokenDescriptor[0] === 0
}

export const tokenizeLeftParen: Tokenizer<LParenToken> = (input, position) =>
  tokenizeSimpleToken('LParen', '(', input, position)
export const tokenizeRightParen: Tokenizer<RParenToken> = (input, position) =>
  tokenizeSimpleToken('RParen', ')', input, position)
export const tokenizeLeftBracket: Tokenizer<LBracketToken> = (input, position) =>
  tokenizeSimpleToken('LBracket', '[', input, position)
export const tokenizeRightBracket: Tokenizer<RBracketToken> = (input, position) =>
  tokenizeSimpleToken('RBracket', ']', input, position)
export const tokenizeLeftCurly: Tokenizer<LBraceToken> = (input, position) =>
  tokenizeSimpleToken('LBrace', '{', input, position)
export const tokenizeRightCurly: Tokenizer<RBraceToken> = (input, position) =>
  tokenizeSimpleToken('RBrace', '}', input, position)

export const tokenizeString: Tokenizer<StringToken> = (input, position) => {
  if (input[position] !== '"')
    return NO_MATCH

  let value = '"'
  let length = 1
  let char = input[position + length]
  let escaping = false
  while (char !== '"' || escaping) {
    if (char === undefined)
      throw new LitsError(`Unclosed string at position ${position}.`)

    length += 1
    if (escaping) {
      escaping = false
      value += char
    }
    else {
      if (char === '\\') {
        escaping = true
      }
      value += char
    }
    char = input[position + length]
  }
  value += '"' // closing quote
  return [length + 1, ['String', value]]
}

export function tokenizeSimpleToken<T extends SimpleToken>(
  type: T[0],
  value: string,
  input: string,
  position: number,
): TokenDescriptor<T> {
  if (value === input[position])
    return [1, [type] as T]
  else
    return NO_MATCH
}
