import { LitsError } from '../../errors'
import type { TokenDescriptor, Tokenizer } from '../interface'
import type { SimpleToken } from '../tokens'
import type { LBraceToken, LBracketToken, LParenToken, RBraceToken, RBracketToken, RParenToken, RegexpShorthandToken, StringToken } from './commonTokens'

export const NO_MATCH: TokenDescriptor<never> = [0]

export function isNoMatch(tokenDescriptor: TokenDescriptor<any>): tokenDescriptor is TokenDescriptor<never> {
  return tokenDescriptor[0] === 0
}

const tokenizeLParen: Tokenizer<LParenToken> = (input, position) =>
  tokenizeSimpleToken('LParen', '(', input, position)
const tokenizeRParen: Tokenizer<RParenToken> = (input, position) =>
  tokenizeSimpleToken('RParen', ')', input, position)
const tokenizeLBracket: Tokenizer<LBracketToken> = (input, position) =>
  tokenizeSimpleToken('LBracket', '[', input, position)
const tokenizeRBracket: Tokenizer<RBracketToken> = (input, position) =>
  tokenizeSimpleToken('RBracket', ']', input, position)
const tokenizeLBrace: Tokenizer<LBraceToken> = (input, position) =>
  tokenizeSimpleToken('LBrace', '{', input, position)
const tokenizeRBrace: Tokenizer<RBraceToken> = (input, position) =>
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
      throw new LitsError(`Unclosed string at position ${position}.`, undefined)

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

export const tokenizeRegexpShorthand: Tokenizer<RegexpShorthandToken> = (input, position) => {
  if (input[position] !== '#')
    return NO_MATCH

  const [stringLength, token] = tokenizeString(input, position + 1)
  if (!token)
    return NO_MATCH

  position += stringLength + 1
  let length = stringLength + 1

  let options = ''
  while (input[position] === 'g' || input[position] === 'i') {
    if (options.includes(input[position]!)) {
      throw new LitsError(`Duplicated regexp option "${input[position]}" at position ${position}.`, undefined)
    }
    options += input[position]!
    length += 1
    position += 1
  }

  return [length, ['RegexpShorthand', `#${token[1]}${options}`]]
}

export function tokenizeSimpleToken<T extends SimpleToken>(
  type: T[0],
  value: string,
  input: string,
  position: number,
): TokenDescriptor<T> {
  if (value === input.slice(position, position + value.length))
    return [value.length, [type] as T]
  else
    return NO_MATCH
}

export const commonTokenizers = [
  tokenizeLParen,
  tokenizeRParen,
  tokenizeLBracket,
  tokenizeRBracket,
  tokenizeLBrace,
  tokenizeRBrace,
  tokenizeString,
  tokenizeRegexpShorthand,
] as const
