import { LitsError } from '../../errors'
import type { TokenDescriptor, Tokenizer } from '../interface'
import type { SimpleToken } from '../tokens'
import type { AlgebraicNotationToken, EndNotationToken, LBraceToken, LBracketToken, LParenToken, PolishNotationToken, RBraceToken, RBracketToken, RParenToken, StringToken } from './commonTokens'

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
const tokenizePolishNotation: Tokenizer<PolishNotationToken> = (input, position) =>
  tokenizeSimpleToken('PolNotation', '$`', input, position)
const tokenizeAlgebraicNotation: Tokenizer<AlgebraicNotationToken> = (input, position) =>
  tokenizeSimpleToken('AlgNotation', '@`', input, position)
const tokenizeEndNotation: Tokenizer<EndNotationToken> = (input, position) =>
  tokenizeSimpleToken('EndNotation', '`', input, position)

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
  if (value === input.slice(position, position + value.length))
    return [value.length, [type] as T]
  else
    return NO_MATCH
}

export const commonTokenizers = [
  tokenizePolishNotation,
  tokenizeAlgebraicNotation,
  tokenizeEndNotation,
  tokenizeLParen,
  tokenizeRParen,
  tokenizeLBracket,
  tokenizeRBracket,
  tokenizeLBrace,
  tokenizeRBrace,
  tokenizeString,
] as const
