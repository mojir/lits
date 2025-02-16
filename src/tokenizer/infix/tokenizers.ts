import { LitsError } from '../../errors'
import { infixIdentifierCharacterClass, infixIdentifierFirstCharacterClass } from '../../identifier'
import {
  NO_MATCH,
  tokenizeCollectionAccessor,
  tokenizeComment,
  tokenizeLeftBracket,
  tokenizeLeftCurly,
  tokenizeLeftParen,
  tokenizeNewLine,
  tokenizeNumber,
  tokenizeRegexpShorthand,
  tokenizeRightBracket,
  tokenizeRightCurly,
  tokenizeRightParen,
  tokenizeSimpleToken,
  tokenizeString,
} from '../common/tokenizers'
import type { Tokenizer } from '../interface'
import type { InfixOperatorToken, PostfixToken, ReservedSymbolToken, SymbolToken, Token } from '../Token'
import { prefixInfixNamesRecord } from './reservedNames'

const identifierRegExp = new RegExp(infixIdentifierCharacterClass)
const identifierFirstCharacterRegExp = new RegExp(infixIdentifierFirstCharacterClass)
const whitespaceRegExp = /\s/

export const skipWhiteSpace: Tokenizer<never> = (input, current) =>
  whitespaceRegExp.test(input[current] as string) ? [1, undefined] : NO_MATCH

export const tokenizeReservedSymbol: Tokenizer<ReservedSymbolToken> = (input, position) => {
  for (const [reservedName, { forbidden }] of Object.entries(prefixInfixNamesRecord)) {
    const length = reservedName.length
    const nextChar = input[position + length]
    if (nextChar && identifierRegExp.test(nextChar))
      continue

    const name = input.substring(position, position + length)
    if (name === reservedName) {
      if (forbidden)
        throw new LitsError(`${name} is forbidden!`)

      return [length, ['ReservedSymbol', reservedName]]
    }
  }
  return NO_MATCH
}

export const tokenizeSymbol: Tokenizer<SymbolToken> = (input, position) => {
  const initialPosition = position
  let value = input[position]

  if (!value || !identifierFirstCharacterRegExp.test(value))
    return NO_MATCH

  position += 1
  let char = input[position]

  while (char && identifierRegExp.test(char)) {
    value += char
    position += 1
    char = input[position]
  }

  return [position - initialPosition, ['Symbol', value]]
}

export const tokenizeInfixOperator: Tokenizer<InfixOperatorToken> = (input, position) => {
  const twoChars = input.slice(position, position + 2)
  let nextChar = input[position + 2]
  if (['==', '!=', '>=', '<=', '&&', '||'].includes(twoChars) && [' ', '\n', undefined].includes(nextChar)) {
    return [2, ['InfixOperator', twoChars as InfixOperatorToken[1]]]
  }
  const oneChar = input[position] ?? ''
  nextChar = input[position + 1]
  if (['<', '>', '+', '-', '*', '/', '%', '^', '!', '=', '&', '|'].includes(oneChar) && [' ', '\n', undefined].includes(nextChar)) {
    return [1, ['InfixOperator', oneChar as InfixOperatorToken[1]]]
  }
  return NO_MATCH
}

export const tokenizePostfixDirective: Tokenizer<PostfixToken> = (input, position) =>
  tokenizeSimpleToken('Postfix', '@', input, position)

// All tokenizers, order matters!
export const tokenizers: Tokenizer<Token>[] = [
  skipWhiteSpace,
  tokenizeComment,
  tokenizePostfixDirective,
  tokenizeInfixOperator,
  tokenizeLeftParen,
  tokenizeRightParen,
  tokenizeLeftBracket,
  tokenizeRightBracket,
  tokenizeLeftCurly,
  tokenizeRightCurly,
  tokenizeString,
  tokenizeNumber,
  tokenizeReservedSymbol,
  tokenizeSymbol,
  tokenizeRegexpShorthand,
  tokenizeCollectionAccessor,
]

export const newLineTokenizers = [
  tokenizeNewLine,
  skipWhiteSpace,
]

export const commentTokenizers = [
  tokenizeNewLine,
  skipWhiteSpace,
  tokenizeComment,
]
