import { LitsError } from '../../errors'
import { infixIdentifierCharacterClass, infixIdentifierFirstCharacterClass } from '../../identifier'
import {
  NO_MATCH,
  tokenizeCharacter,
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
  tokenizeString,
} from '../common/tokenizers'
import type { Tokenizer } from '../interface'
import { prefixInfixNamesRecord } from './reservedNames'

const identifierRegExp = new RegExp(infixIdentifierCharacterClass)
const identifierFirstCharacterRegExp = new RegExp(infixIdentifierFirstCharacterClass)
const whitespaceRegExp = /\s/

export const skipWhiteSpace: Tokenizer = (input, current) =>
  whitespaceRegExp.test(input[current] as string) ? [1, undefined] : NO_MATCH

export const tokenizeReservedName: Tokenizer = (input, position, debugData) => {
  for (const [reservedName, { forbidden }] of Object.entries(prefixInfixNamesRecord)) {
    const length = reservedName.length
    const nextChar = input[position + length]
    if (nextChar && identifierRegExp.test(nextChar))
      continue

    const name = input.substring(position, position + length)
    if (name === reservedName) {
      if (forbidden)
        throw new LitsError(`${name} is forbidden!`, debugData?.sourceCodeInfo)

      return [length, { t: 'ReservedName', v: reservedName, debugData }]
    }
  }
  return NO_MATCH
}

// tokenizePattern('Name', nameRegExp, input, position, debugData)
export const tokenizeName: Tokenizer = (input, position, debugData) => {
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

  return [position - initialPosition, { t: 'Name', v: value, debugData }]
}

export const tokenizeOperator: Tokenizer = (input, position, debugData) => {
  const twoChars = input.slice(position, position + 2)
  let nextChar = input[position + 2]
  if (['==', '!=', '>=', '<=', '&&', '||'].includes(twoChars) && [' ', '\n', undefined].includes(nextChar)) {
    return [2, { t: 'InfixOperator', v: twoChars, debugData }]
  }
  const oneChar = input[position] ?? ''
  nextChar = input[position + 1]
  if (['<', '>', '+', '-', '*', '/', '%', '^', '!', '=', '&', '|'].includes(oneChar) && [' ', '\n', undefined].includes(nextChar)) {
    return [1, { t: 'InfixOperator', v: oneChar, debugData }]
  }
  return NO_MATCH
}

export const tokenizePostfixDirective: Tokenizer = (input, position, debugData) =>
  tokenizeCharacter('Postfix', '@', input, position, debugData)

// All tokenizers, order matters!
export const tokenizers: Tokenizer[] = [
  skipWhiteSpace,
  tokenizeComment,
  tokenizePostfixDirective,
  tokenizeOperator,
  tokenizeLeftParen,
  tokenizeRightParen,
  tokenizeLeftBracket,
  tokenizeRightBracket,
  tokenizeLeftCurly,
  tokenizeRightCurly,
  tokenizeString,
  tokenizeNumber,
  tokenizeReservedName,
  tokenizeName,
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
