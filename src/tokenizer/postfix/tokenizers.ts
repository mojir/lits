import { LitsError } from '../../errors'
import { postfixIdentifierCharacterClass } from '../../identifier'
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
  tokenizeString,
} from '../common/tokenizers'
import type { Tokenizer } from '../interface'
import type { FnShorthandToken, InfixToken, ModifierName, ModifierToken, ReservedSymbolToken, StringShorthandToken, SymbolToken } from '../Token'
import { modifierNames } from '../Token'
import { postfixReservedNamesRecord } from './reservedNames'

const nameRegExp = new RegExp(postfixIdentifierCharacterClass)
const whitespaceRegExp = /\s|,/

export const skipWhiteSpace: Tokenizer<never> = (input, current) =>
  whitespaceRegExp.test(input[current] as string) ? [1, undefined] : NO_MATCH

export const tokenizeFnShorthand: Tokenizer<FnShorthandToken> = (input, position) => {
  if (input.slice(position, position + 2) !== '#(')
    return NO_MATCH

  return [1, ['FnShorthand']]
}

export const tokenizeReservedSymbol: Tokenizer<ReservedSymbolToken> = (input, position) => {
  for (const [reservedName, { forbidden }] of Object.entries(postfixReservedNamesRecord)) {
    const length = reservedName.length
    const nextChar = input[position + length]
    if (nextChar && nameRegExp.test(nextChar)) {
      continue
    }

    const symbol = input.substring(position, position + length)
    if (symbol === reservedName) {
      if (forbidden)
        throw new LitsError(`${symbol} is forbidden!`)

      return [length, ['ReservedSymbol', reservedName]]
    }
  }
  return NO_MATCH
}

export const tokenizeSymbol: Tokenizer<SymbolToken> = (input, position) => {
  let char = input[position]
  let length = 0
  let value = ''

  if (!char || !nameRegExp.test(char))
    return NO_MATCH

  while (char && nameRegExp.test(char)) {
    value += char
    length += 1
    char = input[position + length]
  }

  return [length, ['Symbol', value]]
}

const tokenizeStringShorthand: Tokenizer<StringShorthandToken> = (input, position) => {
  if (input[position] !== ':')
    return NO_MATCH

  let value = ''
  let length = 1
  let char = input[position + length]
  while (char && nameRegExp.test(char)) {
    length += 1
    value += char
    char = input[position + length]
  }
  if (length === 1)
    return NO_MATCH

  return [length, ['StringShorthand', value]]
}

export const tokenizeModifier: Tokenizer<ModifierToken> = (input, position) => {
  for (const modifierName of modifierNames) {
    const length = modifierName.length
    const charAfterModifier = input[position + length]
    if (input.substring(position, position + length) === modifierName && (!charAfterModifier || !nameRegExp.test(charAfterModifier))) {
      const value: ModifierName = modifierName
      return [length, ['Modifier', value]]
    }
  }
  return NO_MATCH
}

export const tokenizeInfixDirective: Tokenizer<InfixToken> = (input, position) => {
  if (input[position] !== '$') {
    return NO_MATCH
  }
  const nextChar = input[position + 1]
  if (nextChar && nameRegExp.test(nextChar)) {
    return NO_MATCH
  }
  return [1, ['Infix']]
}

// All tokenizers, order matters!
export const postfixTokenizers = [
  tokenizeInfixDirective,
  skipWhiteSpace,
  tokenizeComment,
  tokenizeLeftParen,
  tokenizeRightParen,
  tokenizeLeftBracket,
  tokenizeRightBracket,
  tokenizeLeftCurly,
  tokenizeRightCurly,
  tokenizeString,
  tokenizeStringShorthand,
  tokenizeNumber,
  tokenizeReservedSymbol,
  tokenizeSymbol,
  tokenizeModifier,
  tokenizeRegexpShorthand,
  tokenizeFnShorthand,
  tokenizeCollectionAccessor,
] as const

export const newLineTokenizers = [
  tokenizeNewLine,
  skipWhiteSpace,
]

export const commentTokenizers = [
  tokenizeNewLine,
  skipWhiteSpace,
  tokenizeComment,
]
