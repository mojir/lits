import { TokenType } from '../../constants/constants'
import { LitsError } from '../../errors'
import { postfixIdentifierCharacterClass } from '../../identifier'
import type { ModifierName } from '../../parser/interface'
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
import type { Token, TokenDebugData, TokenDescriptor, Tokenizer } from '../interface'
import { postfixReservedNamesRecord } from './reservedNames'

const nameRegExp = new RegExp(postfixIdentifierCharacterClass)
const whitespaceRegExp = /\s|,/

export const skipWhiteSpace: Tokenizer = (input, current) =>
  whitespaceRegExp.test(input[current] as string) ? [1, undefined] : NO_MATCH

export const tokenizeFnShorthand: Tokenizer = (input, position, debugData) => {
  if (input.slice(position, position + 2) !== '#(')
    return NO_MATCH

  return [
    1,
    {
      t: TokenType.FnShorthand,
      v: '#',
      debugData,
    },
  ]
}

export const tokenizeReservedName: Tokenizer = (input, position, debugData) => {
  for (const [reservedName, { forbidden }] of Object.entries(postfixReservedNamesRecord)) {
    const length = reservedName.length
    const nextChar = input[position + length]
    if (nextChar && nameRegExp.test(nextChar))
      continue

    const name = input.substring(position, position + length)
    if (name === reservedName) {
      if (forbidden)
        throw new LitsError(`${name} is forbidden!`, debugData?.sourceCodeInfo)

      return [length, { t: TokenType.ReservedName, v: reservedName, debugData }]
    }
  }
  return NO_MATCH
}

export const tokenizeName: Tokenizer = (input, position, debugData) =>
  tokenizePattern(TokenType.Name, nameRegExp, input, position, debugData)

const tokenizeSymbolString: Tokenizer = (input, position, debugData) => {
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

  return [length, { t: TokenType.String, v: value, debugData, o: { s: true } } satisfies Token]
}

export const tokenizeModifier: Tokenizer = (input, position, debugData) => {
  const modifiers: ModifierName[] = ['&', '&let', '&when', '&while']
  for (const modifier of modifiers) {
    const length = modifier.length
    const charAfterModifier = input[position + length]
    if (input.substring(position, position + length) === modifier && (!charAfterModifier || !nameRegExp.test(charAfterModifier))) {
      const value: ModifierName = modifier
      return [length, { t: TokenType.Modifier, v: value, debugData }]
    }
  }
  return NO_MATCH
}

export const tokenizeInfixDirective: Tokenizer = (input, position, debugData) => {
  if (input[position] !== '$') {
    return NO_MATCH
  }
  const nextChar = input[position + 1]
  if (nextChar && nameRegExp.test(nextChar)) {
    return NO_MATCH
  }
  return [1, { t: TokenType.Infix, v: '$', debugData }]
}

function tokenizePattern(
  type: TokenType,
  pattern: RegExp,
  input: string,
  position: number,
  debugData?: TokenDebugData,
): TokenDescriptor {
  let char = input[position]
  let length = 0
  let value = ''

  if (!char || !pattern.test(char))
    return NO_MATCH

  while (char && pattern.test(char)) {
    value += char
    length += 1
    char = input[position + length]
  }

  return [length, { t: type, v: value, debugData }]
}

// All tokenizers, order matters!
export const tokenizers: Tokenizer[] = [
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
  tokenizeSymbolString,
  tokenizeNumber,
  tokenizeReservedName,
  tokenizeName,
  tokenizeModifier,
  tokenizeRegexpShorthand,
  tokenizeFnShorthand,
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
