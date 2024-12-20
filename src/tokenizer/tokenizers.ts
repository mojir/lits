import { TokenType } from '../constants/constants'
import { LitsError } from '../errors'
import { identifierCharacterClass } from '../identifier'
import type { ModifierName } from '../parser/interface'
import { reservedNamesRecord } from '../reservedNames'
import { asString } from '../typeGuards/string'
import type { Token, TokenDebugData, TokenDescriptor, Tokenizer } from './interface'

const NO_MATCH: TokenDescriptor = [0, undefined]

const nameRegExp = new RegExp(identifierCharacterClass)
const whitespaceRegExp = /\s|,/
const newLineRegExp = /\n/

export const tokenizeNewLine: Tokenizer = (input, current, debugData) =>
  newLineRegExp.test(input[current] as string)
    ? [1, { t: TokenType.NewLine, v: '\n', debugData }]
    : NO_MATCH

export const tokenizeComment: Tokenizer = (input, current, debugData) => {
  if (input[current] === ';') {
    let length = 0
    let value = ''
    while (input[current + length] !== '\n' && current + length < input.length) {
      value += input[current + length]
      length += 1
    }

    if (input[current + length] === '\n' && current + length < input.length)
      length += 1

    return [length, { t: TokenType.Comment, v: value.trim(), debugData }]
  }
  return NO_MATCH
}

export const skipWhiteSpace: Tokenizer = (input, current) =>
  whitespaceRegExp.test(input[current] as string) ? [1, undefined] : NO_MATCH

export const tokenizeLeftParen: Tokenizer = (input, position, debugData) =>
  tokenizeCharacter(TokenType.Bracket, '(', input, position, debugData)
export const tokenizeRightParen: Tokenizer = (input, position, debugData) =>
  tokenizeCharacter(TokenType.Bracket, ')', input, position, debugData)
export const tokenizeLeftBracket: Tokenizer = (input, position, debugData) =>
  tokenizeCharacter(TokenType.Bracket, '[', input, position, debugData)
export const tokenizeRightBracket: Tokenizer = (input, position, debugData) =>
  tokenizeCharacter(TokenType.Bracket, ']', input, position, debugData)
export const tokenizeLeftCurly: Tokenizer = (input, position, debugData) =>
  tokenizeCharacter(TokenType.Bracket, '{', input, position, debugData)
export const tokenizeRightCurly: Tokenizer = (input, position, debugData) =>
  tokenizeCharacter(TokenType.Bracket, '}', input, position, debugData)

export const tokenizeString: Tokenizer = (input, position, debugData) => {
  if (input[position] !== '"')
    return NO_MATCH

  let value = ''
  let length = 1
  let char = input[position + length]
  let escape = false
  while (char !== '"' || escape) {
    if (char === undefined)
      throw new LitsError(`Unclosed string at position ${position}.`, debugData?.sourceCodeInfo)

    length += 1
    if (escape) {
      escape = false
      if (char === '"' || char === '\\') {
        value += char
      }
      else {
        value += '\\'
        value += char
      }
    }
    else {
      if (char === '\\')
        escape = true
      else
        value += char
    }
    char = input[position + length]
  }
  return [length + 1, { t: TokenType.String, v: value, debugData }]
}

export const tokenizeCollectionAccessor: Tokenizer = (input, position, debugData) => {
  const char = input[position]
  if (char !== '.' && char !== '#')
    return NO_MATCH

  return [
    1,
    {
      t: TokenType.CollectionAccessor,
      v: char,
      debugData,
    },
  ]
}

export const tokenizeSymbolString: Tokenizer = (input, position, debugData) => {
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

export const tokenizeRegexpShorthand: Tokenizer = (input, position, debugData) => {
  if (input[position] !== '#')
    return NO_MATCH

  const [stringLength, token] = tokenizeString(input, position + 1, debugData)
  if (!token)
    return NO_MATCH

  position += stringLength + 1
  let length = stringLength + 1

  const options: Record<string, boolean> = {}
  while (input[position] === 'g' || input[position] === 'i') {
    if (input[position] === 'g') {
      if (options.g)
        throw new LitsError(`Duplicated regexp option "${input[position]}" at position ${position}.`, debugData?.sourceCodeInfo)

      length += 1
      options.g = true
    }
    else {
      if (options.i)
        throw new LitsError(`Duplicated regexp option "${input[position]}" at position ${position}.`, debugData?.sourceCodeInfo)

      length += 1
      options.i = true
    }
    position += 1
  }

  if (nameRegExp.test(input[position] ?? ''))
    throw new LitsError(`Unexpected regexp option "${input[position]}" at position ${position}.`, debugData?.sourceCodeInfo)

  return [
    length,
    {
      t: TokenType.RegexpShorthand,
      v: token.v,
      o: options,
      debugData,
    },
  ]
}

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

const endOfNumberRegExp = /[\s)\]},#]/
const decimalNumberRegExp = /\d/
const octalNumberRegExp = /[0-7]/
const hexNumberRegExp = /[0-9a-f]/i
const binaryNumberRegExp = /[01]/
const firstCharRegExp = /[0-9.-]/
export const tokenizeNumber: Tokenizer = (input, position, debugData) => {
  let type: 'decimal' | 'octal' | 'hex' | 'binary' = 'decimal'
  const firstChar = input[position] as string
  if (!firstCharRegExp.test(firstChar))
    return NO_MATCH

  let hasDecimals = firstChar === '.'

  let i: number
  for (i = position + 1; i < input.length; i += 1) {
    const char = asString(input[i], debugData?.sourceCodeInfo, { char: true })
    if (endOfNumberRegExp.test(char))
      break

    if (char === '.') {
      const nextChar = input[i + 1]
      if (typeof nextChar === 'string' && !decimalNumberRegExp.test(nextChar))
        break
    }
    if (i === position + 1 && firstChar === '0') {
      if (char === 'b' || char === 'B') {
        type = 'binary'
        continue
      }
      if (char === 'o' || char === 'O') {
        type = 'octal'
        continue
      }
      if (char === 'x' || char === 'X') {
        type = 'hex'
        continue
      }
    }
    if (type === 'decimal' && hasDecimals) {
      if (!decimalNumberRegExp.test(char))
        return NO_MATCH
    }
    else if (type === 'binary') {
      if (!binaryNumberRegExp.test(char))
        return NO_MATCH
    }
    else if (type === 'octal') {
      if (!octalNumberRegExp.test(char))
        return NO_MATCH
    }
    else if (type === 'hex') {
      if (!hexNumberRegExp.test(char))
        return NO_MATCH
    }
    else {
      if (char === '.') {
        hasDecimals = true
        continue
      }
      if (!decimalNumberRegExp.test(char))
        return NO_MATCH
    }
  }

  const length = i - position
  const value = input.substring(position, i)
  if ((type !== 'decimal' && length <= 2) || value === '.' || value === '-')
    return NO_MATCH

  return [length, { t: TokenType.Number, v: value, debugData }]
}

export const tokenizeReservedName: Tokenizer = (input, position, debugData) => {
  for (const [reservedName, { forbidden }] of Object.entries(reservedNamesRecord)) {
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

function tokenizeCharacter(
  type: TokenType,
  value: string,
  input: string,
  position: number,
  debugData?: TokenDebugData,
): TokenDescriptor {
  if (value === input[position])
    return [1, { t: type, v: value, debugData }]
  else
    return NO_MATCH
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
