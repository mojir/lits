import { LitsError } from '../errors'
import { ModifierName } from '../parser/interface'
import { reservedNamesRecord } from '../reservedNames'
import { asNotUndefined } from '../utils'
import { TokenDescriptor, Tokenizer, TokenizerType, SourceCodeInfo } from './interface'

const NO_MATCH: TokenDescriptor = [0, undefined]

// A name (function or variable) can contain a lot of different characters

const nameRegExp =
  /[@%0-9a-zA-ZàáâãăäāåæćčçèéêĕëēìíîĭïðłñòóôõöőøšùúûüűýÿþÀÁÂÃĂÄĀÅÆĆČÇÈÉÊĔËĒÌÍÎĬÏÐŁÑÒÓÔÕÖŐØŠÙÚÛÜŰÝÞß_^?=!$%<>.+*/-]/
const whitespaceRegExp = /\s|,/

export const skipWhiteSpace: Tokenizer = (input, current) =>
  whitespaceRegExp.test(input[current] as string) ? [1, undefined] : NO_MATCH

export const skipComment: Tokenizer = (input, current) => {
  if (input[current] === `;`) {
    let length = 1
    while (input[current + length] !== `\n` && current + length < input.length) {
      length += 1
    }
    if (input[current + length] === `\n` && current + length < input.length) {
      length += 1
    }
    return [length, undefined]
  }
  return NO_MATCH
}

export const tokenizeLeftParen: Tokenizer = (input, position, sourceCodeInfo) =>
  tokenizeCharacter(`paren`, `(`, input, position, sourceCodeInfo)
export const tokenizeRightParen: Tokenizer = (input, position, sourceCodeInfo) =>
  tokenizeCharacter(`paren`, `)`, input, position, sourceCodeInfo)
export const tokenizeLeftBracket: Tokenizer = (input, position, sourceCodeInfo) =>
  tokenizeCharacter(`paren`, `[`, input, position, sourceCodeInfo)
export const tokenizeRightBracket: Tokenizer = (input, position, sourceCodeInfo) =>
  tokenizeCharacter(`paren`, `]`, input, position, sourceCodeInfo)
export const tokenizeLeftCurly: Tokenizer = (input, position, sourceCodeInfo) =>
  tokenizeCharacter(`paren`, `{`, input, position, sourceCodeInfo)
export const tokenizeRightCurly: Tokenizer = (input, position, sourceCodeInfo) =>
  tokenizeCharacter(`paren`, `}`, input, position, sourceCodeInfo)

export const tokenizeString: Tokenizer = (input, position, sourceCodeInfo) => {
  if (input[position] !== `'`) {
    return NO_MATCH
  }

  let value = ``
  let length = 1
  let char = input[position + length]
  let escape = false
  while (char !== `'` || escape) {
    if (char === undefined) {
      throw new LitsError(`Unclosed string at position ${position}`, sourceCodeInfo)
    }
    length += 1
    if (escape) {
      escape = false
      if (char === `'` || char === `\\`) {
        value += char
      } else {
        value += `\\`
        value += char
      }
    } else {
      if (char === `\\`) {
        escape = true
      } else {
        value += char
      }
    }
    char = input[position + length]
  }
  return [length + 1, { type: `string`, value, sourceCodeInfo }]
}

export const tokenizeSymbolString: Tokenizer = (input, position, sourceCodeInfo) => {
  if (input[position] !== `:`) {
    return NO_MATCH
  }

  let value = ``
  let length = 1
  let char = input[position + length]
  while (char && nameRegExp.test(char)) {
    length += 1
    value += char
    char = input[position + length]
  }
  if (length === 1) {
    return NO_MATCH
  }
  return [length, { type: `string`, value, sourceCodeInfo }]
}

export const tokenizeRegexpShorthand: Tokenizer = (input, position, sourceCodeInfo) => {
  if (input[position] !== `#`) {
    return NO_MATCH
  }
  const [stringLength, token] = tokenizeString(input, position + 1, sourceCodeInfo)
  if (!token) {
    return NO_MATCH
  }
  position += stringLength + 1
  let length = stringLength + 1

  const options: Record<string, boolean> = {}
  while (input[position] === `g` || input[position] === `i`) {
    if (input[position] === `g`) {
      if (options.g) {
        throw new LitsError(`Duplicated regexp option "${input[position]}" at position ${position}`, sourceCodeInfo)
      }
      length += 1
      options.g = true
    } else {
      if (options.i) {
        throw new LitsError(`Duplicated regexp option "${input[position]}" at position ${position}`, sourceCodeInfo)
      }
      length += 1
      options.i = true
    }
    position += 1
  }

  if (nameRegExp.test(input[position] ?? ``)) {
    throw new LitsError(`Unexpected regexp option "${input[position]}" at position ${position}`, sourceCodeInfo)
  }

  return [
    length,
    {
      type: `regexpShorthand`,
      value: token.value,
      options,
      sourceCodeInfo,
    },
  ]
}

export const tokenizeFnShorthand: Tokenizer = (input, position, sourceCodeInfo) => {
  if (input.slice(position, position + 2) !== `#(`) {
    return NO_MATCH
  }
  return [
    1,
    {
      type: `fnShorthand`,
      value: `#`,
      sourceCodeInfo,
    },
  ]
}

const endOfNumberRegExp = /\s|[)\]},]/
const decimalNumberRegExp = /[0-9]/
const octalNumberRegExp = /[0-7]/
const hexNumberRegExp = /[0-9a-fA-F]/
const binaryNumberRegExp = /[0-1]/
const firstCharRegExp = /[0-9.-]/
export const tokenizeNumber: Tokenizer = (input, position, sourceCodeInfo) => {
  let type: `decimal` | `octal` | `hex` | `binary` = `decimal`
  const firstChar = input[position] as string
  if (!firstCharRegExp.test(firstChar)) {
    return NO_MATCH
  }

  let hasDecimals = firstChar === `.`

  let i: number
  for (i = position + 1; i < input.length; i += 1) {
    const char = asNotUndefined(input[i], sourceCodeInfo)
    if (endOfNumberRegExp.test(char)) {
      break
    }
    if (i === position + 1 && firstChar === `0`) {
      if (char === `b` || char === `B`) {
        type = `binary`
        continue
      }
      if (char === `o` || char === `O`) {
        type = `octal`
        continue
      }
      if (char === `x` || char === `X`) {
        type = `hex`
        continue
      }
    }
    if (type === `decimal` && hasDecimals) {
      if (!decimalNumberRegExp.test(char)) {
        return NO_MATCH
      }
    } else if (type === `binary`) {
      if (!binaryNumberRegExp.test(char)) {
        return NO_MATCH
      }
    } else if (type === `octal`) {
      if (!octalNumberRegExp.test(char)) {
        return NO_MATCH
      }
    } else if (type === `hex`) {
      if (!hexNumberRegExp.test(char)) {
        return NO_MATCH
      }
    } else {
      if (char === `.`) {
        hasDecimals = true
        continue
      }
      if (!decimalNumberRegExp.test(char)) {
        return NO_MATCH
      }
    }
  }

  const length = i - position
  const value = input.substring(position, i)
  if ((type !== `decimal` && length <= 2) || value === `.` || value === `-`) {
    return NO_MATCH
  }

  return [length, { type: `number`, value, sourceCodeInfo }]
}

export const tokenizeReservedName: Tokenizer = (input, position, sourceCodeInfo) => {
  for (const reservedName of Object.keys(reservedNamesRecord)) {
    const length = reservedName.length
    const nextChar = input[position + length]
    if (nextChar && nameRegExp.test(nextChar)) {
      continue
    }
    if (input.substr(position, length) === reservedName) {
      return [length, { type: `reservedName`, value: reservedName, sourceCodeInfo }]
    }
  }
  return NO_MATCH
}

export const tokenizeName: Tokenizer = (input, position, sourceCodeInfo) =>
  tokenizePattern(`name`, nameRegExp, input, position, sourceCodeInfo)

export const tokenizeModifier: Tokenizer = (input, position, sourceCodeInfo) => {
  const modifiers: ModifierName[] = [`&`, `&let`, `&when`, `&while`]
  for (const modifier of modifiers) {
    const length = modifier.length
    const charAfterModifier = input[position + length]
    if (input.substr(position, length) === modifier && (!charAfterModifier || !nameRegExp.test(charAfterModifier))) {
      const value: ModifierName = modifier
      return [length, { type: `modifier`, value, sourceCodeInfo }]
    }
  }
  return NO_MATCH
}

function tokenizeCharacter(
  type: TokenizerType,
  value: string,
  input: string,
  position: number,
  sourceCodeInfo: SourceCodeInfo,
): TokenDescriptor {
  if (value === input[position]) {
    return [1, { type, value, sourceCodeInfo }]
  } else {
    return NO_MATCH
  }
}

function tokenizePattern(
  type: TokenizerType,
  pattern: RegExp,
  input: string,
  position: number,
  sourceCodeInfo: SourceCodeInfo,
): TokenDescriptor {
  let char = input[position]
  let length = 0
  let value = ``

  if (!char || !pattern.test(char)) {
    return NO_MATCH
  }

  while (char && pattern.test(char)) {
    value += char
    length += 1
    char = input[position + length]
  }

  return [length, { type, value, sourceCodeInfo }]
}
