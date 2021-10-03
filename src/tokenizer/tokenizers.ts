import { ModifierName } from '../parser/interface'
import { reservedNamesRecord } from '../reservedNames'
import { asNotUndefined } from '../utils'
import { TokenDescriptor, Tokenizer, TokenizerType } from './interface'

// A name (function or variable) can contain a lot of different characters
const nameRegExp = /[0-9a-zA-Z_^?=!$%<>.+*/-]/

export const skipWhiteSpace: Tokenizer = (input, current) =>
  /\s/.test(input[current] ?? '') ? [1, undefined] : [0, undefined]

export const skipComment: Tokenizer = (input, current) => {
  if (input[current] === ';') {
    let length = 1
    while (input[current + length] !== '\n' && current + length < input.length) {
      length += 1
    }
    if (input[current + length] === '\n' && current + length < input.length) {
      length += 1
    }
    return [length, undefined]
  }
  return [0, undefined]
}

export const tokenizeLeftParen: Tokenizer = (input: string, position: number) =>
  tokenizeCharacter('paren', '(', input, position)

export const tokenizeRightParen: Tokenizer = (input: string, position: number) =>
  tokenizeCharacter('paren', ')', input, position)

export const tokenizeLeftBracket: Tokenizer = (input: string, position: number) =>
  tokenizeCharacter('paren', '[', input, position)

export const tokenizeRightBracket: Tokenizer = (input: string, position: number) =>
  tokenizeCharacter('paren', ']', input, position)

export const tokenizeLeftCurly: Tokenizer = (input: string, position: number) =>
  tokenizeCharacter('paren', '{', input, position)

export const tokenizeRightCurly: Tokenizer = (input: string, position: number) =>
  tokenizeCharacter('paren', '}', input, position)

export const tokenizeString: Tokenizer = (input, position) => {
  if (input[position] !== '"') {
    return [0, undefined]
  }

  let value = ''
  let length = 1
  let char = input[position + length]
  let escape = false
  while (char !== '"' || escape) {
    if (char === undefined) {
      throw new SyntaxError(`Unclosed string at position ${position}`)
    }
    length += 1
    if (escape) {
      escape = false
      if (char === '"' || char === '\\') {
        value += char
      } else {
        value += '\\'
        value += char
      }
    } else {
      if (char === '\\') {
        escape = true
      } else {
        value += char
      }
    }
    char = input[position + length]
  }
  return [length + 1, { type: 'string', value }]
}

const endOfNumberRegExp = /\s|[)\]}]/
const decimalNumberRegExp = /[0-9]/
const octalNumberRegExp = /[0-7]/
const hexNumberRegExp = /[0-9a-fA-F]/
const binaryNumberRegExp = /[0-1]/
const firstCharRegExp = /[0-9.-]/
export const tokenizeNumber: Tokenizer = (input: string, position: number) => {
  let type: 'decimal' | 'octal' | 'hex' | 'binary' = 'decimal'
  const firstChar = input[position]
  if (firstChar === undefined) {
    return [0, undefined]
  }
  let hasDecimals = firstChar === '.'
  if (!firstCharRegExp.test(firstChar)) {
    return [0, undefined]
  }

  let i: number
  for (i = position + 1; i < input.length; i += 1) {
    const char = asNotUndefined(input[i])
    if (endOfNumberRegExp.test(char)) {
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
      if (!decimalNumberRegExp.test(char)) {
        return [0, undefined]
      }
    } else if (type === 'binary') {
      if (!binaryNumberRegExp.test(char)) {
        return [0, undefined]
      }
    } else if (type === 'octal') {
      if (!octalNumberRegExp.test(char)) {
        return [0, undefined]
      }
    } else if (type === 'hex') {
      if (!hexNumberRegExp.test(char)) {
        return [0, undefined]
      }
    } else {
      if (char === '.') {
        hasDecimals = true
        continue
      }
      if (!decimalNumberRegExp.test(char)) {
        return [0, undefined]
      }
    }
  }

  const length = i - position
  const value = input.substring(position, i)
  if ((type !== 'decimal' && length <= 2) || value === '.' || value === '-') {
    return [0, undefined]
  }

  return [length, { type: 'number', value }]
}

export function tokenizeReservedName(input: string, position: number): TokenDescriptor {
  for (const reservedName of Object.keys(reservedNamesRecord)) {
    const length = reservedName.length
    const nextChar = input[position + length]
    if (nextChar && nameRegExp.test(nextChar)) {
      continue
    }
    if (input.substr(position, length) === reservedName) {
      return [length, { type: 'reservedName', value: reservedName }]
    }
  }
  return [0, undefined]
}

export const tokenizeName: Tokenizer = (input: string, position: number) =>
  tokenizePattern('name', nameRegExp, input, position)

export const tokenizeShorthand: Tokenizer = (input: string, position: number) => {
  if (input.substr(position, 2) === `#'`) {
    return [2, { type: 'shorthand', value: `#'` }]
  }
  // if (input.substr(position, 2) === `'(`) {
  //   return [1, { type: 'shorthand', value: `'` }]
  // }
  return [0, undefined]
}

export const tokenizeModifier: Tokenizer = (input: string, position: number) => {
  if (input.substr(position, 5) === `&rest`) {
    const value: ModifierName = '&rest'
    return [5, { type: 'modifier', value }]
  }
  if (input.substr(position, 9) === `&optional`) {
    const value: ModifierName = '&optional'
    return [9, { type: 'modifier', value }]
  }
  if (input.substr(position, 5) === `&bind`) {
    const value: ModifierName = '&bind'
    return [5, { type: 'modifier', value }]
  }
  return [0, undefined]
}

function tokenizeCharacter(type: TokenizerType, value: string, input: string, position: number): TokenDescriptor {
  if (value === input[position]) {
    return [1, { type, value }]
  } else {
    return [0, undefined]
  }
}

function tokenizePattern(type: TokenizerType, pattern: RegExp, input: string, position: number): TokenDescriptor {
  let char = input[position]
  let length = 0
  let value = ''

  if (!char || !pattern.test(char)) {
    return [0, undefined]
  }

  while (char && pattern.test(char)) {
    value += char
    length += 1
    char = input[position + length]
  }

  return [length, { type, value }]
}
