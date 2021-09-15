import { reservedNamesRecord } from '../reservedNames'
import { asNotUndefined } from '../utils'
import { TokenDescriptor, Tokenizer, TokenizerType } from './interface'

// A name (function or variable) can contain a lot of different characters
const nameRegExp = /[0-9a-zA-Z_^?=!$%<>.+*/\-[\]]/
const fullNumberRegExp = /^-?\d+(\.\d+)?$/

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

export const tokenizeNumber: Tokenizer = (input: string, position: number) => {
  const result = tokenizePattern('number', /[0-9.-]/, input, position)
  const [length, token] = result
  if (length === 0) {
    return result
  }

  const nextPosition = position + length
  const nextChar = input[nextPosition]
  if (nextChar && nameRegExp.test(nextChar)) {
    return [0, undefined]
  }
  const value = asNotUndefined(token).value

  if (!fullNumberRegExp.test(value)) {
    return [0, undefined]
  }
  return result
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
  return [0, undefined]
}

export const tokenizeRest: Tokenizer = (input: string, position: number) => {
  if (input.substr(position, 5) === `&rest`) {
    return [5, { type: 'rest', value: `&rest` }]
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
