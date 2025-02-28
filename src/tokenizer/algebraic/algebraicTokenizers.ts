import { LitsError } from '../../errors'
import { algebraicIdentifierCharacterClass, algebraicIdentifierFirstCharacterClass } from '../../identifier'
import {
  NO_MATCH,
  commonTokenizers,
} from '../common/commonTokenizers'
import type { Tokenizer } from '../interface'
import { tokenizeP_Symbol } from '../polish/polishTokenizers'
import { algebraicReservedNamesRecord } from './algebraicReservedNames'
import type { A_MultiLineCommentToken, A_NumberToken, A_OperatorToken, A_ReservedSymbolToken, A_SingleLineCommentToken, A_SymbolToken, A_WhitespaceToken, AlgebraicToken } from './algebraicTokens'
import { isSymbolicOperator } from './algebraicTokens'

const identifierRegExp = new RegExp(algebraicIdentifierCharacterClass)
const identifierFirstCharacterRegExp = new RegExp(algebraicIdentifierFirstCharacterClass)
const whitespaceRegExp = /\s/

export const tokenizeA_Whitespace: Tokenizer<A_WhitespaceToken> = (input, position) => {
  let char = input[position]
  if (!char || !whitespaceRegExp.test(char)) {
    return NO_MATCH
  }
  let value = char
  position += 1
  char = input[position]
  while (char && whitespaceRegExp.test(char)) {
    value += char
    position += 1
    char = input[position]
  }
  return [value.length, ['A_Whitespace', value]]
}

const decimalNumberRegExp = /\d/
const octalNumberRegExp = /[0-7]/
const hexNumberRegExp = /[0-9a-f]/i
const binaryNumberRegExp = /[01]/
const firstCharRegExp = /[0-9.]/
export const tokenizeA_Number: Tokenizer<A_NumberToken> = (input, position) => {
  let type: 'decimal' | 'octal' | 'hex' | 'binary' = 'decimal'
  const firstChar = input[position] as string
  if (!firstCharRegExp.test(firstChar))
    return NO_MATCH

  let hasDecimals = firstChar === '.'

  let i: number
  for (i = position + 1; i < input.length; i += 1) {
    const char = input[i] as string

    if ((i === position + 1 && firstChar === '0')) {
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

    if (type === 'decimal') {
      if (hasDecimals) {
        if (!decimalNumberRegExp.test(char)) {
          break
        }
      }
      else if (char !== '.' && !decimalNumberRegExp.test(char)) {
        break
      }
    }
    if (type === 'binary' && !binaryNumberRegExp.test(char)) {
      break
    }
    if (type === 'octal' && !octalNumberRegExp.test(char)) {
      break
    }
    if (type === 'hex' && !hexNumberRegExp.test(char)) {
      break
    }

    if (char === '.') {
      const nextChar = input[i + 1]
      if (typeof nextChar === 'string' && !decimalNumberRegExp.test(nextChar))
        break
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

  return [length, ['A_Number', value]]
}

export const tokenizeA_ReservedSymbolToken: Tokenizer<A_ReservedSymbolToken> = (input, position) => {
  for (const [reservedName, { forbidden }] of Object.entries(algebraicReservedNamesRecord)) {
    const length = reservedName.length
    const nextChar = input[position + length]
    if (nextChar && identifierRegExp.test(nextChar))
      continue

    const name = input.substring(position, position + length)
    if (name === reservedName) {
      if (forbidden)
        throw new LitsError(`${name} is forbidden!`, undefined)

      return [length, ['A_ReservedSymbol', reservedName]]
    }
  }
  return NO_MATCH
}

export const tokenizeA_Symbol: Tokenizer<A_SymbolToken> = (input, position) => {
  const initialPosition = position
  let value = input[position]

  if (!value) {
    return NO_MATCH
  }

  if (identifierFirstCharacterRegExp.test(value)) {
    position += 1
    let char = input[position]

    while (char && identifierRegExp.test(char)) {
      value += char
      position += 1
      char = input[position]
    }
    return [position - initialPosition, ['A_Symbol', value]]
  }

  if (value === '\'') {
    position += 1
    const [count, pfSymbolToken] = tokenizeP_Symbol(input, position)
    if (pfSymbolToken === undefined) {
      return NO_MATCH
    }
    position += count
    if (input[position] !== '\'') {
      return NO_MATCH
    }
    position += 1
    const pfValue = pfSymbolToken[1]
    return [position - initialPosition, ['A_Symbol', pfValue]]
  }

  return NO_MATCH
}

export const tokenizeA_Operator: Tokenizer<A_OperatorToken> = (input, position) => {
  const threeChars = input.slice(position, position + 3)
  if (position + 2 < input.length && isSymbolicOperator(threeChars)) {
    return [3, ['A_Operator', threeChars]]
  }

  const twoChars = input.slice(position, position + 2)
  if (position + 1 < input.length && isSymbolicOperator(twoChars)) {
    return [2, ['A_Operator', twoChars]]
  }

  const oneChar = input[position] ?? ''
  if (isSymbolicOperator(oneChar)) {
    return [1, ['A_Operator', oneChar]]
  }
  return NO_MATCH
}

export const tokenizeA_MultiLineComment: Tokenizer<A_MultiLineCommentToken> = (input, position) => {
  if (input[position] === '/' && input[position + 1] === '*') {
    let length = 2
    let value = '/*'
    while (input[position + length] !== '*' && input[position + length + 1] !== '/' && position + length + 1 < input.length) {
      value += input[position + length]
      length += 1
    }
    if (position + length + 1 >= input.length) {
      throw new LitsError('Comment not closed', undefined)
    }
    value += '*/'
    length += 2

    return [length, ['A_MultiLineComment', value]]
  }
  return NO_MATCH
}

export const tokenizeA_SingleLineComment: Tokenizer<A_SingleLineCommentToken> = (input, position) => {
  if (input[position] === '/' && input[position + 1] === '/') {
    let length = 2
    let value = '//'
    while (input[position + length] !== '\n' && position + length < input.length) {
      value += input[position + length]
      length += 1
    }

    return [length, ['A_SingleLineComment', value]]
  }
  return NO_MATCH
}

// All tokenizers, order matters!
export const algebraicTokenizers = [
  tokenizeA_Whitespace,
  tokenizeA_MultiLineComment,
  tokenizeA_SingleLineComment,
  ...commonTokenizers,
  tokenizeA_Number,
  tokenizeA_Operator,
  tokenizeA_ReservedSymbolToken,
  tokenizeA_Symbol,
] as const satisfies Tokenizer<AlgebraicToken>[]
