import { LitsError } from '../../errors'
import { algebraicIdentifierCharacterClass, algebraicIdentifierFirstCharacterClass } from '../../identifier'
import {
  NO_MATCH,
  commonTokenizers,
} from '../common/commonTokenizers'
import type { Tokenizer } from '../interface'
import type { AlgebraicReservedName, ValidAlgebraicReservedName } from './algebraicReservedNames'
import { algebraicReservedNamesRecord } from './algebraicReservedNames'
import type { A_BasePrefixedNumberToken, A_MultiLineCommentToken, A_NumberToken, A_OperatorToken, A_ReservedSymbolToken, A_SingleLineCommentToken, A_SymbolToken, A_WhitespaceToken, AlgebraicToken } from './algebraicTokens'
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

export const tokenizeA_Number: Tokenizer<A_NumberToken> = (input, position) => {
  let i: number
  for (i = position; i < input.length; i += 1) {
    const char = input[i] as string
    if (!decimalNumberRegExp.test(char)) {
      break
    }
  }

  const length = i - position
  if (length === 0) {
    return NO_MATCH
  }

  return [length, ['A_Number', input.substring(position, i)]]
}

export const tokenizeA_BasePrefixedNumber: Tokenizer<A_BasePrefixedNumberToken> = (input, position) => {
  if (input[position] !== '0') {
    return NO_MATCH
  }

  const baseChar = input[position + 1]

  const type = baseChar === 'b' || baseChar === 'B'
    ? 'binary'
    : baseChar === 'o' || baseChar === 'O'
      ? 'octal'
      : baseChar === 'x' || baseChar === 'X'
        ? 'hex'
        : null

  if (type === null) {
    return NO_MATCH
  }

  let i: number
  for (i = position + 2; i < input.length; i += 1) {
    const char = input[i] as string
    if (type === 'binary' && !binaryNumberRegExp.test(char)) {
      break
    }
    if (type === 'octal' && !octalNumberRegExp.test(char)) {
      break
    }
    if (type === 'hex' && !hexNumberRegExp.test(char)) {
      break
    }
  }

  const length = i - position
  if (length <= 2) {
    return NO_MATCH
  }

  return [length, ['A_BasePrefixedNumber', input.substring(position, i)]]
}

export const tokenizeA_Symbol: Tokenizer<A_SymbolToken> = (input, position) => {
  let value = input[position]

  if (!value) {
    return NO_MATCH
  }

  if (value === '\'') {
    let length = 1
    let char = input[position + length]
    let escaping = false
    while (char !== '\'' || escaping) {
      if (char === undefined)
        throw new LitsError(`Unclosed string at position ${position}.`, undefined)

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
    value += '\'' // closing quote
    return [length + 1, ['A_Symbol', value]]
  }

  if (identifierFirstCharacterRegExp.test(value)) {
    const initialPosition = position
    position += 1
    let char = input[position]

    while (char && identifierRegExp.test(char)) {
      value += char
      position += 1
      char = input[position]
    }
    return [position - initialPosition, ['A_Symbol', value]]
  }

  return NO_MATCH
}

export const tokenizeA_ReservedSymbolToken: Tokenizer<A_ReservedSymbolToken> = (input, position) => {
  const symbolMeta = tokenizeA_Symbol(input, position)
  if (symbolMeta[0] === 0 || !symbolMeta[1]) {
    return NO_MATCH
  }
  let symbolName = symbolMeta[1][1]
  symbolName = symbolName.startsWith('\'') ? symbolName.slice(1, symbolName.length - 1) : symbolName

  const info = algebraicReservedNamesRecord[symbolName as AlgebraicReservedName]
  if (!info) {
    return NO_MATCH
  }
  if (info.forbidden) {
    throw new LitsError(`${symbolName} is forbidden!`, undefined)
  }
  return [symbolMeta[0], ['A_ReservedSymbol', symbolName as ValidAlgebraicReservedName]]
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
  tokenizeA_BasePrefixedNumber,
  tokenizeA_Number,
  tokenizeA_Operator,
  tokenizeA_ReservedSymbolToken,
  tokenizeA_Symbol,
] as const satisfies Tokenizer<AlgebraicToken>[]
