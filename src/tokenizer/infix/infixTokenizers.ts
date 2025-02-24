import { LitsError } from '../../errors'
import { infixIdentifierCharacterClass, infixIdentifierFirstCharacterClass } from '../../identifier'
import {
  NO_MATCH,
  tokenizeLeftBracket,
  tokenizeLeftCurly,
  tokenizeLeftParen,
  tokenizeRightBracket,
  tokenizeRightCurly,
  tokenizeRightParen,
  tokenizeSimpleToken,
  tokenizeString,
} from '../common/commonTokenizers'
import type { Tokenizer } from '../interface'
import { tokenizePF_Symbol } from '../postfix/postfixTokenizers'
import { infixReservedNamesRecord } from './infixReservedNames'
import type { IF_MultiLineCommentToken, IF_NumberToken, IF_OperatorToken, IF_PostfixToken, IF_ReservedSymbolToken, IF_SingleLineCommentToken, IF_SymbolToken, IF_WhitespaceToken, InfixToken } from './infixTokens'
import { isInfixOperator } from './infixTokens'

const identifierRegExp = new RegExp(infixIdentifierCharacterClass)
const identifierFirstCharacterRegExp = new RegExp(infixIdentifierFirstCharacterClass)
const whitespaceRegExp = /\s/

export const tokenizeIF_Whitespace: Tokenizer<IF_WhitespaceToken> = (input, position) => {
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
  return [value.length, ['IF_Whitespace', value]]
}

const decimalNumberRegExp = /\d/
const octalNumberRegExp = /[0-7]/
const hexNumberRegExp = /[0-9a-f]/i
const binaryNumberRegExp = /[01]/
const firstCharRegExp = /[0-9.]/
export const tokenizeIF_Number: Tokenizer<IF_NumberToken> = (input, position) => {
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

  return [length, ['IF_Number', value]]
}

export const tokenizeIF_ReservedSymbolToken: Tokenizer<IF_ReservedSymbolToken> = (input, position) => {
  for (const [reservedName, { forbidden }] of Object.entries(infixReservedNamesRecord)) {
    const length = reservedName.length
    const nextChar = input[position + length]
    if (nextChar && identifierRegExp.test(nextChar))
      continue

    const name = input.substring(position, position + length)
    if (name === reservedName) {
      if (forbidden)
        throw new LitsError(`${name} is forbidden!`)

      return [length, ['IF_ReservedSymbol', reservedName]]
    }
  }
  return NO_MATCH
}

export const tokenizeIF_Symbol: Tokenizer<IF_SymbolToken> = (input, position) => {
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
    return [position - initialPosition, ['IF_Symbol', value]]
  }

  if (value === '`') {
    position += 1
    const [count, pfSymbolToken] = tokenizePF_Symbol(input, position)
    if (pfSymbolToken === undefined) {
      return NO_MATCH
    }
    position += count
    if (input[position] !== '`') {
      return NO_MATCH
    }
    position += 1
    const pfValue = pfSymbolToken[1]
    return [position - initialPosition, ['IF_Symbol', pfValue]]
  }

  return NO_MATCH
}

export const tokenizeIF_Operator: Tokenizer<IF_OperatorToken> = (input, position) => {
  const threeChars = input.slice(position, position + 3)
  if (position + 2 < input.length && isInfixOperator(threeChars)) {
    return [3, ['IF_Operator', threeChars]]
  }

  const twoChars = input.slice(position, position + 2)
  if (position + 1 < input.length && isInfixOperator(twoChars)) {
    return [2, ['IF_Operator', twoChars]]
  }

  const oneChar = input[position] ?? ''
  if (isInfixOperator(oneChar)) {
    return [1, ['IF_Operator', oneChar]]
  }
  return NO_MATCH
}

export const tokenizeIF_PostfixToken: Tokenizer<IF_PostfixToken> = (input, position) =>
  tokenizeSimpleToken('IF_Postfix', '@', input, position)

export const tokenizeIF_MultiLineComment: Tokenizer<IF_MultiLineCommentToken> = (input, position) => {
  if (input[position] === '/' && input[position + 1] === '*') {
    let length = 2
    let value = '/*'
    while (input[position + length] !== '*' && input[position + length + 1] !== '/' && position + length + 1 < input.length) {
      value += input[position + length]
      length += 1
    }
    if (position + length + 1 >= input.length) {
      throw new LitsError('Comment not closed')
    }
    value += '*/'
    length += 2

    return [length, ['IF_MultiLineComment', value]]
  }
  return NO_MATCH
}

export const tokenizeIF_SingleLineComment: Tokenizer<IF_SingleLineCommentToken> = (input, position) => {
  if (input[position] === '/' && input[position + 1] === '/') {
    let length = 2
    let value = '//'
    while (input[position + length] !== '\n' && position + length < input.length) {
      value += input[position + length]
      length += 1
    }

    return [length, ['IF_SingleLineComment', value]]
  }
  return NO_MATCH
}

// All tokenizers, order matters!
export const infixTokenizers = [
  tokenizeIF_Whitespace,
  tokenizeIF_MultiLineComment,
  tokenizeIF_SingleLineComment,
  tokenizeIF_PostfixToken,
  tokenizeLeftParen,
  tokenizeRightParen,
  tokenizeLeftBracket,
  tokenizeRightBracket,
  tokenizeLeftCurly,
  tokenizeRightCurly,
  tokenizeString,
  tokenizeIF_Number,
  tokenizeIF_Operator,
  tokenizeIF_ReservedSymbolToken,
  tokenizeIF_Symbol,
] as const satisfies Tokenizer<InfixToken>[]
