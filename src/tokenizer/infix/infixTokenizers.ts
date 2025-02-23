import { LitsError } from '../../errors'
import { infixIdentifierCharacterClass, infixIdentifierFirstCharacterClass } from '../../identifier'
import {
  NO_MATCH,
  tokenizeLeftBracket,
  tokenizeLeftCurly,
  tokenizeLeftParen,
  tokenizeNumber,
  tokenizeRightBracket,
  tokenizeRightCurly,
  tokenizeRightParen,
  tokenizeSimpleToken,
  tokenizeString,
} from '../common/tokenizers'
import type { Tokenizer } from '../interface'
import { tokenizePF_Symbol } from '../postfix/postfixTokenizers'
import { infixReservedNamesRecord } from './infixReservedNames'
import type { IF_MultiLineCommentToken, IF_OperatorToken, IF_PostfixToken, IF_ReservedSymbolToken, IF_SingleLineCommentToken, IF_SymbolToken, IF_WhitespaceToken, InfixToken } from './infixTokens'
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
  tokenizeIF_Operator,
  tokenizeLeftParen,
  tokenizeRightParen,
  tokenizeLeftBracket,
  tokenizeRightBracket,
  tokenizeLeftCurly,
  tokenizeRightCurly,
  tokenizeString,
  tokenizeNumber,
  tokenizeIF_ReservedSymbolToken,
  tokenizeIF_Symbol,
] as const satisfies Tokenizer<InfixToken>[]
