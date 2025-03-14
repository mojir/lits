import { LitsError } from '../errors'
import type { TokenDescriptor, Tokenizer } from './interface'
import { isSymbolicOperator } from './operators'
import type { A_BasePrefixedNumberToken, A_MultiLineCommentToken, A_NumberToken, A_OperatorToken, A_ReservedSymbolToken, A_SingleLineCommentToken, A_SymbolToken, A_WhitespaceToken, LBraceToken, LBracketToken, LParenToken, RBraceToken, RBracketToken, RParenToken, RegexpShorthandToken, StringToken, Token } from './tokens'
import type { AlgebraicReservedSymbol, ValidReservedSymbol } from './reservedNames'
import { algebraicReservedSymbolRecord } from './reservedNames'

const illegalSymbolCharacters = [
  '(',
  ')',
  '[',
  ']',
  '{',
  '}',
  '\'',
  '"',
  '`',
  ',',
  '.',
  ';',
  ' ',
  '\n',
  '\r',
  '\t',
]
const illegalFirstSymbolCharacters = [
  '0',
  '1',
  '2',
  '3',
  '4',
  '5',
  '6',
  '7',
  '8',
  '9',
  ...illegalSymbolCharacters,
]
const illegalSymbolCharacterSet = new Set(illegalSymbolCharacters)
const illegalFirstSymbolCharacterSet = new Set(illegalFirstSymbolCharacters)

const whitespaceRegExp = /\s/

export const NO_MATCH: TokenDescriptor<never> = [0]

const tokenizeLParen: Tokenizer<LParenToken> = (input, position) =>
  tokenizeToken('LParen', '(', input, position)
const tokenizeRParen: Tokenizer<RParenToken> = (input, position) =>
  tokenizeToken('RParen', ')', input, position)
const tokenizeLBracket: Tokenizer<LBracketToken> = (input, position) =>
  tokenizeToken('LBracket', '[', input, position)
const tokenizeRBracket: Tokenizer<RBracketToken> = (input, position) =>
  tokenizeToken('RBracket', ']', input, position)
const tokenizeLBrace: Tokenizer<LBraceToken> = (input, position) =>
  tokenizeToken('LBrace', '{', input, position)
const tokenizeRBrace: Tokenizer<RBraceToken> = (input, position) =>
  tokenizeToken('RBrace', '}', input, position)

const tokenizeString: Tokenizer<StringToken> = (input, position) => {
  if (input[position] !== '"')
    return NO_MATCH

  let value = '"'
  let length = 1
  let char = input[position + length]
  let escaping = false
  while (char !== '"' || escaping) {
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
  value += '"' // closing quote
  return [length + 1, ['String', value]]
}

const tokenizeRegexpShorthand: Tokenizer<RegexpShorthandToken> = (input, position) => {
  if (input[position] !== '#')
    return NO_MATCH

  const [stringLength, token] = tokenizeString(input, position + 1)
  if (!token)
    return NO_MATCH

  position += stringLength + 1
  let length = stringLength + 1

  let options = ''
  while (input[position] === 'g' || input[position] === 'i') {
    if (options.includes(input[position]!)) {
      throw new LitsError(`Duplicated regexp option "${input[position]}" at position ${position}.`, undefined)
    }
    options += input[position]!
    length += 1
    position += 1
  }

  return [length, ['RegexpShorthand', `#${token[1]}${options}`]]
}

function tokenizeToken<T extends Token>(
  type: T[0],
  value: string,
  input: string,
  position: number,
): TokenDescriptor<T> {
  if (value === input.slice(position, position + value.length))
    return [value.length, [type, value] as T]
  else
    return NO_MATCH
}

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
  const negate = input[position] === '-'
  const plusPrefix = input[position] === '+'
  const start = negate || plusPrefix ? position + 1 : position
  let hasDecimalPoint = false
  let hasExponent = false
  for (i = start; i < input.length; i += 1) {
    const char = input[i] as string

    if (char === '_') {
      if (!decimalNumberRegExp.test(input[i - 1]!) || !decimalNumberRegExp.test(input[i + 1]!)) {
        return NO_MATCH
      }
    }

    else if (char === '.') {
      if (i === start || hasDecimalPoint || hasExponent) {
        return NO_MATCH
      }
      hasDecimalPoint = true
    }

    else if (char === 'e' || char === 'E') {
      if (i === start || hasExponent) {
        return NO_MATCH
      }

      if (input[i - 1] === '.' || input[i - 1] === '+' || input[i - 1] === '-') {
        return NO_MATCH
      }

      if (input[i + 1] === '+' || input[i + 1] === '-') {
        i += 1
      }

      hasExponent = true
    }

    else if (!decimalNumberRegExp.test(char)) {
      break
    }
  }

  if ((negate || plusPrefix) && i === start) {
    return NO_MATCH
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

  if (!illegalFirstSymbolCharacterSet.has(value)) {
    const initialPosition = position
    position += 1
    let char = input[position]

    while (char && !illegalSymbolCharacterSet.has(char)) {
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

  const info = algebraicReservedSymbolRecord[symbolName as AlgebraicReservedSymbol]
  if (info === undefined) {
    return NO_MATCH
  }
  return [symbolMeta[0], ['A_ReservedSymbol', symbolName as ValidReservedSymbol]]
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
    while ((input[position + length] !== '*' || input[position + length + 1] !== '/') && position + length + 1 < input.length) {
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
export const tokenizers = [
  tokenizeA_Whitespace,
  tokenizeA_MultiLineComment,
  tokenizeA_SingleLineComment,
  tokenizeA_ReservedSymbolToken,
  tokenizeLParen,
  tokenizeRParen,
  tokenizeLBracket,
  tokenizeRBracket,
  tokenizeLBrace,
  tokenizeRBrace,
  tokenizeString,
  tokenizeRegexpShorthand,
  tokenizeA_BasePrefixedNumber,
  tokenizeA_Number,
  tokenizeA_Operator,
  tokenizeA_Symbol,
] as const satisfies Tokenizer<Token>[]
