import { isSymbolicOperator } from './operators'
import type { BasePrefixedNumberToken, DocStringToken, ErrorToken, LBraceToken, LBracketToken, LParenToken, MultiLineCommentToken, NumberToken, OperatorToken, RBraceToken, RBracketToken, RParenToken, RegexpShorthandToken, ReservedSymbolToken, SingleLineCommentToken, StringToken, SymbolToken, Token, TokenDescriptor, WhitespaceToken } from './token'
import type { ReservedSymbol } from './reservedNames'
import { reservedSymbolRecord } from './reservedNames'

export type Tokenizer<T extends Token> = (input: string, position: number) => TokenDescriptor<T | ErrorToken>

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

export const tokenizeDocString: Tokenizer<DocStringToken> = (input, position) => {
  if (input[position] !== '"' || input[position + 1] !== '"' || input[position + 2] !== '"')
    return NO_MATCH

  let value = '"""'
  let length = 3
  let char = input[position + length]
  let nextThreeChars = input.slice(position + length, position + length + 3)
  let escaping = false
  while (char && (nextThreeChars !== '"""' || escaping)) {
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
    nextThreeChars = input.slice(position + length, position + length + 3)
  }
  if (!char) {
    return [length, ['Error', value, undefined, `Unclosed doc string at position ${position}`]]
  }
  value += '"""' // closing quote
  return [length + 3, ['DocString', value]]
}

const tokenizeString: Tokenizer<StringToken> = (input, position) => {
  if (input[position] !== '"')
    return NO_MATCH

  let value = '"'
  let length = 1
  let char = input[position + length]
  let escaping = false
  while (char && (char !== '"' || escaping)) {
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
  if (!char) {
    return [length, ['Error', value, undefined, `Unclosed string at position ${position}`]]
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

  if (token[0] === 'Error') {
    const errorToken: ErrorToken = ['Error', `#${token[1]}`, undefined, `Unclosed regexp at position ${position}`]
    return [stringLength + 1, errorToken]
  }

  position += stringLength + 1
  let length = stringLength + 1

  let options = ''
  while (input[position] === 'g' || input[position] === 'i') {
    options += input[position]!
    length += 1
    position += 1
    if (options.includes(input[position]!)) {
      return [length, ['Error', `#${token[1]}${options}`, undefined, `Duplicated regexp option "${input[position]}"`]]
    }
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

export const tokenizeWhitespace: Tokenizer<WhitespaceToken> = (input, position) => {
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
  return [value.length, ['Whitespace', value]]
}

const decimalNumberRegExp = /\d/
const octalNumberRegExp = /[0-7]/
const hexNumberRegExp = /[0-9a-f]/i
const binaryNumberRegExp = /[01]/
const postNumberRegExp = /[\s)\]}(,;]/

export const tokenizeNumber: Tokenizer<NumberToken> = (input, position) => {
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
        if (i === start) {
          return NO_MATCH
        }
        return [i - position + 1, ['Error', input.substring(position, i + 1), undefined, `Invalid number format at position ${i + 1}`]]
      }
    }

    else if (char === '.') {
      if (i === start) {
        return NO_MATCH
      }
      if (hasDecimalPoint || hasExponent) {
        return [i - position + 1, ['Error', input.substring(position, i + 1), undefined, `Invalid number format at position ${i + 1}`]]
      }
      hasDecimalPoint = true
    }

    else if (char === 'e' || char === 'E') {
      if (i === start) {
        return NO_MATCH
      }

      if (hasExponent) {
        return [i - position + 1, ['Error', input.substring(position, i + 1), undefined, `Invalid number format at position ${i + 1}`]]
      }

      if (input[i - 1] === '.' || input[i - 1] === '+' || input[i - 1] === '-') {
        return [i - position + 1, ['Error', input.substring(position, i + 1), undefined, `Invalid number format at position ${i + 1}`]]
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

  const nextChar = input[i]
  if (nextChar && nextChar !== ':' && !postNumberRegExp.test(nextChar)) {
    return [i - position + 1, ['Error', input.substring(position, i + 1), undefined, `Invalid number format at position ${i + 1}`]]
  }

  return [length, ['Number', input.substring(position, i)]]
}

export const tokenizeBasePrefixedNumber: Tokenizer<BasePrefixedNumberToken> = (input, position) => {
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

  const nextChar = input[i]
  if (nextChar && !postNumberRegExp.test(nextChar)) {
    return NO_MATCH
  }

  return [length, ['BasePrefixedNumber', input.substring(position, i)]]
}

export const tokenizeSymbol: Tokenizer<SymbolToken> = (input, position) => {
  let value = input[position]!

  if (value === '\'') {
    let length = 1
    let char = input[position + length]
    let escaping = false
    while (char !== '\'' || escaping) {
      if (char === undefined)
        return [length, ['Error', value, undefined, `Unclosed quoted symbol at position ${position}`]]

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
    return [length + 1, ['Symbol', value]]
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

    // : can be used as symbol character, but it must not be the last character
    return value.endsWith(':')
      ? [position - initialPosition - 1, ['Symbol', value.slice(0, -1)]]
      : [position - initialPosition, ['Symbol', value]]
  }

  return NO_MATCH
}

export const tokenizeReservedSymbolToken: Tokenizer<ReservedSymbolToken> = (input, position) => {
  const symbolMeta = tokenizeSymbol(input, position)
  if (symbolMeta[0] === 0 || !symbolMeta[1]) {
    return NO_MATCH
  }
  let symbolName = symbolMeta[1][1]
  symbolName = symbolName.startsWith('\'') ? symbolName.slice(1, symbolName.length - 1) : symbolName

  const info = reservedSymbolRecord[symbolName as ReservedSymbol]
  if (info === undefined) {
    return NO_MATCH
  }
  return [symbolMeta[0], ['ReservedSymbol', symbolName as ReservedSymbol]]
}

export const tokenizeOperator: Tokenizer<OperatorToken> = (input, position) => {
  const threeChars = input.slice(position, position + 3)
  if (position + 2 < input.length && isSymbolicOperator(threeChars)) {
    return [3, ['Operator', threeChars]]
  }

  const twoChars = input.slice(position, position + 2)
  if (position + 1 < input.length && isSymbolicOperator(twoChars)) {
    return [2, ['Operator', twoChars]]
  }

  const oneChar = input[position] ?? ''
  if (isSymbolicOperator(oneChar)) {
    return [1, ['Operator', oneChar]]
  }
  return NO_MATCH
}

export const tokenizeMultiLineComment: Tokenizer<MultiLineCommentToken> = (input, position) => {
  if (input[position] === '/' && input[position + 1] === '*') {
    let length = 2
    let value = '/*'
    while ((input[position + length] !== '*' || input[position + length + 1] !== '/') && position + length + 1 < input.length) {
      value += input[position + length]
      length += 1
    }
    if (position + length + 1 >= input.length) {
      return [length, ['Error', value, undefined, `Unclosed multi-line comment at position ${position}`]]
    }
    value += '*/'
    length += 2

    return [length, ['MultiLineComment', value]]
  }
  return NO_MATCH
}

export const tokenizeShebang: Tokenizer<SingleLineCommentToken> = (input, position) => {
  if (input[position] === '#' && input[position + 1] === '!') {
    let length = 2
    let value = '#!'
    while (input[position + length] !== '\n' && position + length < input.length) {
      value += input[position + length]
      length += 1
    }

    return [length, ['SingleLineComment', value]]
  }
  return NO_MATCH
}

export const tokenizeSingleLineComment: Tokenizer<SingleLineCommentToken> = (input, position) => {
  if (input[position] === '/' && input[position + 1] === '/') {
    let length = 2
    let value = '//'
    while (input[position + length] !== '\n' && position + length < input.length) {
      value += input[position + length]
      length += 1
    }

    return [length, ['SingleLineComment', value]]
  }
  return NO_MATCH
}

// All tokenizers, order matters!
export const tokenizers = [
  tokenizeWhitespace,
  tokenizeMultiLineComment,
  tokenizeSingleLineComment,
  tokenizeReservedSymbolToken,
  tokenizeLParen,
  tokenizeRParen,
  tokenizeLBracket,
  tokenizeRBracket,
  tokenizeLBrace,
  tokenizeRBrace,
  tokenizeDocString,
  tokenizeString,
  tokenizeRegexpShorthand,
  tokenizeBasePrefixedNumber,
  tokenizeNumber,
  tokenizeOperator,
  tokenizeSymbol,
] as const satisfies Tokenizer<Token>[]
