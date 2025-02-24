import { LitsError } from '../../errors'
import { postfixIdentifierCharacterClass } from '../../identifier'
import {
  NO_MATCH,
  isNoMatch,
  tokenizeLeftBracket,
  tokenizeLeftCurly,
  tokenizeLeftParen,
  tokenizeRightBracket,
  tokenizeRightCurly,
  tokenizeRightParen,
  tokenizeString,
} from '../common/commonTokenizers'
import type { Tokenizer } from '../interface'
import { postfixReservedNamesRecord } from './postfixReservedNames'
import type { ModifierName, PF_CollectionAccessorToken, PF_CommentToken, PF_FnShorthandToken, PF_InfixToken, PF_ModifierToken, PF_NumberToken, PF_RegexpShorthandToken, PF_ReservedSymbolToken, PF_StringShorthandToken, PF_SymbolToken, PF_WhitespaceToken, PostfixToken } from './postfixTokens'
import { asPF_SymbolToken, modifierNames } from './postfixTokens'

const whitespaceRegExp = /\s|,/

export const tokenizePF_Comment: Tokenizer<PF_CommentToken> = (input, position) => {
  if (input[position] === ';') {
    let length = 0
    let value = ''
    while (input[position + length] !== '\n' && position + length < input.length) {
      value += input[position + length]
      length += 1
    }

    return [length, ['PF_Comment', value]]
  }
  return NO_MATCH
}

export const tokenizePF_Whitespace: Tokenizer<PF_WhitespaceToken> = (input, position) => {
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
  return [value.length, ['PF_Whitespace', value]]
}

const endOfNumberRegExp = /[\s)\]},#]/
const decimalNumberRegExp = /\d/
const octalNumberRegExp = /[0-7]/
const hexNumberRegExp = /[0-9a-f]/i
const binaryNumberRegExp = /[01]/
const firstCharRegExp = /[0-9.-]/
export const tokenizePF_Number: Tokenizer<PF_NumberToken> = (input, position) => {
  let type: 'decimal' | 'octal' | 'hex' | 'binary' = 'decimal'
  const firstChar = input[position] as string
  if (!firstCharRegExp.test(firstChar))
    return NO_MATCH

  let hasDecimals = firstChar === '.'

  let i: number
  for (i = position + 1; i < input.length; i += 1) {
    const char = input[i] as string
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

  return [length, ['PF_Number', value]]
}

export const pf_symbolRegExp = new RegExp(postfixIdentifierCharacterClass)
export const tokenizePF_Symbol: Tokenizer<PF_SymbolToken> = (input, position) => {
  let char = input[position]
  let length = 0
  let value = ''

  if (!char || !pf_symbolRegExp.test(char))
    return NO_MATCH

  while (char && pf_symbolRegExp.test(char)) {
    value += char
    length += 1
    char = input[position + length]
  }

  return [length, ['PF_Symbol', value]]
}

export const tokenizePF_FnShorthand: Tokenizer<PF_FnShorthandToken> = (input, position) => {
  if (input.slice(position, position + 2) !== '#(')
    return NO_MATCH

  return [1, ['PF_FnShorthand']]
}

export const tokenizePF_ReservedSymbol: Tokenizer<PF_ReservedSymbolToken> = (input, position) => {
  for (const [reservedName, { forbidden }] of Object.entries(postfixReservedNamesRecord)) {
    const length = reservedName.length
    const nextChar = input[position + length]
    if (nextChar && pf_symbolRegExp.test(nextChar)) {
      continue
    }

    const symbol = input.substring(position, position + length)
    if (symbol === reservedName) {
      if (forbidden)
        throw new LitsError(`${symbol} is forbidden!`)

      return [length, ['PF_ReservedSymbol', reservedName]]
    }
  }
  return NO_MATCH
}

const tokenizePF_StringShorthand: Tokenizer<PF_StringShorthandToken> = (input, position) => {
  if (input[position] !== ':')
    return NO_MATCH

  const symbolDescription = tokenizePF_Symbol(input, position + 1)
  if (isNoMatch(symbolDescription)) {
    return symbolDescription
  }

  const symbolToken = asPF_SymbolToken(symbolDescription[1])

  return [symbolDescription[0] + 1, ['PF_StringShorthand', `:${symbolToken[1]}`]]
}

export const tokenizePF_Modifier: Tokenizer<PF_ModifierToken> = (input, position) => {
  for (const modifierName of modifierNames) {
    const length = modifierName.length
    const charAfterModifier = input[position + length]
    if (input.substring(position, position + length) === modifierName && (!charAfterModifier || !pf_symbolRegExp.test(charAfterModifier))) {
      const value: ModifierName = modifierName
      return [length, ['PF_Modifier', value]]
    }
  }
  return NO_MATCH
}

export const tokenizePF_InfixToken: Tokenizer<PF_InfixToken> = (input, position) => {
  if (input[position] !== '$') {
    return NO_MATCH
  }
  const nextChar = input[position + 1]
  if (nextChar && pf_symbolRegExp.test(nextChar)) {
    return NO_MATCH
  }
  return [1, ['PF_Infix']]
}

export const tokenizePF_CollectionAccessor: Tokenizer<PF_CollectionAccessorToken> = (input, position) => {
  const char = input[position]
  if (char !== '.' && char !== '#')
    return NO_MATCH

  return [1, ['PF_CollectionAccessor', char]]
}

export const tokenizePF_RegexpShorthand: Tokenizer<PF_RegexpShorthandToken> = (input, position) => {
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
      throw new LitsError(`Duplicated regexp option "${input[position]}" at position ${position}.`)
    }
    options += input[position]!
    length += 1
    position += 1
  }

  return [length, ['PF_RegexpShorthand', `#${token[1]}${options}`]]
}

// All tokenizers, order matters!
export const postfixTokenizers = [
  tokenizePF_Whitespace,
  tokenizePF_InfixToken,
  tokenizePF_Comment,
  tokenizeLeftParen,
  tokenizeRightParen,
  tokenizeLeftBracket,
  tokenizeRightBracket,
  tokenizeLeftCurly,
  tokenizeRightCurly,
  tokenizeString,
  tokenizePF_StringShorthand,
  tokenizePF_Number,
  tokenizePF_ReservedSymbol,
  tokenizePF_Symbol,
  tokenizePF_Modifier,
  tokenizePF_RegexpShorthand,
  tokenizePF_FnShorthand,
  tokenizePF_CollectionAccessor,
] as const satisfies Tokenizer<PostfixToken>[]
