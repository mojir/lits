import { LitsError } from '../../errors'
import {
  NO_MATCH,
  isNoMatch,
  tokenizeLeftBracket,
  tokenizeLeftCurly,
  tokenizeLeftParen,
  tokenizeNumber,
  tokenizeRightBracket,
  tokenizeRightCurly,
  tokenizeRightParen,
  tokenizeString,
} from '../common/tokenizers'
import type { Tokenizer } from '../interface'
import type { ModifierName, PF_CollectionAccessorToken, PF_CommentToken, PF_FnShorthandToken, PF_InfixToken, PF_ModifierToken, PF_RegexpShorthandToken, PF_ReservedSymbolToken, PF_StringShorthandToken, PF_WhitespaceToken } from '../Token'
import { asPF_SymbolToken, modifierNames } from '../Token'
import { PF_symbolRegExp as pf_symbolRegExp, tokenizePF_Symbol } from '../tokenizePF_Symbol'
import { postfixReservedNamesRecord } from './postfixReservedNames'

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
  tokenizeNumber,
  tokenizePF_ReservedSymbol,
  tokenizePF_Symbol,
  tokenizePF_Modifier,
  tokenizePF_RegexpShorthand,
  tokenizePF_FnShorthand,
  tokenizePF_CollectionAccessor,
] as const
