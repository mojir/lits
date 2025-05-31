import { LitsError } from '../errors'
import type { ReservedSymbol } from './reservedNames'
import { type SymbolicBinaryOperator, type SymbolicOperator, isBinaryOperator } from './operators'

export const tokenTypes = [
  'LBrace',
  'LBracket',
  'RBrace',
  'RBracket',
  'LParen',
  'RParen',
  'BasePrefixedNumber',
  'DocString',
  'Error',
  'MultiLineComment',
  'Number',
  'Operator',
  'RegexpShorthand',
  'ReservedSymbol',
  'SingleLineComment',
  'String',
  'Symbol',
  'Whitespace',
] as const

export type TokenType = typeof tokenTypes[number]

const modifierNames = ['&rest', '&let', '&when', '&while'] as const
export type ModifierName = typeof modifierNames[number]

type GenericToken<T extends TokenType, V extends string = string> = [T, V] | [T, V, SourceCodeInfo]

export type ErrorToken = ['Error', string, SourceCodeInfo | undefined, string]

export type LBraceToken = GenericToken<'LBrace', '{'>
export type LBracketToken = GenericToken<'LBracket', '['>
export type LParenToken = GenericToken<'LParen', '('>
export type RBraceToken = GenericToken<'RBrace', '}'>
export type RBracketToken = GenericToken<'RBracket', ']'>
export type RParenToken = GenericToken<'RParen', ')'>

export type BasePrefixedNumberToken = GenericToken<'BasePrefixedNumber'>
export type MultiLineCommentToken = GenericToken<'MultiLineComment'>
export type NumberToken = GenericToken<'Number'>
export type OperatorToken<T extends SymbolicOperator = SymbolicOperator> = GenericToken<'Operator', T>
export type RegexpShorthandToken = GenericToken<'RegexpShorthand'>
export type ReservedSymbolToken<T extends ReservedSymbol = ReservedSymbol> = GenericToken<'ReservedSymbol', T>
export type SingleLineCommentToken = GenericToken<'SingleLineComment'>
export type StringToken = GenericToken<'String'>
export type DocStringToken = GenericToken<'DocString'>
export type SymbolToken<T extends string = string> = GenericToken<'Symbol', T>
export type WhitespaceToken = GenericToken<'Whitespace'>

export type Token =
  | LBraceToken
  | LBracketToken
  | LParenToken
  | RBraceToken
  | RBracketToken
  | RParenToken
  | BasePrefixedNumberToken
  | DocStringToken
  | ErrorToken
  | MultiLineCommentToken
  | NumberToken
  | OperatorToken
  | RegexpShorthandToken
  | ReservedSymbolToken
  | SingleLineCommentToken
  | StringToken
  | SymbolToken
  | WhitespaceToken

export type TokenDescriptor<T extends Token> = [length: number, token?: T]

export interface SourceCodeInfo {
  position: {
    line: number
    column: number
  }
  code: string
  filePath?: string
}

export function isSymbolToken<T extends string>(token: Token | undefined | undefined, symbolName?: T): token is SymbolToken<T> {
  if (token?.[0] !== 'Symbol') {
    return false
  }
  if (symbolName && token[1] !== symbolName) {
    return false
  }
  return true
}

export function assertSymbolToken<T extends string>(token: Token | undefined | undefined, symbolName?: T): asserts token is SymbolToken<T> {
  if (!isSymbolToken(token, symbolName)) {
    throwUnexpectedToken('Symbol', undefined, token)
  }
}
export function asSymbolToken<T extends string>(token: Token | undefined | undefined, symbolName?: T): SymbolToken<T> {
  assertSymbolToken(token, symbolName)
  return token
}

export function isReservedSymbolToken<T extends ReservedSymbol>(token: Token | undefined | undefined, symbolName?: T): token is ReservedSymbolToken<T> {
  if (token?.[0] !== 'ReservedSymbol') {
    return false
  }
  if (symbolName && token[1] !== symbolName) {
    return false
  }
  return true
}
export function assertReservedSymbolToken<T extends ReservedSymbol>(token: Token | undefined | undefined, symbolName?: T): asserts token is ReservedSymbolToken<T> {
  if (!isReservedSymbolToken(token, symbolName)) {
    throwUnexpectedToken('ReservedSymbol', symbolName, token)
  }
}
export function asReservedSymbolToken<T extends ReservedSymbol>(token: Token | undefined | undefined, symbolName?: T): ReservedSymbolToken<T> {
  assertReservedSymbolToken(token, symbolName)
  return token
}

export function isSingleLineCommentToken(token: Token | undefined): token is SingleLineCommentToken {
  return token?.[0] === 'SingleLineComment'
}
export function assertSingleLineCommentToken(token: Token | undefined): asserts token is SingleLineCommentToken {
  if (!isSingleLineCommentToken(token)) {
    throwUnexpectedToken('SingleLineComment', undefined, token)
  }
}
export function asSingleLineCommentToken(token: Token | undefined): SingleLineCommentToken {
  assertSingleLineCommentToken(token)
  return token
}

export function isMultiLineCommentToken(token: Token | undefined): token is MultiLineCommentToken {
  return token?.[0] === 'MultiLineComment'
}
export function assertMultiLineCommentToken(token: Token | undefined): asserts token is MultiLineCommentToken {
  if (!isMultiLineCommentToken(token)) {
    throwUnexpectedToken('MultiLineComment', undefined, token)
  }
}
export function asMultiLineCommentToken(token: Token | undefined): MultiLineCommentToken {
  assertMultiLineCommentToken(token)
  return token
}

export function isOperatorToken<T extends SymbolicOperator>(token: Token | undefined | undefined, operatorName?: T): token is OperatorToken<T> {
  if (token?.[0] !== 'Operator') {
    return false
  }
  if (operatorName && token[1] !== operatorName) {
    return false
  }
  return true
}
export function assertOperatorToken<T extends SymbolicOperator>(token: Token | undefined | undefined, operatorName?: T): asserts token is OperatorToken<T> {
  if (!isOperatorToken(token, operatorName)) {
    throwUnexpectedToken('Operator', operatorName, token)
  }
}
export function asOperatorToken<T extends SymbolicOperator>(token: Token | undefined | undefined, operatorName?: T): OperatorToken<T> {
  assertOperatorToken(token, operatorName)
  return token
}

export function isWhitespaceToken(token: Token | undefined): token is WhitespaceToken {
  return token?.[0] === 'Whitespace'
}
export function assertWhitespaceToken(token: Token | undefined): asserts token is WhitespaceToken {
  if (!isWhitespaceToken(token)) {
    throwUnexpectedToken('Whitespace', undefined, token)
  }
}
export function asWhitespaceToken(token: Token | undefined): WhitespaceToken {
  assertWhitespaceToken(token)
  return token
}

export function isNumberToken(token: Token | undefined): token is NumberToken {
  return token?.[0] === 'Number'
}
export function assertNumberToken(token: Token | undefined): asserts token is NumberToken {
  if (!isNumberToken(token)) {
    throwUnexpectedToken('Number', undefined, token)
  }
}
export function asNumberToken(token: Token | undefined): NumberToken {
  assertNumberToken(token)
  return token
}

export function isBasePrefixedNumberToken(token: Token | undefined): token is BasePrefixedNumberToken {
  return token?.[0] === 'BasePrefixedNumber'
}
export function assertBasePrefixedNumberToken(token: Token | undefined): asserts token is BasePrefixedNumberToken {
  if (!isBasePrefixedNumberToken(token)) {
    throwUnexpectedToken('BasePrefixedNumber', undefined, token)
  }
}
export function asBasePrefixedNumberToken(token: Token | undefined): BasePrefixedNumberToken {
  assertBasePrefixedNumberToken(token)
  return token
}

export function isLParenToken(token: Token | undefined): token is LParenToken {
  return token?.[0] === 'LParen'
}
export function assertLParenToken(token: Token | undefined): asserts token is LParenToken {
  if (!isLParenToken(token)) {
    throwUnexpectedToken('LParen', undefined, token)
  }
}
export function asLParenToken(token: Token | undefined): LParenToken {
  assertLParenToken(token)
  return token
}

export function isRParenToken(token: Token | undefined): token is RParenToken {
  return token?.[0] === 'RParen'
}
export function assertRParenToken(token: Token | undefined): asserts token is RParenToken {
  if (!isRParenToken(token)) {
    throwUnexpectedToken('RParen', undefined, token)
  }
}
export function asRParenToken(token: Token | undefined): RParenToken {
  assertRParenToken(token)
  return token
}

export function isLBracketToken(token: Token | undefined): token is LBracketToken {
  return token?.[0] === 'LBracket'
}
export function assertLBracketToken(token: Token | undefined): asserts token is LBracketToken {
  if (!isLBracketToken(token)) {
    throwUnexpectedToken('LBracket', undefined, token)
  }
}
export function asLBracketToken(token: Token | undefined): LBracketToken {
  assertLBracketToken(token)
  return token
}

export function isRBracketToken(token: Token | undefined): token is RBracketToken {
  return token?.[0] === 'RBracket'
}
export function assertRBracketToken(token: Token | undefined): asserts token is RBracketToken {
  if (!isRBracketToken(token)) {
    throwUnexpectedToken('RBracket', undefined, token)
  }
}
export function asRBracketToken(token: Token | undefined): RBracketToken {
  assertRBracketToken(token)
  return token
}

export function isLBraceToken(token: Token | undefined): token is LBraceToken {
  return token?.[0] === 'LBrace'
}
export function assertLBraceToken(token: Token | undefined): asserts token is LBraceToken {
  if (!isLBraceToken(token)) {
    throwUnexpectedToken('LBrace', undefined, token)
  }
}
export function asLBraceToken(token: Token | undefined): LBraceToken {
  assertLBraceToken(token)
  return token
}

export function isRBraceToken(token: Token | undefined): token is RBraceToken {
  return token?.[0] === 'RBrace'
}
export function assertRBraceToken(token: Token | undefined): asserts token is RBraceToken {
  if (!isRBraceToken(token)) {
    throwUnexpectedToken('RBrace', undefined, token)
  }
}
export function asRBraceToken(token: Token | undefined): RBraceToken {
  assertRBraceToken(token)
  return token
}

export function isStringToken(token: Token | undefined): token is StringToken {
  return token?.[0] === 'String'
}
export function assertStringToken(token: Token | undefined): asserts token is StringToken {
  if (!isStringToken(token)) {
    throwUnexpectedToken('String', undefined, token)
  }
}
export function asStringToken(token: Token | undefined): StringToken {
  assertStringToken(token)
  return token
}

export function isDocStringToken(token: Token | undefined): token is DocStringToken {
  return token?.[0] === 'DocString'
}

export function isRegexpShorthandToken(token: Token | undefined): token is RegexpShorthandToken {
  return token?.[0] === 'RegexpShorthand'
}
export function assertRegexpShorthandToken(token: Token | undefined): asserts token is RegexpShorthandToken {
  if (!isRegexpShorthandToken(token)) {
    throwUnexpectedToken('RegexpShorthand', undefined, token)
  }
}
export function asRegexpShorthandToken(token: Token | undefined): RegexpShorthandToken {
  assertRegexpShorthandToken(token)
  return token
}

export function isA_BinaryOperatorToken(token: Token | undefined): token is OperatorToken<SymbolicBinaryOperator> {
  return token?.[0] === 'Operator' && isBinaryOperator(token[1])
}
export function assertA_BinaryOperatorToken(token: Token | undefined): asserts token is OperatorToken<SymbolicBinaryOperator> {
  if (!isA_BinaryOperatorToken(token)) {
    throwUnexpectedToken('Operator', undefined, token)
  }
}
export function asA_BinaryOperatorToken(token: Token | undefined): OperatorToken<SymbolicBinaryOperator> {
  assertA_BinaryOperatorToken(token)
  return token
}

function throwUnexpectedToken(expected: TokenType, expectedValue: string | undefined, actual: Token | undefined): never {
  const actualOutput = actual ? `${actual[0]} '${actual[1]}'` : 'end of input'
  throw new LitsError(`Unexpected token: ${actualOutput}, expected ${expected}${expectedValue ? ` '${expectedValue}'` : ''}`, actual?.[2])
}
