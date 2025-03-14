import { LitsError } from '../errors'
import { getSourceCodeInfo } from '../utils/debug/getSourceCodeInfo'
import type { ValidReservedSymbol } from './reservedNames'
import { type SymbolicBinaryOperator, type SymbolicOperator, isBinaryOperator } from './operators'

export const tokenTypes = [
  'LBrace',
  'LBracket',
  'RBrace',
  'RBracket',
  'LParen',
  'RParen',
  'BasePrefixedNumber',
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

type GenericAlgebraicValueToken<T extends TokenType, V extends string = string> = [T, V] | [T, V, TokenDebugData]

export type LBraceToken = GenericAlgebraicValueToken<'LBrace', '{'>
export type LBracketToken = GenericAlgebraicValueToken<'LBracket', '['>
export type LParenToken = GenericAlgebraicValueToken<'LParen', '('>
export type RBraceToken = GenericAlgebraicValueToken<'RBrace', '}'>
export type RBracketToken = GenericAlgebraicValueToken<'RBracket', ']'>
export type RParenToken = GenericAlgebraicValueToken<'RParen', ')'>

export type BasePrefixedNumberToken = GenericAlgebraicValueToken<'BasePrefixedNumber'>
export type MultiLineCommentToken = GenericAlgebraicValueToken<'MultiLineComment'>
export type NumberToken = GenericAlgebraicValueToken<'Number'>
export type OperatorToken<T extends SymbolicOperator = SymbolicOperator> = GenericAlgebraicValueToken<'Operator', T>
export type RegexpShorthandToken = GenericAlgebraicValueToken<'RegexpShorthand'>
export type ReservedSymbolToken<T extends ValidReservedSymbol = ValidReservedSymbol> = GenericAlgebraicValueToken<'ReservedSymbol', T>
export type SingleLineCommentToken = GenericAlgebraicValueToken<'SingleLineComment'>
export type StringToken = GenericAlgebraicValueToken<'String'>
export type SymbolToken<T extends string = string> = GenericAlgebraicValueToken<'Symbol', T>
export type WhitespaceToken = GenericAlgebraicValueToken<'Whitespace'>

export type Token =
  | LBraceToken
  | LBracketToken
  | LParenToken
  | RBraceToken
  | RBracketToken
  | RParenToken
  | BasePrefixedNumberToken
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
  position?: {
    line: number
    column: number
  }
  code?: string
  filePath?: string
}

export type TokenDebugData = {
  sourceCodeInfo: SourceCodeInfo
}

function isTokenDebugData(tokenDebugData: unknown): tokenDebugData is TokenDebugData {
  return (
    typeof tokenDebugData === 'object'
    && tokenDebugData !== null
    && 'sourceCodeInfo' in tokenDebugData
  )
}

export function getTokenDebugData(token?: Token): TokenDebugData | undefined {
  const debugData = token?.at(-1)
  return isTokenDebugData(debugData) ? debugData : undefined
}

export function hasTokenDebugData(token?: Token): boolean {
  return isTokenDebugData(token?.at(-1))
}

export function addTokenDebugData(token: Token, debugData: TokenDebugData): void {
  if (isTokenDebugData(token.at(-1))) {
    throw new Error(`Token already has debug data: ${token}`)
  }
  ;(token as unknown[]).push(debugData)
}

export function isTokenType(type: string): type is TokenType {
  return typeof type === 'string' && tokenTypes.includes(type as TokenType)
}

export function isToken(token?: Token): token is Token {
  return !!token
}
export function assertToken(token?: Token): asserts token is Token {
  if (!isToken(token)) {
    throw new LitsError(`Expected token, got ${token}`, undefined)
  }
}
export function asToken(token?: Token): Token {
  assertToken(token)
  return token
}

export function isSymbolToken<T extends string>(token: Token | undefined, symbolName?: T): token is SymbolToken<T> {
  if (token?.[0] !== 'Symbol') {
    return false
  }
  if (symbolName && token[1] !== symbolName) {
    return false
  }
  return true
}

export function assertSymbolToken<T extends string>(token: Token | undefined, symbolName?: T): asserts token is SymbolToken<T> {
  if (!isSymbolToken(token, symbolName)) {
    throwUnexpectedToken('Symbol', undefined, token)
  }
}
export function asSymbolToken<T extends string>(token: Token | undefined, symbolName?: T): SymbolToken<T> {
  assertSymbolToken(token, symbolName)
  return token
}

export function isReservedSymbolToken<T extends ValidReservedSymbol>(token: Token | undefined, symbolName?: T): token is ReservedSymbolToken<T> {
  if (token?.[0] !== 'ReservedSymbol') {
    return false
  }
  if (symbolName && token[1] !== symbolName) {
    return false
  }
  return true
}
export function assertReservedSymbolToken<T extends ValidReservedSymbol>(token: Token | undefined, symbolName?: T): asserts token is ReservedSymbolToken<T> {
  if (!isReservedSymbolToken(token, symbolName)) {
    throwUnexpectedToken('ReservedSymbol', symbolName, token)
  }
}
export function asReservedSymbolToken<T extends ValidReservedSymbol>(token: Token | undefined, symbolName?: T): ReservedSymbolToken<T> {
  assertReservedSymbolToken(token, symbolName)
  return token
}

export function isA_CommentToken(token: Token | undefined): token is SingleLineCommentToken {
  return token?.[0] === 'SingleLineComment'
}
export function assertA_CommentToken(token: Token | undefined): asserts token is SingleLineCommentToken {
  if (!isA_CommentToken(token)) {
    throwUnexpectedToken('SingleLineComment', undefined, token)
  }
}
export function asA_CommentToken(token: Token | undefined): SingleLineCommentToken {
  assertA_CommentToken(token)
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

export function isOperatorToken<T extends SymbolicOperator>(token: Token | undefined, operatorName?: T): token is OperatorToken<T> {
  if (token?.[0] !== 'Operator') {
    return false
  }
  if (operatorName && token[1] !== operatorName) {
    return false
  }
  return true
}
export function assertOperatorToken<T extends SymbolicOperator>(token: Token | undefined, operatorName?: T): asserts token is OperatorToken<T> {
  if (!isOperatorToken(token, operatorName)) {
    if (operatorName) {
      throw new LitsError(`Unexpected token: ${token}, expected operator ${operatorName}`, undefined)
    }
    throwUnexpectedToken('Operator', operatorName, token)
  }
}
export function asOperatorToken<T extends SymbolicOperator>(token: Token | undefined, operatorName?: T): OperatorToken<T> {
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

export function isLParenToken(token?: Token): token is LParenToken {
  return token?.[0] === 'LParen'
}
export function assertLParenToken(token?: Token): asserts token is LParenToken {
  if (!isLParenToken(token)) {
    throwUnexpectedToken('LParen', undefined, token)
  }
}
export function asLParenToken(token?: Token): LParenToken {
  assertLParenToken(token)
  return token
}

export function isRParenToken(token?: Token): token is RParenToken {
  return token?.[0] === 'RParen'
}
export function assertRParenToken(token?: Token): asserts token is RParenToken {
  if (!isRParenToken(token)) {
    throwUnexpectedToken('RParen', undefined, token)
  }
}
export function asRParenToken(token?: Token): RParenToken {
  assertRParenToken(token)
  return token
}

export function isLBracketToken(token?: Token): token is LBracketToken {
  return token?.[0] === 'LBracket'
}
export function assertLBracketToken(token?: Token): asserts token is LBracketToken {
  if (!isLBracketToken(token)) {
    throwUnexpectedToken('LBracket', undefined, token)
  }
}
export function asLBracketToken(token?: Token): LBracketToken {
  assertLBracketToken(token)
  return token
}

export function isRBracketToken(token?: Token): token is RBracketToken {
  return token?.[0] === 'RBracket'
}
export function assertRBracketToken(token?: Token): asserts token is RBracketToken {
  if (!isRBracketToken(token)) {
    throwUnexpectedToken('RBracket', undefined, token)
  }
}
export function asRBracketToken(token?: Token): RBracketToken {
  assertRBracketToken(token)
  return token
}

export function isLBraceToken(token?: Token): token is LBraceToken {
  return token?.[0] === 'LBrace'
}
export function assertLBraceToken(token?: Token): asserts token is LBraceToken {
  if (!isLBraceToken(token)) {
    throwUnexpectedToken('LBrace', undefined, token)
  }
}
export function asLBraceToken(token?: Token): LBraceToken {
  assertLBraceToken(token)
  return token
}

export function isRBraceToken(token?: Token): token is RBraceToken {
  return token?.[0] === 'RBrace'
}
export function assertRBraceToken(token?: Token): asserts token is RBraceToken {
  if (!isRBraceToken(token)) {
    throwUnexpectedToken('RBrace', undefined, token)
  }
}
export function asRBraceToken(token?: Token): RBraceToken {
  assertRBraceToken(token)
  return token
}

export function isStringToken(token?: Token): token is StringToken {
  return token?.[0] === 'String'
}
export function assertStringToken(token?: Token): asserts token is StringToken {
  if (!isStringToken(token)) {
    throwUnexpectedToken('String', undefined, token)
  }
}
export function asStringToken(token?: Token): StringToken {
  assertStringToken(token)
  return token
}

export function isRegexpShorthandToken(token?: Token): token is RegexpShorthandToken {
  return token?.[0] === 'RegexpShorthand'
}
export function assertRegexpShorthandToken(token?: Token): asserts token is RegexpShorthandToken {
  if (!isRegexpShorthandToken(token)) {
    throwUnexpectedToken('RegexpShorthand', undefined, token)
  }
}
export function asRegexpShorthandToken(token?: Token): RegexpShorthandToken {
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

function throwUnexpectedToken(expected: TokenType, expectedValue: string | undefined, actual?: Token): never {
  if (actual === undefined) {
    throw new LitsError(`Unexpected end of input, expected ${expected}${expectedValue ? ` '${expectedValue}'` : ''}`, undefined)
  }
  const actualOutput = `${actual[0]}${actual[1] ? ` '${actual[1]}'` : ''}`
  throw new LitsError(`Unexpected token: ${actualOutput}, expected ${expected}${expectedValue ? ` '${expectedValue}'` : ''}`, getSourceCodeInfo(actual))
}
