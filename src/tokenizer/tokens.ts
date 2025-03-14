import { LitsError } from '../errors'
import type { ValidReservedSymbol } from './reservedNames'
import type { SymbolicOperator } from './operators'
import type { TokenDebugData } from './utils'
import { throwUnexpectedToken } from './utils'
// import type { PolishOnlyToken } from './polish/polishTokens'
// import { polishOnlyTokenTypes } from './polish/polishTokens'

export const tokenTypes = [
  'LBrace',
  'LBracket',
  'LParen',
  'RBrace',
  'RBracket',
  'RParen',
  'String',
  'RegexpShorthand',
  'A_Whitespace',
  'A_Operator',
  'A_Symbol',
  'A_ReservedSymbol',
  'A_SingleLineComment',
  'A_MultiLineComment',
  'A_Number',
  'A_BasePrefixedNumber',
] as const

export type TokenType = typeof tokenTypes[number]

const modifierNames = ['&rest', '&let', '&when', '&while'] as const
export type ModifierName = typeof modifierNames[number]

type GenericAlgebraicValueToken<T extends TokenType, V extends string = string> = [T, V] | [T, V, TokenDebugData]

export type A_WhitespaceToken = GenericAlgebraicValueToken<'A_Whitespace'>
export type A_NumberToken = GenericAlgebraicValueToken<'A_Number'>
export type A_BasePrefixedNumberToken = GenericAlgebraicValueToken<'A_BasePrefixedNumber'>
export type A_OperatorToken<T extends SymbolicOperator = SymbolicOperator> = GenericAlgebraicValueToken<'A_Operator', T>
export type A_SymbolToken<T extends string = string> = GenericAlgebraicValueToken<'A_Symbol', T>
export type A_ReservedSymbolToken<T extends ValidReservedSymbol = ValidReservedSymbol> = GenericAlgebraicValueToken<'A_ReservedSymbol', T>
export type A_SingleLineCommentToken = GenericAlgebraicValueToken<'A_SingleLineComment'>
export type A_MultiLineCommentToken = GenericAlgebraicValueToken<'A_MultiLineComment'>
export type LParenToken = GenericAlgebraicValueToken<'LParen', '('>
export type RParenToken = GenericAlgebraicValueToken<'RParen', ')'>
export type LBracketToken = GenericAlgebraicValueToken<'LBracket', '['>
export type RBracketToken = GenericAlgebraicValueToken<'RBracket', ']'>
export type LBraceToken = GenericAlgebraicValueToken<'LBrace', '{'>
export type RBraceToken = GenericAlgebraicValueToken<'RBrace', '}'>
export type StringToken = GenericAlgebraicValueToken<'String'>
export type RegexpShorthandToken = GenericAlgebraicValueToken<'RegexpShorthand'>

export type Token =
  | LParenToken
  | RParenToken
  | LBracketToken
  | RBracketToken
  | LBraceToken
  | RBraceToken
  | StringToken
  | RegexpShorthandToken
  | A_WhitespaceToken
  | A_NumberToken
  | A_BasePrefixedNumberToken
  | A_OperatorToken
  | A_SymbolToken
  | A_ReservedSymbolToken
  | A_SingleLineCommentToken
  | A_MultiLineCommentToken

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

export function isA_SymbolToken<T extends string>(token: Token | undefined, symbolName?: T): token is A_SymbolToken<T> {
  if (token?.[0] !== 'A_Symbol') {
    return false
  }
  if (symbolName && token[1] !== symbolName) {
    return false
  }
  return true
}

export function assertA_SymbolToken<T extends string>(token: Token | undefined, symbolName?: T): asserts token is A_SymbolToken<T> {
  if (!isA_SymbolToken(token, symbolName)) {
    throwUnexpectedToken('A_Symbol', undefined, token)
  }
}
export function asA_SymbolToken<T extends string>(token: Token | undefined, symbolName?: T): A_SymbolToken<T> {
  assertA_SymbolToken(token, symbolName)
  return token
}

export function isA_ReservedSymbolToken<T extends ValidReservedSymbol>(token: Token | undefined, symbolName?: T): token is A_ReservedSymbolToken<T> {
  if (token?.[0] !== 'A_ReservedSymbol') {
    return false
  }
  if (symbolName && token[1] !== symbolName) {
    return false
  }
  return true
}
export function assertA_ReservedSymbolToken<T extends ValidReservedSymbol>(token: Token | undefined, symbolName?: T): asserts token is A_ReservedSymbolToken<T> {
  if (!isA_ReservedSymbolToken(token, symbolName)) {
    throwUnexpectedToken('A_ReservedSymbol', symbolName, token)
  }
}
export function asA_ReservedSymbolToken<T extends ValidReservedSymbol>(token: Token | undefined, symbolName?: T): A_ReservedSymbolToken<T> {
  assertA_ReservedSymbolToken(token, symbolName)
  return token
}

export function isA_CommentToken(token: Token | undefined): token is A_SingleLineCommentToken {
  return token?.[0] === 'A_SingleLineComment'
}
export function assertA_CommentToken(token: Token | undefined): asserts token is A_SingleLineCommentToken {
  if (!isA_CommentToken(token)) {
    throwUnexpectedToken('A_SingleLineComment', undefined, token)
  }
}
export function asA_CommentToken(token: Token | undefined): A_SingleLineCommentToken {
  assertA_CommentToken(token)
  return token
}

export function isA_MultiLineCommentToken(token: Token | undefined): token is A_MultiLineCommentToken {
  return token?.[0] === 'A_MultiLineComment'
}
export function assertA_MultiLineCommentToken(token: Token | undefined): asserts token is A_MultiLineCommentToken {
  if (!isA_MultiLineCommentToken(token)) {
    throwUnexpectedToken('A_MultiLineComment', undefined, token)
  }
}
export function asA_MultiLineCommentToken(token: Token | undefined): A_MultiLineCommentToken {
  assertA_MultiLineCommentToken(token)
  return token
}

export function isA_OperatorToken<T extends SymbolicOperator>(token: Token | undefined, operatorName?: T): token is A_OperatorToken<T> {
  if (token?.[0] !== 'A_Operator') {
    return false
  }
  if (operatorName && token[1] !== operatorName) {
    return false
  }
  return true
}
export function assertA_OperatorToken<T extends SymbolicOperator>(token: Token | undefined, operatorName?: T): asserts token is A_OperatorToken<T> {
  if (!isA_OperatorToken(token, operatorName)) {
    if (operatorName) {
      throw new LitsError(`Unexpected token: ${token}, expected operator ${operatorName}`, undefined)
    }
    throwUnexpectedToken('A_Operator', operatorName, token)
  }
}
export function asA_OperatorToken<T extends SymbolicOperator>(token: Token | undefined, operatorName?: T): A_OperatorToken<T> {
  assertA_OperatorToken(token, operatorName)
  return token
}

export function isA_WhitespaceToken(token: Token | undefined): token is A_WhitespaceToken {
  return token?.[0] === 'A_Whitespace'
}
export function assertA_WhitespaceToken(token: Token | undefined): asserts token is A_WhitespaceToken {
  if (!isA_WhitespaceToken(token)) {
    throwUnexpectedToken('A_Whitespace', undefined, token)
  }
}
export function asA_WhitespaceToken(token: Token | undefined): A_WhitespaceToken {
  assertA_WhitespaceToken(token)
  return token
}

export function isA_NumberToken(token: Token | undefined): token is A_NumberToken {
  return token?.[0] === 'A_Number'
}
export function assertA_NumberToken(token: Token | undefined): asserts token is A_NumberToken {
  if (!isA_NumberToken(token)) {
    throwUnexpectedToken('A_Number', undefined, token)
  }
}
export function asA_NumberToken(token: Token | undefined): A_NumberToken {
  assertA_NumberToken(token)
  return token
}

export function isA_BasePrefixedNumberToken(token: Token | undefined): token is A_BasePrefixedNumberToken {
  return token?.[0] === 'A_BasePrefixedNumber'
}
export function assertA_BasePrefixedNumberToken(token: Token | undefined): asserts token is A_BasePrefixedNumberToken {
  if (!isA_BasePrefixedNumberToken(token)) {
    throwUnexpectedToken('A_BasePrefixedNumber', undefined, token)
  }
}
export function asA_BasePrefixedNumberToken(token: Token | undefined): A_BasePrefixedNumberToken {
  assertA_BasePrefixedNumberToken(token)
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
