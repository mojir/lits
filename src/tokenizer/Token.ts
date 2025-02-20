import { LitsError } from '../errors'
import type { SourceCodeInfo } from './interface'

export const modifierNames = ['&', '&let', '&when', '&while'] as const
export type ModifierName = typeof modifierNames[number]

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

export function addTokenDebugData(token: Token, debugData: TokenDebugData): void {
  if (isTokenDebugData(token.at(-1))) {
    throw new Error(`Token already has debug data: ${token}`)
  }
  ;(token as unknown[]).push(debugData)
}

export const commonSimpleTokenTypes = [
  'LBrace',
  'LBracket',
  'LParen',
  'RBrace',
  'RBracket',
  'RParen',
] as const

const infixOnlySimpleTokenTypes = [
  'IF_Postfix',
] as const satisfies `IF_${string}`[]

export const infixSimpleTokenTypes = [
  ...commonSimpleTokenTypes,
  ...infixOnlySimpleTokenTypes,
] as const

const postfixOnlySimpleTokenTypes = [
  'PF_FnShorthand',
  'PF_Infix',
] as const satisfies `PF_${string}`[]

export const postfixSimpleTokenTypes = [
  ...commonSimpleTokenTypes,
  ...postfixOnlySimpleTokenTypes,
] as const

export const commomValueTokenTypes = [
  'Number',
  'String',
] as const

const infixOnlyValueTokenTypes = [
  'IF_Whitespace',
  'IF_Operator',
  'IF_Symbol',
  'IF_ReservedSymbol',
  'IF_SingleLineComment',
  'IF_MultiLineComment',
] as const satisfies `IF_${string}`[]

export const infixValueTokenTypes = [
  ...commomValueTokenTypes,
  ...infixOnlyValueTokenTypes,
] as const

const postfixOnlyValueTokenTypes = [
  'PF_Modifier',
  'PF_StringShorthand',
  'PF_Symbol',
  'PF_ReservedSymbol',
  'PF_RegexpShorthand',
  'PF_CollectionAccessor',
  'PF_Comment',
  'PF_Whitespace',
] as const satisfies `PF_${string}`[]

export const postfixValueTokenTypes = [
  ...commomValueTokenTypes,
  ...postfixOnlyValueTokenTypes,
] as const

export const infixTokenTypes = [
  ...infixSimpleTokenTypes,
  ...infixValueTokenTypes,
] as const

export const postfixTokenTypes = [
  ...postfixSimpleTokenTypes,
  ...postfixValueTokenTypes,
] as const

export const simpleTokenTypes = [
  ...commonSimpleTokenTypes,
  ...infixOnlySimpleTokenTypes,
  ...postfixOnlySimpleTokenTypes,
] as const

export const valueTokenTypes = [
  ...commomValueTokenTypes,
  ...infixOnlyValueTokenTypes,
  ...postfixOnlyValueTokenTypes,
] as const

export const tokenTypes = [
  ...commonSimpleTokenTypes,
  ...infixOnlySimpleTokenTypes,
  ...postfixOnlySimpleTokenTypes,
  ...commomValueTokenTypes,
  ...infixOnlyValueTokenTypes,
  ...postfixOnlyValueTokenTypes,
] as const

export const infixOperators = [
  '**', // exponentiation

  '*', // multiplication
  '/', // division
  '%', // remainder

  '+', // addition
  '-', // subtraction

  '<<', // left shift
  '>>', // signed right shift
  '>>>', // unsigned right shift

  '<', // less than
  '<=', // less than or equal
  '>', // greater than
  '>=', // greater than or equal

  '==', // equal
  '!=', // not equal

  '&', // bitwise AND
  '^', // bitwise XOR
  '|', // bitwise OR

  '&&', // logical AND
  '||', // logical OR
  '??', // nullish coalescing

] as const

export type InfixOperator = typeof infixOperators[number]

export function isInfixOperator(operator: string): operator is InfixOperator {
  return infixOperators.includes(operator as InfixOperator)
}
export function assertInfixOperator(operator: string): asserts operator is InfixOperator {
  if (!isInfixOperator(operator)) {
    throw new LitsError(`Expected infix operator, got ${operator}`)
  }
}
export function asInfixOperator(operator: string): InfixOperator {
  assertInfixOperator(operator)
  return operator
}

type CommonSimpleTokenType = typeof commonSimpleTokenTypes[number]
type InfixSimpleTokenType = typeof infixSimpleTokenTypes[number]
type PostfixSimpleTokenType = typeof postfixSimpleTokenTypes[number]

type CommonValueTokenType = typeof commomValueTokenTypes[number]
type InfixValueTokenType = typeof infixValueTokenTypes[number]
type PostfixValueTokenType = typeof postfixValueTokenTypes[number]

type SimpleTokenType = InfixSimpleTokenType | PostfixSimpleTokenType
type ValueTokenType = InfixValueTokenType | PostfixValueTokenType

export type TokenType = typeof tokenTypes[number]

type GenericCommonSimpleToken<T extends CommonSimpleTokenType> = [T] | [T, TokenDebugData]
type GenericInfixSimpleToken<T extends Exclude<InfixSimpleTokenType, CommonSimpleTokenType>> = [T] | [T, TokenDebugData]
type GenericPostfixSimpleToken<T extends Exclude<PostfixSimpleTokenType, CommonSimpleTokenType>> = [T] | [T, TokenDebugData]

type GenericCommonValueToken<T extends CommonValueTokenType, V extends string = string> = [T, V] | [T, V, TokenDebugData]
type GenericInfixValueToken<T extends Exclude<InfixValueTokenType, CommonValueTokenType>, V extends string = string> = [T, V] | [T, V, TokenDebugData]
type GenericPostfixValueToken<T extends Exclude<PostfixValueTokenType, CommonValueTokenType>, V extends string = string> = [T, V] | [T, V, TokenDebugData]

export type LParenToken = GenericCommonSimpleToken<'LParen'>
export type RParenToken = GenericCommonSimpleToken<'RParen'>
export type LBracketToken = GenericCommonSimpleToken<'LBracket'>
export type RBracketToken = GenericCommonSimpleToken<'RBracket'>
export type LBraceToken = GenericCommonSimpleToken<'LBrace'>
export type RBraceToken = GenericCommonSimpleToken<'RBrace'>

export type IF_PostfixToken = GenericInfixSimpleToken<'IF_Postfix'>

export type PF_FnShorthandToken = GenericPostfixSimpleToken<'PF_FnShorthand'>
export type PF_InfixToken = GenericPostfixSimpleToken<'PF_Infix'>

export type NumberToken = GenericCommonValueToken<'Number'>
export type StringToken = GenericCommonValueToken<'String'>

export type IF_WhitespaceToken = GenericInfixValueToken<'IF_Whitespace'>
export type IF_OperatorToken = GenericInfixValueToken<'IF_Operator', InfixOperator>
export type IF_SymbolToken = GenericInfixValueToken<'IF_Symbol'>
export type IF_ReservedSymbolToken = GenericInfixValueToken<'IF_ReservedSymbol'>
export type IF_SingleLineCommentToken = GenericInfixValueToken<'IF_SingleLineComment'>
export type IF_MultiLineCommentToken = GenericInfixValueToken<'IF_MultiLineComment'>

export type PF_ModifierToken = GenericPostfixValueToken<'PF_Modifier', ModifierName>
export type PF_StringShorthandToken = GenericPostfixValueToken<'PF_StringShorthand'>
export type PF_SymbolToken = GenericPostfixValueToken<'PF_Symbol'>
export type PF_ReservedSymbolToken = GenericPostfixValueToken<'PF_ReservedSymbol'>
export type PF_RegexpShorthandToken = GenericPostfixValueToken<'PF_RegexpShorthand'>
export type PF_CollectionAccessorToken = GenericPostfixValueToken<'PF_CollectionAccessor', '.' | '#'>
export type PF_CommentToken = GenericPostfixValueToken<'PF_Comment'>
export type PF_WhitespaceToken = GenericPostfixValueToken<'PF_Whitespace'>

export type SimpleToken =
  | LParenToken
  | RParenToken
  | LBracketToken
  | RBracketToken
  | LBraceToken
  | RBraceToken
  | IF_PostfixToken
  | PF_FnShorthandToken
  | PF_InfixToken

export type ValueToken =
  | NumberToken
  | StringToken
  | IF_WhitespaceToken
  | IF_OperatorToken
  | IF_SymbolToken
  | IF_ReservedSymbolToken
  | IF_SingleLineCommentToken
  | IF_MultiLineCommentToken
  | PF_ModifierToken
  | PF_StringShorthandToken
  | PF_SymbolToken
  | PF_ReservedSymbolToken
  | PF_RegexpShorthandToken
  | PF_CollectionAccessorToken
  | PF_CommentToken
  | PF_WhitespaceToken

export type Token =
  | SimpleToken
  | ValueToken

export function isTokenType(type: string): type is TokenType {
  return typeof type === 'string' && tokenTypes.includes(type as TokenType)
}

export function isToken(token?: Token): token is Token {
  return !!token
}
export function assertToken(token?: Token): asserts token is Token {
  if (!isToken(token)) {
    throw new LitsError(`Expected token, got ${token}`)
  }
}
export function asToken(token?: Token): Token {
  assertToken(token)
  return token
}

export function isSimpleToken(token?: Token): token is SimpleToken {
  return isToken(token) && simpleTokenTypes.includes(token[0] as SimpleTokenType)
}

export function assertSimpleToken(token?: Token): asserts token is SimpleToken {
  if (!isSimpleToken(token)) {
    throw new LitsError(`Expected simple token, got ${token}`)
  }
}

export function asSimpleToken(token?: Token): SimpleToken {
  assertSimpleToken(token)
  return token
}

export function isValueToken(token?: Token): token is ValueToken {
  return isToken(token) && valueTokenTypes.includes(token[0] as ValueTokenType)
}

export function assertValueToken(token?: Token): asserts token is ValueToken {
  if (!isValueToken(token)) {
    throw new LitsError(`Expected value token, got ${token}`)
  }
}

export function asValueToken(token?: Token): ValueToken {
  assertValueToken(token)
  return token
}

export function isLParenToken(token?: Token): token is LParenToken {
  return token?.[0] === 'LParen'
}
export function assertLParenToken(token?: Token): asserts token is LParenToken {
  if (!isLParenToken(token)) {
    throwUnexpectedToken('LParen', token)
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
    throwUnexpectedToken('RParen', token)
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
    throwUnexpectedToken('LBracket', token)
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
    throwUnexpectedToken('RBracket', token)
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
    throwUnexpectedToken('LBrace', token)
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
    throwUnexpectedToken('RBrace', token)
  }
}
export function asRBraceToken(token?: Token): RBraceToken {
  assertRBraceToken(token)
  return token
}

export function isNumberToken(token?: Token): token is NumberToken {
  return token?.[0] === 'Number'
}
export function assertNumberToken(token?: Token): asserts token is NumberToken {
  if (!isNumberToken(token)) {
    throwUnexpectedToken('Number', token)
  }
}
export function asNumberToken(token?: Token): NumberToken {
  assertNumberToken(token)
  return token
}

export function isStringToken(token?: Token): token is StringToken {
  return token?.[0] === 'String'
}
export function assertStringToken(token?: Token): asserts token is StringToken {
  if (!isStringToken(token)) {
    throwUnexpectedToken('String', token)
  }
}
export function asStringToken(token?: Token): StringToken {
  assertStringToken(token)
  return token
}

export function isPF_StringShorthandToken(token?: Token): token is PF_StringShorthandToken {
  return token?.[0] === 'PF_StringShorthand'
}
export function assertPF_StringShorthandToken(token?: Token): asserts token is PF_StringShorthandToken {
  if (!isPF_StringShorthandToken(token)) {
    throwUnexpectedToken('PF_StringShorthand', token)
  }
}
export function asPF_StringShorthandToken(token?: Token): PF_StringShorthandToken {
  assertPF_StringShorthandToken(token)
  return token
}

export function isPF_SymbolToken(token?: Token): token is PF_SymbolToken {
  return token?.[0] === 'PF_Symbol'
}
export function assertPF_SymbolToken(token?: Token): asserts token is PF_SymbolToken {
  if (!isPF_SymbolToken(token)) {
    throwUnexpectedToken('PF_Symbol', token)
  }
}
export function asPF_SymbolToken(token?: Token): PF_SymbolToken {
  assertPF_SymbolToken(token)
  return token
}

export function isPF_ReservedSymbolToken(token?: Token): token is PF_ReservedSymbolToken {
  return token?.[0] === 'PF_ReservedSymbol'
}
export function assertPF_ReservedSymbolToken(token?: Token): asserts token is PF_ReservedSymbolToken {
  if (!isPF_ReservedSymbolToken(token)) {
    throwUnexpectedToken('PF_ReservedSymbol', token)
  }
}
export function asPF_ReservedSymbolToken(token?: Token): PF_ReservedSymbolToken {
  assertPF_ReservedSymbolToken(token)
  return token
}

export function isIF_SymbolToken(token?: Token): token is IF_SymbolToken {
  return token?.[0] === 'IF_Symbol'
}
export function assertIF_SymbolToken(token?: Token): asserts token is IF_SymbolToken {
  if (!isIF_SymbolToken(token)) {
    throwUnexpectedToken('IF_Symbol', token)
  }
}
export function asIF_SymbolToken(token?: Token): IF_SymbolToken {
  assertIF_SymbolToken(token)
  return token
}

export function isIF_ReservedSymbolToken(token?: Token): token is IF_ReservedSymbolToken {
  return token?.[0] === 'IF_ReservedSymbol'
}
export function assertIF_ReservedSymbolToken(token?: Token): asserts token is IF_ReservedSymbolToken {
  if (!isIF_ReservedSymbolToken(token)) {
    throwUnexpectedToken('IF_ReservedSymbol', token)
  }
}
export function asIF_ReservedSymbolToken(token?: Token): IF_ReservedSymbolToken {
  assertIF_ReservedSymbolToken(token)
  return token
}

export function isPF_ModifierToken(token?: Token): token is PF_ModifierToken {
  return token?.[0] === 'PF_Modifier'
}
export function assertPF_ModifierToken(token?: Token): asserts token is PF_ModifierToken {
  if (!isPF_ModifierToken(token)) {
    throwUnexpectedToken('PF_Modifier', token)
  }
}
export function asPF_ModifierToken(token?: Token): PF_ModifierToken {
  assertPF_ModifierToken(token)
  return token
}

export function isPF_RegexpShorthandToken(token?: Token): token is PF_RegexpShorthandToken {
  return token?.[0] === 'PF_RegexpShorthand'
}
export function assertPF_RegexpShorthandToken(token?: Token): asserts token is PF_RegexpShorthandToken {
  if (!isPF_RegexpShorthandToken(token)) {
    throwUnexpectedToken('PF_RegexpShorthand', token)
  }
}
export function asPF_RegexpShorthandToken(token?: Token): PF_RegexpShorthandToken {
  assertPF_RegexpShorthandToken(token)
  return token
}

export function isPF_FnShorthandToken(token?: Token): token is PF_FnShorthandToken {
  return token?.[0] === 'PF_FnShorthand'
}
export function assertPF_FnShorthandToken(token?: Token): asserts token is PF_FnShorthandToken {
  if (!isPF_FnShorthandToken(token)) {
    throwUnexpectedToken('PF_FnShorthand', token)
  }
}
export function asPF_FnShorthandToken(token?: Token): PF_FnShorthandToken {
  assertPF_FnShorthandToken(token)
  return token
}

export function isPF_CollectionAccessorToken(token?: Token): token is PF_CollectionAccessorToken {
  return token?.[0] === 'PF_CollectionAccessor'
}
export function assertPF_CollectionAccessorToken(token?: Token): asserts token is PF_CollectionAccessorToken {
  if (!isPF_CollectionAccessorToken(token)) {
    throwUnexpectedToken('PF_CollectionAccessor', token)
  }
}
export function asPF_CollectionAccessorToken(token?: Token): PF_CollectionAccessorToken {
  assertPF_CollectionAccessorToken(token)
  return token
}

export function isPF_CommentToken(token?: Token): token is PF_CommentToken {
  return token?.[0] === 'PF_Comment'
}
export function assertPF_CommentToken(token?: Token): asserts token is PF_CommentToken {
  if (!isPF_CommentToken(token)) {
    throwUnexpectedToken('PF_Comment', token)
  }
}
export function asPF_CommentToken(token?: Token): PF_CommentToken {
  assertPF_CommentToken(token)
  return token
}

export function isIF_CommentToken(token?: Token): token is IF_SingleLineCommentToken {
  return token?.[0] === 'IF_SingleLineComment'
}
export function assertIF_CommentToken(token?: Token): asserts token is IF_SingleLineCommentToken {
  if (!isIF_CommentToken(token)) {
    throwUnexpectedToken('IF_SingleLineComment', token)
  }
}
export function asIF_CommentToken(token?: Token): IF_SingleLineCommentToken {
  assertIF_CommentToken(token)
  return token
}

export function isIF_MultiLineCommentToken(token?: Token): token is IF_MultiLineCommentToken {
  return token?.[0] === 'IF_MultiLineComment'
}
export function assertIF_MultiLineCommentToken(token?: Token): asserts token is IF_MultiLineCommentToken {
  if (!isIF_MultiLineCommentToken(token)) {
    throwUnexpectedToken('IF_MultiLineComment', token)
  }
}
export function asIF_MultiLineCommentToken(token?: Token): IF_MultiLineCommentToken {
  assertIF_MultiLineCommentToken(token)
  return token
}

export function isPF_InfixToken(token?: Token): token is PF_InfixToken {
  return token?.[0] === 'PF_Infix'
}
export function assertPF_InfixToken(token?: Token): asserts token is PF_InfixToken {
  if (!isPF_InfixToken(token)) {
    throwUnexpectedToken('PF_Infix', token)
  }
}
export function asPF_InfixToken(token?: Token): PF_InfixToken {
  assertPF_InfixToken(token)
  return token
}

export function isIF_PostfixToken(token?: Token): token is IF_PostfixToken {
  return token?.[0] === 'IF_Postfix'
}
export function assertIF_PostfixToken(token?: Token): asserts token is IF_PostfixToken {
  if (!isIF_PostfixToken(token)) {
    throwUnexpectedToken('IF_Postfix', token)
  }
}
export function asIF_PostfixToken(token?: Token): IF_PostfixToken {
  assertIF_PostfixToken(token)
  return token
}

export function isIF_OperatorToken(token?: Token): token is IF_OperatorToken {
  return token?.[0] === 'IF_Operator'
}
export function assertIF_OperatorToken(token?: Token): asserts token is IF_OperatorToken {
  if (!isIF_OperatorToken(token)) {
    throwUnexpectedToken('IF_Operator', token)
  }
}
export function asIF_OperatorToken(token?: Token): IF_OperatorToken {
  assertIF_OperatorToken(token)
  return token
}

export function isIF_WhitespaceToken(token?: Token): token is IF_WhitespaceToken {
  return token?.[0] === 'IF_Whitespace'
}
export function assertIF_WhitespaceToken(token?: Token): asserts token is IF_WhitespaceToken {
  if (!isIF_WhitespaceToken(token)) {
    throwUnexpectedToken('IF_Whitespace', token)
  }
}
export function asIF_WhitespaceToken(token?: Token): IF_WhitespaceToken {
  assertIF_WhitespaceToken(token)
  return token
}

export function isPF_WhitespaceToken(token?: Token): token is PF_WhitespaceToken {
  return token?.[0] === 'PF_Whitespace'
}
export function assertPF_WhitespaceToken(token?: Token): asserts token is PF_WhitespaceToken {
  if (!isPF_WhitespaceToken(token)) {
    throwUnexpectedToken('PF_Whitespace', token)
  }
}
export function asPF_WhitespaceToken(token?: Token): PF_WhitespaceToken {
  assertPF_WhitespaceToken(token)
  return token
}

function throwUnexpectedToken(expected: TokenType, actual?: Token): never {
  throw new LitsError(`Unexpected token: ${actual}, expected ${expected}`)
}
