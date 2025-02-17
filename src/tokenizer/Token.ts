import { LitsError } from '../errors'
import type { SourceCodeInfo } from './interface'

export const modifierNames = ['&', '&let', '&when', '&while'] as const
export type ModifierName = typeof modifierNames[number]

export type TokenDebugData = {
  sourceCodeInfo: SourceCodeInfo
  metaTokens: MetaTokens
}

function isTokenDebugData(tokenDebugData: unknown): tokenDebugData is TokenDebugData {
  return (
    typeof tokenDebugData === 'object'
    && tokenDebugData !== null
    && 'sourceCodeInfo' in tokenDebugData
    && 'metaTokens' in tokenDebugData
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

export const simpleTokenTypes = ['LParen', 'RParen', 'LBracket', 'RBracket', 'LBrace', 'RBrace', 'FnShorthand', 'NewLine', 'Infix', 'Postfix'] as const
export const valueTokenTypes = ['Number', 'String', 'StringShorthand', 'Symbol', 'ReservedSymbol', 'Modifier', 'CollectionAccessor', 'Comment', 'InfixOperator', 'RegexpShorthand'] as const
export const tokenTypes = [...simpleTokenTypes, ...valueTokenTypes] as const

export type SimpleTokenType = typeof simpleTokenTypes[number]
export type ValueTokenType = typeof valueTokenTypes[number]

export type TokenType = typeof tokenTypes[number]

type GenericSimpleToken<T extends SimpleTokenType> = [T] | [T, TokenDebugData]
type GenericValueToken<T extends ValueTokenType, V extends string = string> = [T, V] | [T, V, TokenDebugData]

export type LParenToken = GenericSimpleToken<'LParen'>
export type RParenToken = GenericSimpleToken<'RParen'>
export type LBracketToken = GenericSimpleToken<'LBracket'>
export type RBracketToken = GenericSimpleToken<'RBracket'>
export type LBraceToken = GenericSimpleToken<'LBrace'>
export type RBraceToken = GenericSimpleToken<'RBrace'>
export type FnShorthandToken = GenericSimpleToken<'FnShorthand'>
export type NewLineToken = GenericSimpleToken<'NewLine'>
export type InfixToken = GenericSimpleToken<'Infix'>
export type PostfixToken = GenericSimpleToken<'Postfix'>
export type NumberToken = GenericValueToken<'Number'>
export type StringToken = GenericValueToken<'String'>
export type StringShorthandToken = GenericValueToken<'StringShorthand'>
export type SymbolToken = GenericValueToken<'Symbol'>
export type ReservedSymbolToken = GenericValueToken<'ReservedSymbol'>
export type ModifierToken = GenericValueToken<'Modifier', ModifierName>
export type RegexpShorthandToken = GenericValueToken<'RegexpShorthand'>
export type CollectionAccessorToken = GenericValueToken<'CollectionAccessor', '.' | '#'>
export type CommentToken = GenericValueToken<'Comment'>
export type InfixOperatorToken = GenericValueToken<'InfixOperator', '+' | '-' | '*' | '/' | '%' | '==' | '!=' | '<' | '<=' | '>' | '>=' | '&&' | '||' | '^' >

export type SimpleToken =
  | LParenToken
  | RParenToken
  | LBracketToken
  | RBracketToken
  | LBraceToken
  | RBraceToken
  | FnShorthandToken
  | NewLineToken
  | InfixToken
  | PostfixToken

export type ValueToken =
  | NumberToken
  | StringToken
  | StringShorthandToken
  | SymbolToken
  | ReservedSymbolToken
  | ModifierToken
  | RegexpShorthandToken
  | CollectionAccessorToken
  | CommentToken
  | InfixOperatorToken

export type Token =
  | SimpleToken
  | NumberToken
  | StringToken
  | StringShorthandToken
  | SymbolToken
  | ReservedSymbolToken
  | ModifierToken
  | RegexpShorthandToken
  | CollectionAccessorToken
  | CommentToken
  | InfixOperatorToken

export type MetaToken = NewLineToken | CommentToken
export interface MetaTokens {
  leadingMetaTokens: MetaToken[] // Comments on the lines before the token
  inlineCommentToken: CommentToken | null // Comment on the same line as the token
}

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
    throwUnexpectedToken('LParen', token)
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
    throwUnexpectedToken('LParen', token)
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
    throwUnexpectedToken('LParen', token)
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
    throwUnexpectedToken('LParen', token)
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
    throwUnexpectedToken('LParen', token)
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
    throwUnexpectedToken('LParen', token)
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
    throwUnexpectedToken('LParen', token)
  }
}
export function asStringToken(token?: Token): StringToken {
  assertStringToken(token)
  return token
}

export function isStringShorthandToken(token?: Token): token is StringShorthandToken {
  return token?.[0] === 'StringShorthand'
}
export function assertStringShorthandToken(token?: Token): asserts token is StringShorthandToken {
  if (!isStringShorthandToken(token)) {
    throwUnexpectedToken('LParen', token)
  }
}
export function asStringShorthandToken(token?: Token): StringShorthandToken {
  assertStringShorthandToken(token)
  return token
}

export function isSymbolToken(token?: Token): token is SymbolToken {
  return token?.[0] === 'Symbol'
}
export function assertSymbolToken(token?: Token): asserts token is SymbolToken {
  if (!isSymbolToken(token)) {
    throwUnexpectedToken('LParen', token)
  }
}
export function asSymbolToken(token?: Token): SymbolToken {
  assertSymbolToken(token)
  return token
}

export function isReservedSymbolToken(token?: Token): token is ReservedSymbolToken {
  return token?.[0] === 'ReservedSymbol'
}
export function assertReservedSymbolToken(token?: Token): asserts token is ReservedSymbolToken {
  if (!isReservedSymbolToken(token)) {
    throwUnexpectedToken('LParen', token)
  }
}
export function asReservedSymbolToken(token?: Token): ReservedSymbolToken {
  assertReservedSymbolToken(token)
  return token
}

export function isModifierToken(token?: Token): token is ModifierToken {
  return token?.[0] === 'Modifier'
}
export function assertModifierToken(token?: Token): asserts token is ModifierToken {
  if (!isModifierToken(token)) {
    throwUnexpectedToken('LParen', token)
  }
}
export function asModifierToken(token?: Token): ModifierToken {
  assertModifierToken(token)
  return token
}

export function isRegexpShorthandToken(token?: Token): token is RegexpShorthandToken {
  return token?.[0] === 'RegexpShorthand'
}
export function assertRegexpShorthandToken(token?: Token): asserts token is RegexpShorthandToken {
  if (!isRegexpShorthandToken(token)) {
    throwUnexpectedToken('LParen', token)
  }
}
export function asRegexpShorthandToken(token?: Token): RegexpShorthandToken {
  assertRegexpShorthandToken(token)
  return token
}

export function isFnShorthandToken(token?: Token): token is FnShorthandToken {
  return token?.[0] === 'FnShorthand'
}
export function assertFnShorthandToken(token?: Token): asserts token is FnShorthandToken {
  if (!isFnShorthandToken(token)) {
    throwUnexpectedToken('LParen', token)
  }
}
export function asFnShorthandToken(token?: Token): FnShorthandToken {
  assertFnShorthandToken(token)
  return token
}

export function isCollectionAccessorToken(token?: Token): token is CollectionAccessorToken {
  return token?.[0] === 'CollectionAccessor'
}
export function assertCollectionAccessorToken(token?: Token): asserts token is CollectionAccessorToken {
  if (!isCollectionAccessorToken(token)) {
    throwUnexpectedToken('LParen', token)
  }
}
export function asCollectionAccessorToken(token?: Token): CollectionAccessorToken {
  assertCollectionAccessorToken(token)
  return token
}

export function isCommentToken(token?: Token): token is CommentToken {
  return token?.[0] === 'Comment'
}
export function assertCommentToken(token?: Token): asserts token is CommentToken {
  if (!isCommentToken(token)) {
    throwUnexpectedToken('LParen', token)
  }
}
export function asCommentToken(token?: Token): CommentToken {
  assertCommentToken(token)
  return token
}

export function isNewLineToken(token?: Token): token is NewLineToken {
  return token?.[0] === 'NewLine'
}
export function assertNewLineToken(token?: Token): asserts token is NewLineToken {
  if (!isNewLineToken(token)) {
    throwUnexpectedToken('LParen', token)
  }
}
export function asNewLineToken(token?: Token): NewLineToken {
  assertNewLineToken(token)
  return token
}

export function isInfixToken(token?: Token): token is InfixToken {
  return token?.[0] === 'Infix'
}
export function assertInfixToken(token?: Token): asserts token is InfixToken {
  if (!isInfixToken(token)) {
    throwUnexpectedToken('LParen', token)
  }
}
export function asInfixToken(token?: Token): InfixToken {
  assertInfixToken(token)
  return token
}

export function isPostfixToken(token?: Token): token is PostfixToken {
  return token?.[0] === 'Postfix'
}
export function assertPostfixToken(token?: Token): asserts token is PostfixToken {
  if (!isPostfixToken(token)) {
    throwUnexpectedToken('LParen', token)
  }
}
export function asPostfixToken(token?: Token): PostfixToken {
  assertPostfixToken(token)
  return token
}

export function isInfixOperatorToken(token?: Token): token is InfixOperatorToken {
  return token?.[0] === 'InfixOperator'
}
export function assertInfixOperatorToken(token?: Token): asserts token is InfixOperatorToken {
  if (!isInfixOperatorToken(token)) {
    throwUnexpectedToken('LParen', token)
  }
}
export function asInfixOperatorToken(token?: Token): InfixOperatorToken {
  assertInfixOperatorToken(token)
  return token
}

function throwUnexpectedToken(expected: TokenType, actual?: Token): never {
  throw new LitsError(`Unexpected token: ${actual}, expected ${expected}`)
}
