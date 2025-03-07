import type { CommonToken, CommonTokenType } from '../common/commonTokens'
import { commonTokenTypes } from '../common/commonTokens'
import type { Token } from '../tokens'
import { type TokenDebugData, throwUnexpectedToken } from '../utils'

export const modifierNames = ['&rest', '&let', '&when', '&while'] as const
export type ModifierName = typeof modifierNames[number]

export const polishOnlyTokenTypes = [
  'P_FnShorthand',
  'P_Modifier',
  'P_StringShorthand',
  'P_Symbol',
  'P_ReservedSymbol',
  'P_CollectionAccessor',
  'P_Comment',
  'P_Whitespace',
  'P_Number',
] as const satisfies `P_${string}`[]

export const polishTokenTypes = [
  ...commonTokenTypes,
  ...polishOnlyTokenTypes,
] as const

export type PolishValueTokenType = typeof polishTokenTypes[number]
export type PolishTokenType = typeof polishTokenTypes[number]

type GenericPolishValueToken<T extends Exclude<PolishValueTokenType, CommonTokenType>, V extends string = string> = [T, V] | [T, V, TokenDebugData]

export type P_FnShorthandToken = GenericPolishValueToken<'P_FnShorthand', '#'>
export type P_ModifierToken = GenericPolishValueToken<'P_Modifier', ModifierName>
export type P_StringShorthandToken = GenericPolishValueToken<'P_StringShorthand'>
export type P_SymbolToken = GenericPolishValueToken<'P_Symbol'>
export type P_ReservedSymbolToken = GenericPolishValueToken<'P_ReservedSymbol'>
export type P_CollectionAccessorToken = GenericPolishValueToken<'P_CollectionAccessor', '.' | '#'>
export type P_CommentToken = GenericPolishValueToken<'P_Comment'>
export type P_WhitespaceToken = GenericPolishValueToken<'P_Whitespace'>
export type P_NumberToken = GenericPolishValueToken<'P_Number'>

export type PolishOnlyToken =
  | P_FnShorthandToken
  | P_ModifierToken
  | P_StringShorthandToken
  | P_SymbolToken
  | P_ReservedSymbolToken
  | P_CollectionAccessorToken
  | P_CommentToken
  | P_WhitespaceToken
  | P_NumberToken

export type PolishToken =
  | PolishOnlyToken
  | CommonToken

export function isP_StringShorthandToken(token?: Token): token is P_StringShorthandToken {
  return token?.[0] === 'P_StringShorthand'
}
export function assertP_StringShorthandToken(token?: Token): asserts token is P_StringShorthandToken {
  if (!isP_StringShorthandToken(token)) {
    throwUnexpectedToken('P_StringShorthand', undefined, token)
  }
}
export function asP_StringShorthandToken(token?: Token): P_StringShorthandToken {
  assertP_StringShorthandToken(token)
  return token
}

export function isP_SymbolToken(token?: Token): token is P_SymbolToken {
  return token?.[0] === 'P_Symbol'
}
export function assertP_SymbolToken(token?: Token): asserts token is P_SymbolToken {
  if (!isP_SymbolToken(token)) {
    throwUnexpectedToken('P_Symbol', undefined, token)
  }
}
export function asP_SymbolToken(token?: Token): P_SymbolToken {
  assertP_SymbolToken(token)
  return token
}

export function isP_ReservedSymbolToken(token?: Token): token is P_ReservedSymbolToken {
  return token?.[0] === 'P_ReservedSymbol'
}
export function assertP_ReservedSymbolToken(token?: Token): asserts token is P_ReservedSymbolToken {
  if (!isP_ReservedSymbolToken(token)) {
    throwUnexpectedToken('P_ReservedSymbol', undefined, token)
  }
}
export function asP_ReservedSymbolToken(token?: Token): P_ReservedSymbolToken {
  assertP_ReservedSymbolToken(token)
  return token
}

export function isP_ModifierToken(token?: Token): token is P_ModifierToken {
  return token?.[0] === 'P_Modifier'
}
export function assertP_ModifierToken(token?: Token): asserts token is P_ModifierToken {
  if (!isP_ModifierToken(token)) {
    throwUnexpectedToken('P_Modifier', undefined, token)
  }
}
export function asP_ModifierToken(token?: Token): P_ModifierToken {
  assertP_ModifierToken(token)
  return token
}

export function isP_FnShorthandToken(token?: Token): token is P_FnShorthandToken {
  return token?.[0] === 'P_FnShorthand'
}
export function assertP_FnShorthandToken(token?: Token): asserts token is P_FnShorthandToken {
  if (!isP_FnShorthandToken(token)) {
    throwUnexpectedToken('P_FnShorthand', undefined, token)
  }
}
export function asP_FnShorthandToken(token?: Token): P_FnShorthandToken {
  assertP_FnShorthandToken(token)
  return token
}

export function isP_CollectionAccessorToken(token?: Token): token is P_CollectionAccessorToken {
  return token?.[0] === 'P_CollectionAccessor'
}
export function assertP_CollectionAccessorToken(token?: Token): asserts token is P_CollectionAccessorToken {
  if (!isP_CollectionAccessorToken(token)) {
    throwUnexpectedToken('P_CollectionAccessor', undefined, token)
  }
}
export function asP_CollectionAccessorToken(token?: Token): P_CollectionAccessorToken {
  assertP_CollectionAccessorToken(token)
  return token
}

export function isP_CommentToken(token?: Token): token is P_CommentToken {
  return token?.[0] === 'P_Comment'
}
export function assertP_CommentToken(token?: Token): asserts token is P_CommentToken {
  if (!isP_CommentToken(token)) {
    throwUnexpectedToken('P_Comment', undefined, token)
  }
}
export function asP_CommentToken(token?: Token): P_CommentToken {
  assertP_CommentToken(token)
  return token
}

export function isP_WhitespaceToken(token?: Token): token is P_WhitespaceToken {
  return token?.[0] === 'P_Whitespace'
}
export function assertP_WhitespaceToken(token?: Token): asserts token is P_WhitespaceToken {
  if (!isP_WhitespaceToken(token)) {
    throwUnexpectedToken('P_Whitespace', undefined, token)
  }
}
export function asP_WhitespaceToken(token?: Token): P_WhitespaceToken {
  assertP_WhitespaceToken(token)
  return token
}

export function isP_NumberToken(token?: Token): token is P_NumberToken {
  return token?.[0] === 'P_Number'
}
export function assertP_NumberToken(token?: Token): asserts token is P_NumberToken {
  if (!isP_NumberToken(token)) {
    throwUnexpectedToken('P_Number', undefined, token)
  }
}
export function asP_NumberToken(token?: Token): P_NumberToken {
  assertP_NumberToken(token)
  return token
}
