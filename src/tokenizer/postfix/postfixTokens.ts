import type { CommonSimpleToken, CommonSimpleTokenType, CommonValueToken, CommonValueTokenType } from '../common/commonTokens'
import { commomValueTokenTypes, commonSimpleTokenTypes } from '../common/commonTokens'
import type { Token } from '../tokens'
import { type TokenDebugData, throwUnexpectedToken } from '../utils'

export const modifierNames = ['&', '&let', '&when', '&while'] as const
export type ModifierName = typeof modifierNames[number]

export const postfixOnlySimpleTokenTypes = [
  'PF_FnShorthand',
  'PF_Infix',
] as const satisfies `PF_${string}`[]

export const postfixSimpleTokenTypes = [
  ...commonSimpleTokenTypes,
  ...postfixOnlySimpleTokenTypes,
] as const

export const postfixOnlyValueTokenTypes = [
  'PF_Modifier',
  'PF_StringShorthand',
  'PF_Symbol',
  'PF_ReservedSymbol',
  'PF_RegexpShorthand',
  'PF_CollectionAccessor',
  'PF_Comment',
  'PF_Whitespace',
  'PF_Number',
] as const satisfies `PF_${string}`[]

export const postfixValueTokenTypes = [
  ...commomValueTokenTypes,
  ...postfixOnlyValueTokenTypes,
] as const

export const postfixTokenTypes = [
  ...postfixSimpleTokenTypes,
  ...postfixValueTokenTypes,
] as const

export type PostfixSimpleTokenType = typeof postfixSimpleTokenTypes[number]
export type PostfixValueTokenType = typeof postfixValueTokenTypes[number]
export type PostfixTokenType = typeof postfixTokenTypes[number]

type GenericPostfixSimpleToken<T extends Exclude<PostfixSimpleTokenType, CommonSimpleTokenType>> = [T] | [T, TokenDebugData]
type GenericPostfixValueToken<T extends Exclude<PostfixValueTokenType, CommonValueTokenType>, V extends string = string> = [T, V] | [T, V, TokenDebugData]

export type PF_FnShorthandToken = GenericPostfixSimpleToken<'PF_FnShorthand'>
export type PF_InfixToken = GenericPostfixSimpleToken<'PF_Infix'>
export type PF_ModifierToken = GenericPostfixValueToken<'PF_Modifier', ModifierName>
export type PF_StringShorthandToken = GenericPostfixValueToken<'PF_StringShorthand'>
export type PF_SymbolToken = GenericPostfixValueToken<'PF_Symbol'>
export type PF_ReservedSymbolToken = GenericPostfixValueToken<'PF_ReservedSymbol'>
export type PF_RegexpShorthandToken = GenericPostfixValueToken<'PF_RegexpShorthand'>
export type PF_CollectionAccessorToken = GenericPostfixValueToken<'PF_CollectionAccessor', '.' | '#'>
export type PF_CommentToken = GenericPostfixValueToken<'PF_Comment'>
export type PF_WhitespaceToken = GenericPostfixValueToken<'PF_Whitespace'>
export type PF_NumberToken = GenericPostfixValueToken<'PF_Number'>

export type PostfixOnlySimpleToken =
  | PF_FnShorthandToken
  | PF_InfixToken

export type PostfixOnlyValueToken =
  | PF_ModifierToken
  | PF_StringShorthandToken
  | PF_SymbolToken
  | PF_ReservedSymbolToken
  | PF_RegexpShorthandToken
  | PF_CollectionAccessorToken
  | PF_CommentToken
  | PF_WhitespaceToken
  | PF_NumberToken

export type PostfixToken =
  | PostfixOnlySimpleToken
  | PostfixOnlyValueToken
  | CommonSimpleToken
  | CommonValueToken

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

export function isPF_NumberToken(token?: Token): token is PF_NumberToken {
  return token?.[0] === 'PF_Number'
}
export function assertPF_NumberToken(token?: Token): asserts token is PF_NumberToken {
  if (!isPF_NumberToken(token)) {
    throwUnexpectedToken('PF_Number', token)
  }
}
export function asPF_NumberToken(token?: Token): PF_NumberToken {
  assertPF_NumberToken(token)
  return token
}
