import { LitsError } from '../errors'
import type { CommonSimpleToken, CommonValueToken } from './common/commonTokens'
import { commomValueTokenTypes, commonSimpleTokenTypes } from './common/commonTokens'
import type { InfixOnlySimpleToken, InfixOnlyValueToken, InfixSimpleTokenType, InfixValueTokenType } from './infix/infixTokens'
import { infixOnlySimpleTokenTypes, infixOnlyValueTokenTypes } from './infix/infixTokens'
import type { PostfixOnlySimpleToken, PostfixOnlyValueToken, PostfixSimpleTokenType, PostfixValueTokenType } from './postfix/postfixTokens'
import { postfixOnlySimpleTokenTypes, postfixOnlyValueTokenTypes } from './postfix/postfixTokens'

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

type SimpleTokenType = InfixSimpleTokenType | PostfixSimpleTokenType
type ValueTokenType = InfixValueTokenType | PostfixValueTokenType

export type TokenType = typeof tokenTypes[number]

export type SimpleToken =
  | CommonSimpleToken
  | InfixOnlySimpleToken
  | PostfixOnlySimpleToken

export type ValueToken =
  | CommonValueToken
  | InfixOnlyValueToken
  | PostfixOnlyValueToken

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
