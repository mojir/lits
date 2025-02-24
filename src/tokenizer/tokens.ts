import { LitsError } from '../errors'
import type { CommonSimpleToken, CommonValueToken } from './common/commonTokens'
import { commomValueTokenTypes, commonSimpleTokenTypes } from './common/commonTokens'
import type { AlgebraicOnlySimpleToken, AlgebraicOnlyValueToken, AlgebraicSimpleTokenType, AlgebraicValueTokenType } from './algebraic/algebraicTokens'
import { algebraicOnlySimpleTokenTypes, algebraicOnlyValueTokenTypes } from './algebraic/algebraicTokens'
import type { PolishOnlySimpleToken, PolishOnlyValueToken, PolishSimpleTokenType, PolishValueTokenType } from './polish/polishTokens'
import { polishOnlySimpleTokenTypes, polishOnlyValueTokenTypes } from './polish/polishTokens'

export const simpleTokenTypes = [
  ...commonSimpleTokenTypes,
  ...algebraicOnlySimpleTokenTypes,
  ...polishOnlySimpleTokenTypes,
] as const

export const valueTokenTypes = [
  ...commomValueTokenTypes,
  ...algebraicOnlyValueTokenTypes,
  ...polishOnlyValueTokenTypes,
] as const

export const tokenTypes = [
  ...commonSimpleTokenTypes,
  ...algebraicOnlySimpleTokenTypes,
  ...polishOnlySimpleTokenTypes,
  ...commomValueTokenTypes,
  ...algebraicOnlyValueTokenTypes,
  ...polishOnlyValueTokenTypes,
] as const

type SimpleTokenType = AlgebraicSimpleTokenType | PolishSimpleTokenType
type ValueTokenType = AlgebraicValueTokenType | PolishValueTokenType

export type TokenType = typeof tokenTypes[number]

export type SimpleToken =
  | CommonSimpleToken
  | AlgebraicOnlySimpleToken
  | PolishOnlySimpleToken

export type ValueToken =
  | CommonValueToken
  | AlgebraicOnlyValueToken
  | PolishOnlyValueToken

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
