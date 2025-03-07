import { LitsError } from '../errors'
import type { AlgebraicOnlyToken } from './algebraic/algebraicTokens'
import { algebraicOnlyTokenTypes } from './algebraic/algebraicTokens'
import type { CommonToken } from './common/commonTokens'
import { commonTokenTypes } from './common/commonTokens'
import type { PolishOnlyToken } from './polish/polishTokens'
import { polishOnlyTokenTypes } from './polish/polishTokens'

export const tokenTypes = [
  ...commonTokenTypes,
  ...algebraicOnlyTokenTypes,
  ...polishOnlyTokenTypes,
] as const

export type TokenType = typeof tokenTypes[number]

export type Token =
  | PolishOnlyToken
  | AlgebraicOnlyToken
  | CommonToken

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
