import { type TokenDebugData, throwUnexpectedToken } from '../utils'
import type { Token } from '../tokens'

export const commonSimpleTokenTypes = [
  'LBrace',
  'LBracket',
  'LParen',
  'RBrace',
  'RBracket',
  'RParen',
] as const

export const commomValueTokenTypes = [
  'Number',
  'String',
] as const

export type CommonSimpleTokenType = typeof commonSimpleTokenTypes[number]
export type CommonValueTokenType = typeof commomValueTokenTypes[number]

type GenericCommonSimpleToken<T extends CommonSimpleTokenType> = [T] | [T, TokenDebugData]
type GenericCommonValueToken<T extends CommonValueTokenType, V extends string = string> = [T, V] | [T, V, TokenDebugData]

export type LParenToken = GenericCommonSimpleToken<'LParen'>
export type RParenToken = GenericCommonSimpleToken<'RParen'>
export type LBracketToken = GenericCommonSimpleToken<'LBracket'>
export type RBracketToken = GenericCommonSimpleToken<'RBracket'>
export type LBraceToken = GenericCommonSimpleToken<'LBrace'>
export type RBraceToken = GenericCommonSimpleToken<'RBrace'>

export type NumberToken = GenericCommonValueToken<'Number'>
export type StringToken = GenericCommonValueToken<'String'>

export type CommonSimpleToken =
  | LParenToken
  | RParenToken
  | LBracketToken
  | RBracketToken
  | LBraceToken
  | RBraceToken

export type CommonValueToken =
  | NumberToken
  | StringToken

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
