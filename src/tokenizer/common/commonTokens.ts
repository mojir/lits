import { type TokenDebugData, throwUnexpectedToken } from '../utils'
import type { Token } from '../tokens'

export const commonTokenTypes = [
  'LBrace',
  'LBracket',
  'LParen',
  'RBrace',
  'RBracket',
  'RParen',
  'String',
  'RegexpShorthand',
] as const

export type CommonTokenType = typeof commonTokenTypes[number]

type GenericCommonToken<T extends CommonTokenType, V extends string = string> = [T, V] | [T, V, TokenDebugData]

export type LParenToken = GenericCommonToken<'LParen', '('>
export type RParenToken = GenericCommonToken<'RParen', ')'>
export type LBracketToken = GenericCommonToken<'LBracket', '['>
export type RBracketToken = GenericCommonToken<'RBracket', ']'>
export type LBraceToken = GenericCommonToken<'LBrace', '{'>
export type RBraceToken = GenericCommonToken<'RBrace', '}'>

export type StringToken = GenericCommonToken<'String'>
export type RegexpShorthandToken = GenericCommonToken<'RegexpShorthand'>

export type CommonToken =
  | LParenToken
  | RParenToken
  | LBracketToken
  | RBracketToken
  | LBraceToken
  | RBraceToken
  | StringToken
  | RegexpShorthandToken

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
