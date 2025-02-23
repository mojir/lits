import { LitsError } from '../../errors'
import type { CommonSimpleToken, CommonSimpleTokenType, CommonValueToken, CommonValueTokenType } from '../common/commonTokens'
import { commomValueTokenTypes, commonSimpleTokenTypes } from '../common/commonTokens'
import type { Token } from '../tokens'
import { type TokenDebugData, throwUnexpectedToken } from '../utils'

export const infixOnlySimpleTokenTypes = [
  'IF_Postfix',
] as const satisfies `IF_${string}`[]

export const infixSimpleTokenTypes = [
  ...commonSimpleTokenTypes,
  ...infixOnlySimpleTokenTypes,
] as const

export const infixOnlyValueTokenTypes = [
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

export const infixTokenTypes = [
  ...infixSimpleTokenTypes,
  ...infixValueTokenTypes,
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

export type InfixSimpleTokenType = typeof infixSimpleTokenTypes[number]
export type InfixValueTokenType = typeof infixValueTokenTypes[number]

type GenericInfixSimpleToken<T extends Exclude<InfixSimpleTokenType, CommonSimpleTokenType>> = [T] | [T, TokenDebugData]
type GenericInfixValueToken<T extends Exclude<InfixValueTokenType, CommonValueTokenType>, V extends string = string> = [T, V] | [T, V, TokenDebugData]

export type IF_PostfixToken = GenericInfixSimpleToken<'IF_Postfix'>
export type IF_WhitespaceToken = GenericInfixValueToken<'IF_Whitespace'>
export type IF_OperatorToken = GenericInfixValueToken<'IF_Operator', InfixOperator>
export type IF_SymbolToken = GenericInfixValueToken<'IF_Symbol'>
export type IF_ReservedSymbolToken = GenericInfixValueToken<'IF_ReservedSymbol'>
export type IF_SingleLineCommentToken = GenericInfixValueToken<'IF_SingleLineComment'>
export type IF_MultiLineCommentToken = GenericInfixValueToken<'IF_MultiLineComment'>

export type InfixOnlySimpleToken =
  | IF_PostfixToken

export type InfixOnlyValueToken =
  | IF_WhitespaceToken
  | IF_OperatorToken
  | IF_SymbolToken
  | IF_ReservedSymbolToken
  | IF_SingleLineCommentToken
  | IF_MultiLineCommentToken

export type InfixToken =
  | InfixOnlySimpleToken
  | InfixOnlyValueToken
  | CommonSimpleToken
  | CommonValueToken

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
