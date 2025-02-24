import { LitsError } from '../../errors'
import type { CommonSimpleToken, CommonValueToken, CommonValueTokenType } from '../common/commonTokens'
import { commomValueTokenTypes, commonSimpleTokenTypes } from '../common/commonTokens'
import type { Token } from '../tokens'
import { type TokenDebugData, throwUnexpectedToken } from '../utils'

export const algebraicSimpleTokenTypes = [
  ...commonSimpleTokenTypes,
] as const

export const algebraicOnlyValueTokenTypes = [
  'A_Whitespace',
  'A_Operator',
  'A_Symbol',
  'A_ReservedSymbol',
  'A_SingleLineComment',
  'A_MultiLineComment',
  'A_Number',
] as const satisfies `A_${string}`[]

export const algebraicValueTokenTypes = [
  ...commomValueTokenTypes,
  ...algebraicOnlyValueTokenTypes,
] as const

export const algebraicTokenTypes = [
  ...algebraicSimpleTokenTypes,
  ...algebraicValueTokenTypes,
] as const

export const AlgebraicOperators = [
  // Unary only operators
  '!', // logical NOT
  '~', // bitwise NOT
  '=', // property assignemnt operator
  ',', // element delimiter

  '.', // property accessor

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

export type AlgebraicOperator = typeof AlgebraicOperators[number]

export function isAlgebraicOperator(operator: string): operator is AlgebraicOperator {
  return AlgebraicOperators.includes(operator as AlgebraicOperator)
}
export function assertAlgebraicOperator(operator: string): asserts operator is AlgebraicOperator {
  if (!isAlgebraicOperator(operator)) {
    throw new LitsError(`Expected algebraic operator, got ${operator}`, undefined)
  }
}
export function asAlgebraicOperator(operator: string): AlgebraicOperator {
  assertAlgebraicOperator(operator)
  return operator
}

export type AlgebraicSimpleTokenType = typeof algebraicSimpleTokenTypes[number]
export type AlgebraicValueTokenType = typeof algebraicValueTokenTypes[number]
export type AlgebraicTokenType = typeof algebraicTokenTypes[number]

type GenericAlgebraicValueToken<T extends Exclude<AlgebraicValueTokenType, CommonValueTokenType>, V extends string = string> = [T, V] | [T, V, TokenDebugData]

export type A_WhitespaceToken = GenericAlgebraicValueToken<'A_Whitespace'>
export type A_NumberToken = GenericAlgebraicValueToken<'A_Number'>
export type A_OperatorToken<T extends AlgebraicOperator = AlgebraicOperator> = GenericAlgebraicValueToken<'A_Operator', T>
export type A_SymbolToken = GenericAlgebraicValueToken<'A_Symbol'>
export type A_ReservedSymbolToken = GenericAlgebraicValueToken<'A_ReservedSymbol'>
export type A_SingleLineCommentToken = GenericAlgebraicValueToken<'A_SingleLineComment'>
export type A_MultiLineCommentToken = GenericAlgebraicValueToken<'A_MultiLineComment'>

export type AlgebraicOnlyValueToken =
  | A_WhitespaceToken
  | A_NumberToken
  | A_OperatorToken
  | A_SymbolToken
  | A_ReservedSymbolToken
  | A_SingleLineCommentToken
  | A_MultiLineCommentToken

export type AlgebraicToken =
  | AlgebraicOnlyValueToken
  | CommonSimpleToken
  | CommonValueToken

export function isA_SymbolToken(token?: Token): token is A_SymbolToken {
  return token?.[0] === 'A_Symbol'
}
export function assertA_SymbolToken(token?: Token): asserts token is A_SymbolToken {
  if (!isA_SymbolToken(token)) {
    throwUnexpectedToken('A_Symbol', token)
  }
}
export function asA_SymbolToken(token?: Token): A_SymbolToken {
  assertA_SymbolToken(token)
  return token
}

export function isA_ReservedSymbolToken(token?: Token): token is A_ReservedSymbolToken {
  return token?.[0] === 'A_ReservedSymbol'
}
export function assertA_ReservedSymbolToken(token?: Token): asserts token is A_ReservedSymbolToken {
  if (!isA_ReservedSymbolToken(token)) {
    throwUnexpectedToken('A_ReservedSymbol', token)
  }
}
export function asA_ReservedSymbolToken(token?: Token): A_ReservedSymbolToken {
  assertA_ReservedSymbolToken(token)
  return token
}

export function isA_CommentToken(token?: Token): token is A_SingleLineCommentToken {
  return token?.[0] === 'A_SingleLineComment'
}
export function assertA_CommentToken(token?: Token): asserts token is A_SingleLineCommentToken {
  if (!isA_CommentToken(token)) {
    throwUnexpectedToken('A_SingleLineComment', token)
  }
}
export function asA_CommentToken(token?: Token): A_SingleLineCommentToken {
  assertA_CommentToken(token)
  return token
}

export function isA_MultiLineCommentToken(token?: Token): token is A_MultiLineCommentToken {
  return token?.[0] === 'A_MultiLineComment'
}
export function assertA_MultiLineCommentToken(token?: Token): asserts token is A_MultiLineCommentToken {
  if (!isA_MultiLineCommentToken(token)) {
    throwUnexpectedToken('A_MultiLineComment', token)
  }
}
export function asA_MultiLineCommentToken(token?: Token): A_MultiLineCommentToken {
  assertA_MultiLineCommentToken(token)
  return token
}

export function isA_OperatorToken<T extends AlgebraicOperator>(token?: Token, operatorName?: T): token is A_OperatorToken<T> {
  if (token?.[0] !== 'A_Operator') {
    return false
  }
  if (operatorName && token[1] !== operatorName) {
    return false
  }
  return true
}
export function assertA_OperatorToken<T extends AlgebraicOperator>(token?: Token, operatorName?: T): asserts token is A_OperatorToken<T> {
  if (!isA_OperatorToken(token, operatorName)) {
    if (operatorName) {
      throw new LitsError(`Unexpected token: ${token}, expected operator ${operatorName}`, undefined)
    }
    throwUnexpectedToken('A_Operator', token)
  }
}
export function asA_OperatorToken<T extends AlgebraicOperator>(token?: Token, operatorName?: T): A_OperatorToken<T> {
  assertA_OperatorToken(token, operatorName)
  return token
}

export function isA_WhitespaceToken(token?: Token): token is A_WhitespaceToken {
  return token?.[0] === 'A_Whitespace'
}
export function assertA_WhitespaceToken(token?: Token): asserts token is A_WhitespaceToken {
  if (!isA_WhitespaceToken(token)) {
    throwUnexpectedToken('A_Whitespace', token)
  }
}
export function asA_WhitespaceToken(token?: Token): A_WhitespaceToken {
  assertA_WhitespaceToken(token)
  return token
}

export function isA_NumberToken(token?: Token): token is A_NumberToken {
  return token?.[0] === 'A_Number'
}
export function assertA_NumberToken(token?: Token): asserts token is A_NumberToken {
  if (!isA_NumberToken(token)) {
    throwUnexpectedToken('A_Number', token)
  }
}
export function asA_NumberToken(token?: Token): A_NumberToken {
  assertA_NumberToken(token)
  return token
}
