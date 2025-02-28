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

const symbolicUnaryOperators = [
  '!', // logical NOT
  '~', // bitwise NOT
  '+', // addition
  '-', // subtraction
] as const

const symbolicBinaryOperators = [
  '**', // exponentiation

  '*', // multiplication
  '/', // division
  '%', // remainder

  '+', // addition
  '-', // subtraction

  '<<', // left shift
  '>>', // signed right shift
  '>>>', // unsigned right shift

  '++', // string concatenation

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

const otherSymbolicOperators = [
  '=>', // lambda
  '...', // rest
  '.', // property accessor
  ',', // item separator
  '=', // property assignment
] as const

const symbolicOperators = [
  ...symbolicUnaryOperators,
  ...symbolicBinaryOperators,
  ...otherSymbolicOperators,
] as const

const binaryFunctionOperators = [
  'filter',
  'has?',
  'contains?',
  'abs',
] as const

const unaryFunctionOperators = [
  'empty?',
] as const

const algebraicOperatos = [
  ...symbolicOperators,
  ...binaryFunctionOperators,
  ...unaryFunctionOperators,
] as const

export type SymbolicUnaryOperator = typeof symbolicUnaryOperators[number]
export type SymbolicBinaryOperator = typeof symbolicBinaryOperators[number]
export type SymbolicOperator = typeof symbolicOperators[number]

export type BinaryFunctionOperator = typeof binaryFunctionOperators[number]
export type UnaryFunctionOperator = typeof unaryFunctionOperators[number]
export type AlgebraicOperator = typeof algebraicOperatos[number]

const symbolicUnaryOperatorSet = new Set(symbolicUnaryOperators)
export function isSymbolicUnaryOperator(operator: string): operator is SymbolicUnaryOperator {
  return symbolicUnaryOperatorSet.has(operator as SymbolicUnaryOperator)
}
export function assertSymbolicUnaryOperator(operator: string): asserts operator is SymbolicUnaryOperator {
  if (!isSymbolicUnaryOperator(operator)) {
    throw new LitsError(`Expected symbolic unary operator, got ${operator}`, undefined)
  }
}
export function asSymbolicUnaryOperator(operator: string): SymbolicUnaryOperator {
  assertSymbolicUnaryOperator(operator)
  return operator
}

const symbolicBinaryOperatorSet = new Set(symbolicBinaryOperators)
export function isSymbolicBinaryOperator(operator: string): operator is SymbolicBinaryOperator {
  return symbolicBinaryOperatorSet.has(operator as SymbolicBinaryOperator)
}
export function assertSymbolicBinaryOperator(operator: string): asserts operator is SymbolicBinaryOperator {
  if (!isSymbolicBinaryOperator(operator)) {
    throw new LitsError(`Expected symbolic binary operator, got ${operator}`, undefined)
  }
}
export function asSymbolicBinaryOperator(operator: string): SymbolicBinaryOperator {
  assertSymbolicBinaryOperator(operator)
  return operator
}

const symbolicOperatorSet = new Set(symbolicOperators)
export function isSymbolicOperator(operator: string): operator is SymbolicOperator {
  return symbolicOperatorSet.has(operator as SymbolicOperator)
}
export function assertSymbolicOperator(operator: string): asserts operator is SymbolicOperator {
  if (!isSymbolicOperator(operator)) {
    throw new LitsError(`Expected symbolic operator, got ${operator}`, undefined)
  }
}
export function asSymbolicOperator(operator: string): SymbolicOperator {
  assertSymbolicOperator(operator)
  return operator
}

const binaryFunctionOperatorSet = new Set(binaryFunctionOperators)
export function isBinaryFunctionOperator(operator: string): operator is BinaryFunctionOperator {
  return binaryFunctionOperatorSet.has(operator as BinaryFunctionOperator)
}
export function assertBinaryFunctionOperator(operator: string): asserts operator is BinaryFunctionOperator {
  if (!isBinaryFunctionOperator(operator)) {
    throw new LitsError(`Expected binary function operator, got ${operator}`, undefined)
  }
}
export function asBinaryFunctionOperator(operator: string): BinaryFunctionOperator {
  assertBinaryFunctionOperator(operator)
  return operator
}

const unaryFunctionOperatorSet = new Set(unaryFunctionOperators)
export function isUnaryFunctionOperator(operator: string): operator is UnaryFunctionOperator {
  return unaryFunctionOperatorSet.has(operator as UnaryFunctionOperator)
}
export function assertUnaryFunctionOperator(operator: string): asserts operator is UnaryFunctionOperator {
  if (!isUnaryFunctionOperator(operator)) {
    throw new LitsError(`Expected unary function operator, got ${operator}`, undefined)
  }
}
export function asUnaryFunctionOperator(operator: string): UnaryFunctionOperator {
  assertUnaryFunctionOperator(operator)
  return operator
}

const algebraicOperatorSet = new Set(algebraicOperatos)
export function isAlgebraicOperator(operator: string): operator is AlgebraicOperator {
  return algebraicOperatorSet.has(operator as AlgebraicOperator)
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
export type A_OperatorToken<T extends SymbolicOperator = SymbolicOperator> = GenericAlgebraicValueToken<'A_Operator', T>
export type A_SymbolToken<T extends string = string> = GenericAlgebraicValueToken<'A_Symbol', T>
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

export function isA_SymbolToken(token: Token | undefined): token is A_SymbolToken {
  return token?.[0] === 'A_Symbol'
}
export function assertA_SymbolToken(token: Token | undefined): asserts token is A_SymbolToken {
  if (!isA_SymbolToken(token)) {
    throwUnexpectedToken('A_Symbol', token)
  }
}
export function asA_SymbolToken(token: Token | undefined): A_SymbolToken {
  assertA_SymbolToken(token)
  return token
}

export function isA_BinaryOperatorToken(token: Token | undefined): token is A_OperatorToken<SymbolicBinaryOperator> {
  return token?.[0] === 'A_Operator' && isSymbolicBinaryOperator(token[1])
}
export function assertA_BinaryOperatorToken(token: Token | undefined): asserts token is A_OperatorToken<SymbolicBinaryOperator> {
  if (!isA_BinaryOperatorToken(token)) {
    throwUnexpectedToken('A_Operator', token)
  }
}
export function asA_BinaryOperatorToken(token: Token | undefined): A_OperatorToken<SymbolicBinaryOperator> {
  assertA_BinaryOperatorToken(token)
  return token
}

export function isA_BinaryFunctionSymbolToken(token: Token | undefined): token is A_SymbolToken<BinaryFunctionOperator> {
  return token?.[0] === 'A_Symbol' && isBinaryFunctionOperator(token[1])
}
export function assertA_BinaryFunctionSymbolToken(token: Token | undefined): asserts token is A_SymbolToken {
  if (!isA_SymbolToken(token)) {
    throwUnexpectedToken('A_Symbol', token)
  }
}
export function asA_BinaryFunctionSymbolToken(token: Token | undefined): A_SymbolToken {
  assertA_SymbolToken(token)
  return token
}

export function isA_UnaryFunctionSymbolToken(token: Token | undefined): token is A_SymbolToken<UnaryFunctionOperator> {
  return token?.[0] === 'A_Symbol' && isUnaryFunctionOperator(token[1])
}
export function assertA_UnaryFunctionSymbolToken(token: Token | undefined): asserts token is A_SymbolToken {
  if (!isA_SymbolToken(token)) {
    throwUnexpectedToken('A_Symbol', token)
  }
}
export function asA_UnaryFunctionSymbolToken(token: Token | undefined): A_SymbolToken {
  assertA_SymbolToken(token)
  return token
}

export function isA_ReservedSymbolToken(token: Token | undefined): token is A_ReservedSymbolToken {
  return token?.[0] === 'A_ReservedSymbol'
}
export function assertA_ReservedSymbolToken(token: Token | undefined): asserts token is A_ReservedSymbolToken {
  if (!isA_ReservedSymbolToken(token)) {
    throwUnexpectedToken('A_ReservedSymbol', token)
  }
}
export function asA_ReservedSymbolToken(token: Token | undefined): A_ReservedSymbolToken {
  assertA_ReservedSymbolToken(token)
  return token
}

export function isA_CommentToken(token: Token | undefined): token is A_SingleLineCommentToken {
  return token?.[0] === 'A_SingleLineComment'
}
export function assertA_CommentToken(token: Token | undefined): asserts token is A_SingleLineCommentToken {
  if (!isA_CommentToken(token)) {
    throwUnexpectedToken('A_SingleLineComment', token)
  }
}
export function asA_CommentToken(token: Token | undefined): A_SingleLineCommentToken {
  assertA_CommentToken(token)
  return token
}

export function isA_MultiLineCommentToken(token: Token | undefined): token is A_MultiLineCommentToken {
  return token?.[0] === 'A_MultiLineComment'
}
export function assertA_MultiLineCommentToken(token: Token | undefined): asserts token is A_MultiLineCommentToken {
  if (!isA_MultiLineCommentToken(token)) {
    throwUnexpectedToken('A_MultiLineComment', token)
  }
}
export function asA_MultiLineCommentToken(token: Token | undefined): A_MultiLineCommentToken {
  assertA_MultiLineCommentToken(token)
  return token
}

export function isA_OperatorToken<T extends SymbolicOperator>(token: Token | undefined, operatorName?: T): token is A_OperatorToken<T> {
  if (token?.[0] !== 'A_Operator') {
    return false
  }
  if (operatorName && token[1] !== operatorName) {
    return false
  }
  return true
}
export function assertA_OperatorToken<T extends SymbolicOperator>(token: Token | undefined, operatorName?: T): asserts token is A_OperatorToken<T> {
  if (!isA_OperatorToken(token, operatorName)) {
    if (operatorName) {
      throw new LitsError(`Unexpected token: ${token}, expected operator ${operatorName}`, undefined)
    }
    throwUnexpectedToken('A_Operator', token)
  }
}
export function asA_OperatorToken<T extends SymbolicOperator>(token: Token | undefined, operatorName?: T): A_OperatorToken<T> {
  assertA_OperatorToken(token, operatorName)
  return token
}

export function isA_WhitespaceToken(token: Token | undefined): token is A_WhitespaceToken {
  return token?.[0] === 'A_Whitespace'
}
export function assertA_WhitespaceToken(token: Token | undefined): asserts token is A_WhitespaceToken {
  if (!isA_WhitespaceToken(token)) {
    throwUnexpectedToken('A_Whitespace', token)
  }
}
export function asA_WhitespaceToken(token: Token | undefined): A_WhitespaceToken {
  assertA_WhitespaceToken(token)
  return token
}

export function isA_NumberToken(token: Token | undefined): token is A_NumberToken {
  return token?.[0] === 'A_Number'
}
export function assertA_NumberToken(token: Token | undefined): asserts token is A_NumberToken {
  if (!isA_NumberToken(token)) {
    throwUnexpectedToken('A_Number', token)
  }
}
export function asA_NumberToken(token: Token | undefined): A_NumberToken {
  assertA_NumberToken(token)
  return token
}
