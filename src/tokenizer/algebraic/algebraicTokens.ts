import { LitsError } from '../../errors'
import type { CommonToken, CommonTokenType } from '../common/commonTokens'
import { commonTokenTypes } from '../common/commonTokens'
import type { Token } from '../tokens'
import { type TokenDebugData, throwUnexpectedToken } from '../utils'
import type { ValidReservedSymbol } from './algebraicReservedNames'

export const algebraicOnlyTokenTypes = [
  'A_Whitespace',
  'A_Operator',
  'A_Symbol',
  'A_ReservedSymbol',
  'A_SingleLineComment',
  'A_MultiLineComment',
  'A_Number',
  'A_BasePrefixedNumber',
] as const satisfies `A_${string}`[]

export const algebraicTokenTypes = [
  ...commonTokenTypes,
  ...algebraicOnlyTokenTypes,
] as const

const binaryOperators = [
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
  '≤', // less than or equal
  '>', // greater than
  '>=', // greater than or equal
  '≥', // greater than or equal

  '=', // equal
  '!=', // not equal
  '≠', // not equal

  '&', // bitwise AND
  '^', // bitwise XOR
  '|', // bitwise OR

  '&&', // logical AND
  '||', // logical OR
  '??', // nullish coalescing
] as const

const otherOperators = [
  '->', // lambda
  '...', // rest
  '.', // property accessor
  ',', // item separator
  ':=', // property assignment
  ';', // statement terminator
] as const

const symbolicOperators = [
  ...binaryOperators,
  ...otherOperators,
] as const

const nonFunctionOperators = [
  '??',
  '&&',
  '||',
  'comment',
  'cond',
  'def',
  'defined?',
  'defn',
  'do',
  'doseq',
  'fn',
  'if',
  'let',
  'loop',
  'recur',
  'throw',
  'try',
  'unless',
  'while',
]

const nonFunctionOperatorSet = new Set(nonFunctionOperators)
export function isFunctionOperator(operator: string): boolean {
  return !nonFunctionOperatorSet.has(operator)
}

export type SymbolicBinaryOperator = typeof binaryOperators[number]
export type SymbolicOperator = typeof symbolicOperators[number]

const binaryOperatorSet = new Set(binaryOperators)
export function isBinaryOperator(operator: string): operator is SymbolicBinaryOperator {
  return binaryOperatorSet.has(operator as SymbolicBinaryOperator)
}
export function assertBinaryOperator(operator: string): asserts operator is SymbolicBinaryOperator {
  if (!isBinaryOperator(operator)) {
    throw new LitsError(`Expected symbolic binary operator, got ${operator}`, undefined)
  }
}
export function asBinaryOperator(operator: string): SymbolicBinaryOperator {
  assertBinaryOperator(operator)
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
export type AlgebraicTokenType = typeof algebraicTokenTypes[number]

type GenericAlgebraicValueToken<T extends Exclude<AlgebraicTokenType, CommonTokenType>, V extends string = string> = [T, V] | [T, V, TokenDebugData]

export type A_WhitespaceToken = GenericAlgebraicValueToken<'A_Whitespace'>
export type A_NumberToken = GenericAlgebraicValueToken<'A_Number'>
export type A_BasePrefixedNumberToken = GenericAlgebraicValueToken<'A_BasePrefixedNumber'>
export type A_OperatorToken<T extends SymbolicOperator = SymbolicOperator> = GenericAlgebraicValueToken<'A_Operator', T>
export type A_SymbolToken<T extends string = string> = GenericAlgebraicValueToken<'A_Symbol', T>
export type A_ReservedSymbolToken<T extends ValidReservedSymbol = ValidReservedSymbol> = GenericAlgebraicValueToken<'A_ReservedSymbol', T>
export type A_SingleLineCommentToken = GenericAlgebraicValueToken<'A_SingleLineComment'>
export type A_MultiLineCommentToken = GenericAlgebraicValueToken<'A_MultiLineComment'>

export type AlgebraicOnlyToken =
  | A_WhitespaceToken
  | A_NumberToken
  | A_BasePrefixedNumberToken
  | A_OperatorToken
  | A_SymbolToken
  | A_ReservedSymbolToken
  | A_SingleLineCommentToken
  | A_MultiLineCommentToken

export type AlgebraicToken =
  | AlgebraicOnlyToken
  | CommonToken

export function isA_SymbolToken<T extends string>(token: Token | undefined, symbolName?: T): token is A_SymbolToken<T> {
  if (token?.[0] !== 'A_Symbol') {
    return false
  }
  if (symbolName && token[1] !== symbolName) {
    return false
  }
  return true
}

export function assertA_SymbolToken<T extends string>(token: Token | undefined, symbolName?: T): asserts token is A_SymbolToken<T> {
  if (!isA_SymbolToken(token, symbolName)) {
    throwUnexpectedToken('A_Symbol', undefined, token)
  }
}
export function asA_SymbolToken<T extends string>(token: Token | undefined, symbolName?: T): A_SymbolToken<T> {
  assertA_SymbolToken(token, symbolName)
  return token
}

export function isA_BinaryOperatorToken(token: Token | undefined): token is A_OperatorToken<SymbolicBinaryOperator> {
  return token?.[0] === 'A_Operator' && isBinaryOperator(token[1])
}
export function assertA_BinaryOperatorToken(token: Token | undefined): asserts token is A_OperatorToken<SymbolicBinaryOperator> {
  if (!isA_BinaryOperatorToken(token)) {
    throwUnexpectedToken('A_Operator', undefined, token)
  }
}
export function asA_BinaryOperatorToken(token: Token | undefined): A_OperatorToken<SymbolicBinaryOperator> {
  assertA_BinaryOperatorToken(token)
  return token
}

export function isA_ReservedSymbolToken<T extends ValidReservedSymbol>(token: Token | undefined, symbolName?: T): token is A_ReservedSymbolToken<T> {
  if (token?.[0] !== 'A_ReservedSymbol') {
    return false
  }
  if (symbolName && token[1] !== symbolName) {
    return false
  }
  return true
}
export function assertA_ReservedSymbolToken<T extends ValidReservedSymbol>(token: Token | undefined, symbolName?: T): asserts token is A_ReservedSymbolToken<T> {
  if (!isA_ReservedSymbolToken(token, symbolName)) {
    throwUnexpectedToken('A_ReservedSymbol', symbolName, token)
  }
}
export function asA_ReservedSymbolToken<T extends ValidReservedSymbol>(token: Token | undefined, symbolName?: T): A_ReservedSymbolToken<T> {
  assertA_ReservedSymbolToken(token, symbolName)
  return token
}

export function isA_CommentToken(token: Token | undefined): token is A_SingleLineCommentToken {
  return token?.[0] === 'A_SingleLineComment'
}
export function assertA_CommentToken(token: Token | undefined): asserts token is A_SingleLineCommentToken {
  if (!isA_CommentToken(token)) {
    throwUnexpectedToken('A_SingleLineComment', undefined, token)
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
    throwUnexpectedToken('A_MultiLineComment', undefined, token)
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
    throwUnexpectedToken('A_Operator', operatorName, token)
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
    throwUnexpectedToken('A_Whitespace', undefined, token)
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
    throwUnexpectedToken('A_Number', undefined, token)
  }
}
export function asA_NumberToken(token: Token | undefined): A_NumberToken {
  assertA_NumberToken(token)
  return token
}

export function isA_BasePrefixedNumberToken(token: Token | undefined): token is A_BasePrefixedNumberToken {
  return token?.[0] === 'A_BasePrefixedNumber'
}
export function assertA_BasePrefixedNumberToken(token: Token | undefined): asserts token is A_BasePrefixedNumberToken {
  if (!isA_BasePrefixedNumberToken(token)) {
    throwUnexpectedToken('A_BasePrefixedNumber', undefined, token)
  }
}
export function asA_BasePrefixedNumberToken(token: Token | undefined): A_BasePrefixedNumberToken {
  assertA_BasePrefixedNumberToken(token)
  return token
}
