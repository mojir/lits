import { LitsError } from '../errors'
import type { A_OperatorToken, Token } from './tokens'
import { throwUnexpectedToken } from './utils'

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
