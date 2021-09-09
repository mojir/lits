import { isLispishFunction } from '../../utils'
import { NormalExpressionNode } from '../../parser/interface'
import { assertLengthOne, assertNumber } from '../../utils'
import { BuiltinNormalExpressions } from './interface'

export const predicates: BuiltinNormalExpressions = {
  'function?': {
    evaluate: ([first]: unknown[]): boolean => isLispishFunction(first),
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'string?': {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'string',
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'number?': {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'number',
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'integer?': {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'number' && Number.isInteger(first),
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'boolean?': {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'boolean',
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'undefined?': {
    evaluate: ([first]: unknown[]): boolean => first === undefined,
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'null?': {
    evaluate: ([first]: unknown[]): boolean => first === null,
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'zero?': {
    evaluate: ([first]: unknown[]): boolean => {
      assertNumber(first)
      return first === 0
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'even?': {
    evaluate: ([first]: unknown[]): boolean => {
      assertNumber(first)
      return first % 2 === 0
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'odd?': {
    evaluate: ([first]: unknown[]): boolean => {
      assertNumber(first)
      return Number.isInteger(first) && first % 2 !== 0
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'list?': {
    evaluate: ([first]: unknown[]): boolean => {
      return Array.isArray(first)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'object?': {
    evaluate: ([first]: unknown[]): boolean =>
      first !== null &&
      !Array.isArray(first) &&
      typeof first === 'object' &&
      !(first instanceof RegExp) &&
      !isLispishFunction(first),
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'regexp?': {
    evaluate: ([first]: unknown[]): boolean =>
      first !== null && !Array.isArray(first) && typeof first === 'object' && first instanceof RegExp,
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },
}
