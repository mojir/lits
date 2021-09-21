import { assertArray, isLispishFunction } from '../../../utils'
import { NormalExpressionNode } from '../../../parser/interface'
import { assertLength, assertFiniteNumber } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const predicatesNormalExpression: BuiltinNormalExpressions = {
  'function?': {
    evaluate: ([first]: unknown[]): boolean => isLispishFunction(first),
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'string?': {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'string',
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'number?': {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'number',
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'integer?': {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'number' && Number.isInteger(first),
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'boolean?': {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'boolean',
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'undefined?': {
    evaluate: ([first]: unknown[]): boolean => first === undefined,
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'null?': {
    evaluate: ([first]: unknown[]): boolean => first === null,
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'zero?': {
    evaluate: ([first]: unknown[]): boolean => {
      assertFiniteNumber(first)
      return first === 0
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'even?': {
    evaluate: ([first]: unknown[]): boolean => {
      assertFiniteNumber(first)
      return first % 2 === 0
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'odd?': {
    evaluate: ([first]: unknown[]): boolean => {
      assertFiniteNumber(first)
      return Number.isInteger(first) && first % 2 !== 0
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'list?': {
    evaluate: ([first]: unknown[]): boolean => {
      return Array.isArray(first)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'object?': {
    evaluate: ([first]: unknown[]): boolean =>
      first !== null &&
      !Array.isArray(first) &&
      typeof first === 'object' &&
      !(first instanceof RegExp) &&
      !isLispishFunction(first),
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'regexp?': {
    evaluate: ([first]: unknown[]): boolean =>
      first !== null && !Array.isArray(first) && typeof first === 'object' && first instanceof RegExp,
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'empty?': {
    evaluate: ([first]: unknown[]): boolean => {
      assertArray(first)
      return first.length === 0
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },
}
