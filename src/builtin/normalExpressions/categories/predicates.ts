import { assertArray, isLispishFunction } from '../../../utils'
import { NormalExpressionNode } from '../../../parser/interface'
import { assertLength, assertFiniteNumber } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const predicatesNormalExpression: BuiltinNormalExpressions = {
  'function?': {
    evaluate: ([first]: unknown[]): boolean => isLispishFunction(first),
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'string?': {
    evaluate: ([first]: unknown[]): boolean => typeof first === `string`,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'number?': {
    evaluate: ([first]: unknown[]): boolean => typeof first === `number`,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'integer?': {
    evaluate: ([first]: unknown[]): boolean => typeof first === `number` && Number.isInteger(first),
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'boolean?': {
    evaluate: ([first]: unknown[]): boolean => typeof first === `boolean`,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'undefined?': {
    evaluate: ([first]: unknown[]): boolean => first === undefined,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'null?': {
    evaluate: ([first]: unknown[]): boolean => first === null,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'zero?': {
    evaluate: ([first]: unknown[]): boolean => {
      assertFiniteNumber(first)
      return first === 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'even?': {
    evaluate: ([first]: unknown[]): boolean => {
      assertFiniteNumber(first)
      return first % 2 === 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'odd?': {
    evaluate: ([first]: unknown[]): boolean => {
      assertFiniteNumber(first)
      return Number.isInteger(first) && first % 2 !== 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'array?': {
    evaluate: ([first]: unknown[]): boolean => {
      return Array.isArray(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'object?': {
    evaluate: ([first]: unknown[]): boolean =>
      first !== null &&
      !Array.isArray(first) &&
      typeof first === `object` &&
      !(first instanceof RegExp) &&
      !isLispishFunction(first),
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'regexp?': {
    evaluate: ([first]: unknown[]): boolean =>
      first !== null && !Array.isArray(first) && typeof first === `object` && first instanceof RegExp,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'empty?': {
    evaluate: ([first]: unknown[]): boolean => {
      assertArray(first)
      return first.length === 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },
}
