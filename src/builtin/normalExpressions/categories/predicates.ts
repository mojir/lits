import { NormalExpressionNode } from '../../../parser/interface'
import { assertLength, assertFiniteNumber } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
import { collection, litsFunction, number, object, sequence, array } from '../../../utils/assertion'

export const predicatesNormalExpression: BuiltinNormalExpressions = {
  'function?': {
    evaluate: ([first]): boolean => litsFunction.is(first),
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'string?': {
    evaluate: ([first]): boolean => typeof first === `string`,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'number?': {
    evaluate: ([first]): boolean => typeof first === `number`,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'integer?': {
    evaluate: ([first]): boolean => typeof first === `number` && number.is(first, { integer: true }),
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'boolean?': {
    evaluate: ([first]): boolean => typeof first === `boolean`,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'nil?': {
    evaluate: ([first]): boolean => first === null || first === undefined,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'zero?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertFiniteNumber(first, sourceCodeInfo)
      return first === 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'pos?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertFiniteNumber(first, sourceCodeInfo)
      return first > 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'neg?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertFiniteNumber(first, sourceCodeInfo)
      return first < 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'even?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertFiniteNumber(first, sourceCodeInfo)
      return first % 2 === 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'odd?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertFiniteNumber(first, sourceCodeInfo)
      return number.is(first, { integer: true }) && first % 2 !== 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'array?': {
    evaluate: ([first]): boolean => {
      return array.is(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'coll?': {
    evaluate: ([first]): boolean => {
      return collection.is(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'seq?': {
    evaluate: ([first]): boolean => {
      return sequence.is(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'object?': {
    evaluate: ([first]): boolean => object.is(first),
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'regexp?': {
    evaluate: ([first]): boolean =>
      first !== null && !Array.isArray(first) && typeof first === `object` && first instanceof RegExp,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'finite?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      number.assert(value, sourceCodeInfo)
      return Number.isFinite(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'nan?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      number.assert(value, sourceCodeInfo)
      return Number.isNaN(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'positive-infinity?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      number.assert(value, sourceCodeInfo)
      return value === Number.POSITIVE_INFINITY
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'negative-infinity?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      number.assert(value, sourceCodeInfo)
      return value === Number.NEGATIVE_INFINITY
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'true?': {
    evaluate: ([value]): boolean => {
      return value === true
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'false?': {
    evaluate: ([value]): boolean => {
      return value === false
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },
}
