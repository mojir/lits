import { assertNumber, isArr, isColl, isLitsFunction, isObj, isSeq } from '../../../utils'
import { NormalExpressionNode } from '../../../parser/interface'
import { assertLength, assertFiniteNumber } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const predicatesNormalExpression: BuiltinNormalExpressions = {
  'function?': {
    evaluate: ([first]): boolean => isLitsFunction(first),
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
    evaluate: ([first]): boolean => typeof first === `number` && Number.isInteger(first),
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
    evaluate: ([first], meta): boolean => {
      assertFiniteNumber(first, meta)
      return first === 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'pos?': {
    evaluate: ([first], meta): boolean => {
      assertFiniteNumber(first, meta)
      return first > 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'neg?': {
    evaluate: ([first], meta): boolean => {
      assertFiniteNumber(first, meta)
      return first < 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'even?': {
    evaluate: ([first], meta): boolean => {
      assertFiniteNumber(first, meta)
      return first % 2 === 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'odd?': {
    evaluate: ([first], meta): boolean => {
      assertFiniteNumber(first, meta)
      return Number.isInteger(first) && first % 2 !== 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'array?': {
    evaluate: ([first]): boolean => {
      return isArr(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'coll?': {
    evaluate: ([first]): boolean => {
      return isColl(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'seq?': {
    evaluate: ([first]): boolean => {
      return isSeq(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'object?': {
    evaluate: ([first]): boolean => isObj(first),
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'regexp?': {
    evaluate: ([first]): boolean =>
      first !== null && !Array.isArray(first) && typeof first === `object` && first instanceof RegExp,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'finite?': {
    evaluate: ([value], meta): boolean => {
      assertNumber(value, meta)
      return Number.isFinite(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'nan?': {
    evaluate: ([value], meta): boolean => {
      assertNumber(value, meta)
      return Number.isNaN(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'positive-infinity?': {
    evaluate: ([value], meta): boolean => {
      assertNumber(value, meta)
      return value === Number.POSITIVE_INFINITY
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'negative-infinity?': {
    evaluate: ([value], meta): boolean => {
      assertNumber(value, meta)
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
