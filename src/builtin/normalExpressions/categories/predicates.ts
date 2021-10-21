import { assertNumber, isArr, isColl, isLispishFunction, isObj, isSeq } from '../../../utils'
import { NormalExpressionNode } from '../../../parser/interface'
import { assertLength, assertFiniteNumber } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
import { Arr } from '../../../interface'

export const predicatesNormalExpression: BuiltinNormalExpressions = {
  'function?': {
    evaluate: ([first]: Arr): boolean => isLispishFunction(first),
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'string?': {
    evaluate: ([first]: Arr): boolean => typeof first === `string`,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'number?': {
    evaluate: ([first]: Arr): boolean => typeof first === `number`,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'integer?': {
    evaluate: ([first]: Arr): boolean => typeof first === `number` && Number.isInteger(first),
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'boolean?': {
    evaluate: ([first]: Arr): boolean => typeof first === `boolean`,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'nil?': {
    evaluate: ([first]: Arr): boolean => first === null || first === undefined,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'zero?': {
    evaluate: ([first]: Arr): boolean => {
      assertFiniteNumber(first)
      return first === 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'pos?': {
    evaluate: ([first]: Arr): boolean => {
      assertFiniteNumber(first)
      return first > 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'neg?': {
    evaluate: ([first]: Arr): boolean => {
      assertFiniteNumber(first)
      return first < 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'even?': {
    evaluate: ([first]: Arr): boolean => {
      assertFiniteNumber(first)
      return first % 2 === 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'odd?': {
    evaluate: ([first]: Arr): boolean => {
      assertFiniteNumber(first)
      return Number.isInteger(first) && first % 2 !== 0
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'array?': {
    evaluate: ([first]: Arr): boolean => {
      return isArr(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'coll?': {
    evaluate: ([first]: Arr): boolean => {
      return isColl(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'seq?': {
    evaluate: ([first]: Arr): boolean => {
      return isSeq(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'object?': {
    evaluate: ([first]: Arr): boolean => isObj(first),
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'regexp?': {
    evaluate: ([first]: Arr): boolean =>
      first !== null && !Array.isArray(first) && typeof first === `object` && first instanceof RegExp,
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'finite?': {
    evaluate: ([value]: Arr): boolean => {
      assertNumber(value)
      return Number.isFinite(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'nan?': {
    evaluate: ([value]: Arr): boolean => {
      assertNumber(value)
      return Number.isNaN(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'positive-infinity?': {
    evaluate: ([value]: Arr): boolean => {
      assertNumber(value)
      return value === Number.POSITIVE_INFINITY
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'negative-infinity?': {
    evaluate: ([value]: Arr): boolean => {
      assertNumber(value)
      return value === Number.NEGATIVE_INFINITY
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'true?': {
    evaluate: ([value]: Arr): boolean => {
      return value === true
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'false?': {
    evaluate: ([value]: Arr): boolean => {
      return value === false
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },
}
