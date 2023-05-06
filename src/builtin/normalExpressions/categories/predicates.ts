import { NormalExpressionNode } from '../../../parser/interface'
import { BuiltinNormalExpressions } from '../../interface'
import {
  collection,
  litsFunction,
  number,
  object,
  sequence,
  array,
  assertNumberOfParams,
  string,
} from '../../../utils/assertion'
import { isRegularExpression } from '../../../utils/helpers'

export const predicatesNormalExpression: BuiltinNormalExpressions = {
  'function?': {
    evaluate: ([first]): boolean => litsFunction.is(first),
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'string?': {
    evaluate: ([first]): boolean => typeof first === `string`,
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'number?': {
    evaluate: ([first]): boolean => typeof first === `number`,
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'integer?': {
    evaluate: ([first]): boolean => typeof first === `number` && number.is(first, { integer: true }),
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'boolean?': {
    evaluate: ([first]): boolean => typeof first === `boolean`,
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'nil?': {
    evaluate: ([first]): boolean => first === null || first === undefined,
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'zero?': {
    evaluate: ([first], debugInfo): boolean => {
      number.assert(first, debugInfo, { finite: true })
      return first === 0
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'pos?': {
    evaluate: ([first], debugInfo): boolean => {
      number.assert(first, debugInfo, { finite: true })
      return first > 0
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'neg?': {
    evaluate: ([first], debugInfo): boolean => {
      number.assert(first, debugInfo, { finite: true })
      return first < 0
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'even?': {
    evaluate: ([first], debugInfo): boolean => {
      number.assert(first, debugInfo, { finite: true })
      return first % 2 === 0
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'odd?': {
    evaluate: ([first], debugInfo): boolean => {
      number.assert(first, debugInfo, { finite: true })
      return number.is(first, { integer: true }) && first % 2 !== 0
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'array?': {
    evaluate: ([first]): boolean => {
      return array.is(first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'coll?': {
    evaluate: ([first]): boolean => {
      return collection.is(first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'seq?': {
    evaluate: ([first]): boolean => {
      return sequence.is(first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'object?': {
    evaluate: ([first]): boolean => object.is(first),
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'regexp?': {
    evaluate: ([value]): boolean => isRegularExpression(value),
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'finite?': {
    evaluate: ([value], debugInfo): boolean => {
      number.assert(value, debugInfo)
      return Number.isFinite(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'nan?': {
    evaluate: ([value], debugInfo): boolean => {
      number.assert(value, debugInfo)
      return Number.isNaN(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'positive-infinity?': {
    evaluate: ([value], debugInfo): boolean => {
      number.assert(value, debugInfo)
      return value === Number.POSITIVE_INFINITY
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'negative-infinity?': {
    evaluate: ([value], debugInfo): boolean => {
      number.assert(value, debugInfo)
      return value === Number.NEGATIVE_INFINITY
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'true?': {
    evaluate: ([value]): boolean => {
      return value === true
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'false?': {
    evaluate: ([value]): boolean => {
      return value === false
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'empty?': {
    evaluate: ([coll], debugInfo): boolean => {
      collection.assert(coll, debugInfo)
      if (string.is(coll)) {
        return coll.length === 0
      }
      if (Array.isArray(coll)) {
        return coll.length === 0
      }
      return Object.keys(coll).length === 0
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'not-empty?': {
    evaluate: ([coll], debugInfo): boolean => {
      collection.assert(coll, debugInfo)
      if (string.is(coll)) {
        return coll.length > 0
      }
      if (Array.isArray(coll)) {
        return coll.length > 0
      }
      return Object.keys(coll).length > 0
    },
    validate: node => assertNumberOfParams(1, node),
  },
}
