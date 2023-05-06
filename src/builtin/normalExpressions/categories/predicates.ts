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
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `function?`, debugInfo),
  },

  'string?': {
    evaluate: ([first]): boolean => typeof first === `string`,
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `string?`, debugInfo),
  },

  'number?': {
    evaluate: ([first]): boolean => typeof first === `number`,
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `number?`, debugInfo),
  },

  'integer?': {
    evaluate: ([first]): boolean => typeof first === `number` && number.is(first, { integer: true }),
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `integer?`, debugInfo),
  },

  'boolean?': {
    evaluate: ([first]): boolean => typeof first === `boolean`,
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `boolean?`, debugInfo),
  },

  'nil?': {
    evaluate: ([first]): boolean => first === null || first === undefined,
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `nil?`, debugInfo),
  },

  'zero?': {
    evaluate: ([first], debugInfo): boolean => {
      number.assert(first, debugInfo, { finite: true })
      return first === 0
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `zero?`, debugInfo),
  },

  'pos?': {
    evaluate: ([first], debugInfo): boolean => {
      number.assert(first, debugInfo, { finite: true })
      return first > 0
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `pos?`, debugInfo),
  },

  'neg?': {
    evaluate: ([first], debugInfo): boolean => {
      number.assert(first, debugInfo, { finite: true })
      return first < 0
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `neg?`, debugInfo),
  },

  'even?': {
    evaluate: ([first], debugInfo): boolean => {
      number.assert(first, debugInfo, { finite: true })
      return first % 2 === 0
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `even?`, debugInfo),
  },

  'odd?': {
    evaluate: ([first], debugInfo): boolean => {
      number.assert(first, debugInfo, { finite: true })
      return number.is(first, { integer: true }) && first % 2 !== 0
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `odd?`, debugInfo),
  },

  'array?': {
    evaluate: ([first]): boolean => {
      return array.is(first)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `array?`, debugInfo),
  },

  'coll?': {
    evaluate: ([first]): boolean => {
      return collection.is(first)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `coll?`, debugInfo),
  },

  'seq?': {
    evaluate: ([first]): boolean => {
      return sequence.is(first)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `seq?`, debugInfo),
  },

  'object?': {
    evaluate: ([first]): boolean => object.is(first),
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `object?`, debugInfo),
  },

  'regexp?': {
    evaluate: ([value]): boolean => isRegularExpression(value),
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `regexp?`, debugInfo),
  },

  'finite?': {
    evaluate: ([value], debugInfo): boolean => {
      number.assert(value, debugInfo)
      return Number.isFinite(value)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `finite?`, debugInfo),
  },

  'nan?': {
    evaluate: ([value], debugInfo): boolean => {
      number.assert(value, debugInfo)
      return Number.isNaN(value)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `nan?`, debugInfo),
  },

  'true?': {
    evaluate: ([value]): boolean => {
      return value === true
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `true?`, debugInfo),
  },

  'false?': {
    evaluate: ([value]): boolean => {
      return value === false
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `false?`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `empty?`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `not-empty?`, debugInfo),
  },
}
