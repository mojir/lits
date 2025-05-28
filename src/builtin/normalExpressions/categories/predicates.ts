import { isLitsFunction } from '../../../typeGuards/litsFunction'
import { assertColl, isColl, isObj, isRegularExpression, isSeq } from '../../../typeGuards/lits'
import { assertNumber, isNumber } from '../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../interface'
import { isGrid, isMatrix, isVector } from '../../../typeGuards/annotatedArrays'
import { EPSILON } from '../../../utils'
import { toFixedArity } from '../../../utils/arity'

export const predicatesNormalExpression: BuiltinNormalExpressions = {
  'function?': {
    evaluate: ([first]): boolean => isLitsFunction(first),
    arity: toFixedArity(1),
  },

  'string?': {
    evaluate: ([first]): boolean => typeof first === 'string',
    arity: toFixedArity(1),
  },

  'number?': {
    evaluate: ([first]): boolean => typeof first === 'number',
    arity: toFixedArity(1),
  },

  'integer?': {
    evaluate: ([first]): boolean => typeof first === 'number' && isNumber(first, { integer: true }),
    arity: toFixedArity(1),
  },

  'boolean?': {
    evaluate: ([first]): boolean => typeof first === 'boolean',
    arity: toFixedArity(1),
  },

  'null?': {
    evaluate: ([first]): boolean => first === null || first === undefined,
    arity: toFixedArity(1),
  },

  'zero?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo, { finite: true })
      return Math.abs(value) < EPSILON
    },
    arity: toFixedArity(1),
  },

  'pos?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertNumber(first, sourceCodeInfo, { finite: true })
      return first > 0
    },
    arity: toFixedArity(1),
  },

  'neg?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertNumber(first, sourceCodeInfo, { finite: true })
      return first < 0
    },
    arity: toFixedArity(1),
  },

  'even?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertNumber(first, sourceCodeInfo, { finite: true })
      return first % 2 === 0
    },
    arity: toFixedArity(1),
  },

  'odd?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertNumber(first, sourceCodeInfo, { finite: true })
      return isNumber(first, { integer: true }) && first % 2 !== 0
    },
    arity: toFixedArity(1),
  },

  'array?': {
    evaluate: ([first]): boolean => {
      return Array.isArray(first)
    },
    arity: toFixedArity(1),
  },

  'coll?': {
    evaluate: ([first]): boolean => {
      return isColl(first)
    },
    arity: toFixedArity(1),
  },

  'seq?': {
    evaluate: ([first]): boolean => {
      return isSeq(first)
    },
    arity: toFixedArity(1),
  },

  'object?': {
    evaluate: ([first]): boolean => isObj(first),
    arity: toFixedArity(1),
  },

  'regexp?': {
    evaluate: ([value]): boolean => isRegularExpression(value),
    arity: toFixedArity(1),
  },

  'finite?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo)
      return Number.isFinite(value)
    },
    arity: toFixedArity(1),
  },

  'positive-infinity?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo)
      return value === Number.POSITIVE_INFINITY
    },
    arity: toFixedArity(1),
  },

  'negative-infinity?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo)
      return value === Number.NEGATIVE_INFINITY
    },
    arity: toFixedArity(1),
  },

  'true?': {
    evaluate: ([value]): boolean => {
      return value === true
    },
    arity: toFixedArity(1),
  },

  'false?': {
    evaluate: ([value]): boolean => {
      return value === false
    },
    arity: toFixedArity(1),
  },

  'empty?': {
    evaluate: ([coll], sourceCodeInfo): boolean => {
      if (coll === null)
        return true

      assertColl(coll, sourceCodeInfo)
      if (typeof coll === 'string')
        return coll.length === 0

      if (Array.isArray(coll))
        return coll.length === 0

      return Object.keys(coll).length === 0
    },
    arity: toFixedArity(1),
  },
  'not-empty?': {
    evaluate: ([coll], sourceCodeInfo): boolean => {
      if (coll === null)
        return false

      assertColl(coll, sourceCodeInfo)
      if (typeof coll === 'string')
        return coll.length > 0

      if (Array.isArray(coll))
        return coll.length > 0

      return Object.keys(coll).length > 0
    },
    arity: toFixedArity(1),
  },
  'vector?': {
    evaluate: ([vector]): boolean => isVector(vector),
    arity: toFixedArity(1),
  },
  'matrix?': {
    evaluate: ([matrix]): boolean => isMatrix(matrix),
    arity: toFixedArity(1),
  },
  'grid?': {
    evaluate: ([table]): boolean => isGrid(table),
    arity: toFixedArity(1),
  },

}
