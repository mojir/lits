import { isLitsFunction } from '../../../typeGuards/litsFunction'
import { assertColl, isColl, isObj, isRegularExpression, isSeq } from '../../../typeGuards/lits'
import { assertNumber, isNumber } from '../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../interface'
import { isGrid, isMatrix, isVector } from '../../../typeGuards/annotatedArrays'

export const predicatesNormalExpression: BuiltinNormalExpressions = {
  'function?': {
    evaluate: ([first]): boolean => isLitsFunction(first),
    paramCount: 1,
  },

  'string?': {
    evaluate: ([first]): boolean => typeof first === 'string',
    paramCount: 1,
  },

  'number?': {
    evaluate: ([first]): boolean => typeof first === 'number',
    paramCount: 1,
  },

  'integer?': {
    evaluate: ([first]): boolean => typeof first === 'number' && isNumber(first, { integer: true }),
    paramCount: 1,
  },

  'boolean?': {
    evaluate: ([first]): boolean => typeof first === 'boolean',
    paramCount: 1,
  },

  'null?': {
    evaluate: ([first]): boolean => first === null || first === undefined,
    paramCount: 1,
  },

  'zero?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertNumber(first, sourceCodeInfo, { finite: true })
      return first === 0
    },
    paramCount: 1,
  },

  'pos?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertNumber(first, sourceCodeInfo, { finite: true })
      return first > 0
    },
    paramCount: 1,
  },

  'neg?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertNumber(first, sourceCodeInfo, { finite: true })
      return first < 0
    },
    paramCount: 1,
  },

  'even?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertNumber(first, sourceCodeInfo, { finite: true })
      return first % 2 === 0
    },
    paramCount: 1,
  },

  'odd?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertNumber(first, sourceCodeInfo, { finite: true })
      return isNumber(first, { integer: true }) && first % 2 !== 0
    },
    paramCount: 1,
  },

  'array?': {
    evaluate: ([first]): boolean => {
      return Array.isArray(first)
    },
    paramCount: 1,
  },

  'coll?': {
    evaluate: ([first]): boolean => {
      return isColl(first)
    },
    paramCount: 1,
  },

  'seq?': {
    evaluate: ([first]): boolean => {
      return isSeq(first)
    },
    paramCount: 1,
  },

  'object?': {
    evaluate: ([first]): boolean => isObj(first),
    paramCount: 1,
  },

  'regexp?': {
    evaluate: ([value]): boolean => isRegularExpression(value),
    paramCount: 1,
  },

  'finite?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo)
      return Number.isFinite(value)
    },
    paramCount: 1,
  },

  'nan?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo)
      return Number.isNaN(value)
    },
    paramCount: 1,
  },

  'positive-infinity?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo)
      return value === Number.POSITIVE_INFINITY
    },
    paramCount: 1,
  },

  'negative-infinity?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo)
      return value === Number.NEGATIVE_INFINITY
    },
    paramCount: 1,
  },

  'true?': {
    evaluate: ([value]): boolean => {
      return value === true
    },
    paramCount: 1,
  },

  'false?': {
    evaluate: ([value]): boolean => {
      return value === false
    },
    paramCount: 1,
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
    paramCount: 1,
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
    paramCount: 1,
  },
  'vector?': {
    evaluate: ([vector]): boolean => isVector(vector),
    paramCount: 1,
  },
  'matrix?': {
    evaluate: ([matrix]): boolean => isMatrix(matrix),
    paramCount: 1,
  },
  'grid?': {
    evaluate: ([table]): boolean => isGrid(table),
    paramCount: 1,
  },

}
