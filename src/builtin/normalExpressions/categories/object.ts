import { Any, Arr, Obj } from '../../../interface'
import { collHasKey, toAny } from '../../../utils'
import {
  litsFunction,
  object,
  array,
  string,
  assertEventNumberOfParams,
  assertNumberOfParams,
  stringArray,
} from '../../../utils/assertion'
import { BuiltinNormalExpressions } from '../../interface'

export const objectNormalExpression: BuiltinNormalExpressions = {
  object: {
    evaluate: (params, debugInfo): Obj => {
      const result: Obj = {}
      for (let i = 0; i < params.length; i += 2) {
        const key = params[i]
        const value = params[i + 1]
        string.assert(key, debugInfo)
        result[key] = value
      }
      return result
    },
    validate: node => assertEventNumberOfParams(node),
  },

  keys: {
    evaluate: ([first], debugInfo): string[] => {
      object.assert(first, debugInfo)
      return Object.keys(first)
    },
    validate: node => assertNumberOfParams(1, node),
  },

  vals: {
    evaluate: ([first], debugInfo): Arr => {
      object.assert(first, debugInfo)
      return Object.values(first)
    },
    validate: node => assertNumberOfParams(1, node),
  },

  entries: {
    evaluate: ([first], debugInfo): Array<[string, unknown]> => {
      object.assert(first, debugInfo)
      return Object.entries(first)
    },
    validate: node => assertNumberOfParams(1, node),
  },

  find: {
    evaluate: ([obj, key], debugInfo): [string, unknown] | null => {
      object.assert(obj, debugInfo)
      string.assert(key, debugInfo)
      if (collHasKey(obj, key)) {
        return [key, obj[key]]
      }
      return null
    },
    validate: node => assertNumberOfParams(2, node),
  },

  dissoc: {
    evaluate: ([obj, key], debugInfo): Any => {
      object.assert(obj, debugInfo)
      string.assert(key, debugInfo)
      const newObj = { ...obj }
      delete newObj[key]
      return newObj
    },
    validate: node => assertNumberOfParams(2, node),
  },

  merge: {
    evaluate: (params, debugInfo): Any => {
      if (params.length === 0) {
        return null
      }
      const [first, ...rest] = params
      object.assert(first, debugInfo)

      return rest.reduce(
        (result: Obj, obj) => {
          object.assert(obj, debugInfo)
          return { ...result, ...obj }
        },
        { ...first },
      )
    },
    validate: node => assertNumberOfParams({ min: 0 }, node),
  },

  'merge-with': {
    evaluate: (params: Arr, debugInfo, contextStack, { executeFunction }): Any => {
      const [fn, first, ...rest] = params
      litsFunction.assert(fn, debugInfo)

      if (params.length === 1) {
        return null
      }

      object.assert(first, debugInfo)

      return rest.reduce(
        (result: Obj, obj) => {
          object.assert(obj, debugInfo)
          Object.entries(obj).forEach(entry => {
            const key = string.as(entry[0], debugInfo)
            const val = toAny(entry[1])
            if (collHasKey(result, key)) {
              result[key] = executeFunction(fn, [result[key], val], contextStack, debugInfo)
            } else {
              result[key] = val
            }
          })
          return result
        },
        { ...first },
      )
    },
    validate: node => assertNumberOfParams({ min: 1 }, node),
  },

  zipmap: {
    evaluate: ([keys, values], debugInfo): Any => {
      stringArray.assert(keys, debugInfo)
      array.assert(values, debugInfo)

      const length = Math.min(keys.length, values.length)

      const result: Obj = {}

      for (let i = 0; i < length; i += 1) {
        const key = string.as(keys[i], debugInfo)
        result[key] = toAny(values[i])
      }
      return result
    },
    validate: node => assertNumberOfParams(2, node),
  },

  'select-keys': {
    evaluate: ([obj, keys], debugInfo): Any => {
      stringArray.assert(keys, debugInfo)
      object.assert(obj, debugInfo)

      return keys.reduce((result: Obj, key) => {
        if (collHasKey(obj, key)) {
          result[key] = toAny(obj[key]) as Any
        }
        return result
      }, {})
    },
    validate: node => assertNumberOfParams(2, node),
  },
}
