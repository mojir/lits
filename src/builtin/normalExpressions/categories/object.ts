import { Any, Arr, Obj } from '../../../interface'
import { assertLengthEven, assertLength, collHasKey, assertStringArray, toAny } from '../../../utils'
import { litsFunction, object, array, string } from '../../../utils/assertion'
import { BuiltinNormalExpressions } from '../../interface'

export const objectNormalExpression: BuiltinNormalExpressions = {
  object: {
    evaluate: (params, sourceCodeInfo): Obj => {
      const result: Obj = {}
      for (let i = 0; i < params.length; i += 2) {
        const key = params[i]
        const value = params[i + 1]
        string.assert(key, sourceCodeInfo)
        result[key] = value
      }
      return result
    },
    validate: node => assertLengthEven(node),
  },

  keys: {
    evaluate: ([first], sourceCodeInfo): string[] => {
      object.assert(first, sourceCodeInfo)
      return Object.keys(first)
    },
    validate: node => assertLength(1, node),
  },

  vals: {
    evaluate: ([first], sourceCodeInfo): Arr => {
      object.assert(first, sourceCodeInfo)
      return Object.values(first)
    },
    validate: node => assertLength(1, node),
  },

  entries: {
    evaluate: ([first], sourceCodeInfo): Array<[string, unknown]> => {
      object.assert(first, sourceCodeInfo)
      return Object.entries(first)
    },
    validate: node => assertLength(1, node),
  },

  find: {
    evaluate: ([obj, key], sourceCodeInfo): [string, unknown] | null => {
      object.assert(obj, sourceCodeInfo)
      string.assert(key, sourceCodeInfo)
      if (collHasKey(obj, key)) {
        return [key, obj[key]]
      }
      return null
    },
    validate: node => assertLength(2, node),
  },

  dissoc: {
    evaluate: ([obj, key], sourceCodeInfo): Any => {
      object.assert(obj, sourceCodeInfo)
      string.assert(key, sourceCodeInfo)
      const result = toAny(obj[key])
      delete obj[key]
      return result
    },
    validate: node => assertLength(2, node),
  },

  merge: {
    evaluate: (params, sourceCodeInfo): Any => {
      if (params.length === 0) {
        return null
      }
      const [first, ...rest] = params
      object.assert(first, sourceCodeInfo)

      return rest.reduce(
        (result: Obj, obj) => {
          object.assert(obj, sourceCodeInfo)
          return { ...result, ...obj }
        },
        { ...first },
      )
    },
    validate: node => assertLength({ min: 0 }, node),
  },

  'merge-with': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      const [fn, first, ...rest] = params
      litsFunction.assert(fn, sourceCodeInfo)

      if (params.length === 1) {
        return null
      }

      object.assert(first, sourceCodeInfo)

      return rest.reduce(
        (result: Obj, obj) => {
          object.assert(obj, sourceCodeInfo)
          Object.entries(obj).forEach(entry => {
            const key = string.as(entry[0], sourceCodeInfo)
            const val = toAny(entry[1])
            if (collHasKey(result, key)) {
              result[key] = executeFunction(fn, [result[key], val], sourceCodeInfo, contextStack)
            } else {
              result[key] = val
            }
          })
          return result
        },
        { ...first },
      )
    },
    validate: node => assertLength({ min: 1 }, node),
  },

  zipmap: {
    evaluate: ([keys, values], sourceCodeInfo): Any => {
      assertStringArray(keys, sourceCodeInfo)
      array.assert(values, sourceCodeInfo)

      const length = Math.min(keys.length, values.length)

      const result: Obj = {}

      for (let i = 0; i < length; i += 1) {
        const key = string.as(keys[i], sourceCodeInfo)
        result[key] = toAny(values[i])
      }
      return result
    },
    validate: node => assertLength(2, node),
  },

  'select-keys': {
    evaluate: ([obj, keys], sourceCodeInfo): Any => {
      assertStringArray(keys, sourceCodeInfo)
      object.assert(obj, sourceCodeInfo)

      return keys.reduce((result: Obj, key) => {
        if (collHasKey(obj, key)) {
          result[key] = toAny(obj[key]) as Any
        }
        return result
      }, {})
    },
    validate: node => assertLength(2, node),
  },
}
