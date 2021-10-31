import { Any, Arr, Obj } from '../../../interface'
import {
  assertLengthEven,
  assertLength,
  assertObj,
  assertString,
  collHasKey,
  assertStringArray,
  assertArr,
  toAny,
  asNotUndefined,
  assertLitsFunction,
} from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const objectNormalExpression: BuiltinNormalExpressions = {
  object: {
    evaluate: (params, meta): Obj => {
      const result: Obj = {}
      for (let i = 0; i < params.length; i += 2) {
        const key = params[i]
        const value = params[i + 1]
        assertString(key, meta)
        result[key] = value
      }
      return result
    },
    validate: node => assertLengthEven(node),
  },

  keys: {
    evaluate: ([first], meta): string[] => {
      assertObj(first, meta)
      return Object.keys(first)
    },
    validate: node => assertLength(1, node),
  },

  vals: {
    evaluate: ([first], meta): Arr => {
      assertObj(first, meta)
      return Object.values(first)
    },
    validate: node => assertLength(1, node),
  },

  entries: {
    evaluate: ([first], meta): Array<[string, unknown]> => {
      assertObj(first, meta)
      return Object.entries(first)
    },
    validate: node => assertLength(1, node),
  },

  find: {
    evaluate: ([obj, key], meta): [string, unknown] | null => {
      assertObj(obj, meta)
      assertString(key, meta)
      if (collHasKey(obj, key)) {
        return [key, obj[key]]
      }
      return null
    },
    validate: node => assertLength(2, node),
  },

  dissoc: {
    evaluate: ([obj, key], meta): Any => {
      assertObj(obj, meta)
      assertString(key, meta)
      const result = toAny(obj[key])
      delete obj[key]
      return result
    },
    validate: node => assertLength(2, node),
  },

  merge: {
    evaluate: (params, meta): Any => {
      if (params.length === 0) {
        return null
      }
      const [first, ...rest] = params
      assertObj(first, meta)

      return rest.reduce(
        (result: Obj, obj) => {
          assertObj(obj, meta)
          return { ...result, ...obj }
        },
        { ...first },
      )
    },
    validate: node => assertLength({ min: 0 }, node),
  },

  'merge-with': {
    evaluate: (params: Arr, meta, contextStack, { executeFunction }): Any => {
      const [fn, first, ...rest] = params
      assertLitsFunction(fn, meta)

      if (params.length === 1) {
        return null
      }

      assertObj(first, meta)

      return rest.reduce(
        (result: Obj, obj) => {
          assertObj(obj, meta)
          Object.entries(obj).forEach(entry => {
            const key = asNotUndefined(entry[0], meta)
            const val = toAny(entry[1])
            if (collHasKey(result, key)) {
              result[key] = executeFunction(fn, [result[key], val], meta, contextStack)
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
    evaluate: ([keys, values], meta): Any => {
      assertStringArray(keys, meta)
      assertArr(values, meta)

      const length = Math.min(keys.length, values.length)

      const result: Obj = {}

      for (let i = 0; i < length; i += 1) {
        const key = asNotUndefined(keys[i])
        result[key] = toAny(values[i])
      }
      return result
    },
    validate: node => assertLength(2, node),
  },

  'select-keys': {
    evaluate: ([obj, keys], meta): Any => {
      assertStringArray(keys, meta)
      assertObj(obj, meta)

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
