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
} from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const objectNormalExpression: BuiltinNormalExpressions = {
  object: {
    evaluate: (params: Arr): Obj => {
      const result: Obj = {}
      for (let i = 0; i < params.length; i += 2) {
        const key = params[i]
        const value = params[i + 1]
        assertString(key)
        result[key] = value
      }
      return result
    },
    validate: node => assertLengthEven(node),
  },

  keys: {
    evaluate: ([first]: Arr): string[] => {
      assertObj(first)
      return Object.keys(first)
    },
    validate: node => assertLength(1, node),
  },

  vals: {
    evaluate: ([first]: Arr): Arr => {
      assertObj(first)
      return Object.values(first)
    },
    validate: node => assertLength(1, node),
  },

  entries: {
    evaluate: ([first]: Arr): Array<[string, unknown]> => {
      assertObj(first)
      return Object.entries(first)
    },
    validate: node => assertLength(1, node),
  },

  find: {
    evaluate: ([obj, key]: Arr): [string, unknown] | null => {
      assertObj(obj)
      assertString(key)
      if (collHasKey(obj, key)) {
        return [key, obj[key]]
      }
      return null
    },
    validate: node => assertLength(2, node),
  },

  dissoc: {
    evaluate: ([obj, key]: Arr): Any => {
      assertObj(obj)
      assertString(key)
      const result = toAny(obj[key])
      delete obj[key]
      return result
    },
    validate: node => assertLength(2, node),
  },

  merge: {
    evaluate: ([first, ...rest]: Arr): Any => {
      assertObj(first)

      return rest.reduce(
        (result: Obj, obj) => {
          assertObj(obj)
          return { ...result, ...obj }
        },
        { ...first },
      )
    },
    validate: node => assertLength({ min: 1 }, node),
  },

  zipmap: {
    evaluate: ([keys, values]: Arr): Any => {
      assertStringArray(keys)
      assertArr(values)

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
    evaluate: ([obj, keys]: Arr): Any => {
      assertStringArray(keys)
      assertObj(obj)

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
