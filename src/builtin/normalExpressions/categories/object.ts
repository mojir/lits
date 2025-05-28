import type { Any, Arr, Obj } from '../../../interface'
import { assertArray, assertStringArray } from '../../../typeGuards/array'
import { assertFunctionLike, assertObj } from '../../../typeGuards/lits'
import { asString, assertString } from '../../../typeGuards/string'
import { collHasKey, toAny } from '../../../utils'
import { toFixedArity } from '../../../utils/arity'
import type { BuiltinNormalExpressions } from '../../interface'

export const objectNormalExpression: BuiltinNormalExpressions = {
  'keys': {
    evaluate: ([obj], sourceCodeInfo): string[] => {
      assertObj(obj, sourceCodeInfo)
      return Object.keys(obj)
    },
    arity: toFixedArity(1),
  },

  'vals': {
    evaluate: ([obj], sourceCodeInfo): Arr => {
      assertObj(obj, sourceCodeInfo)
      return Object.values(obj)
    },
    arity: toFixedArity(1),
  },

  'entries': {
    evaluate: ([obj], sourceCodeInfo): Array<[string, unknown]> => {
      assertObj(obj, sourceCodeInfo)
      return Object.entries(obj)
    },
    arity: toFixedArity(1),
  },

  'find': {
    evaluate: ([obj, key], sourceCodeInfo): [string, unknown] | null => {
      assertObj(obj, sourceCodeInfo)
      assertString(key, sourceCodeInfo)
      if (collHasKey(obj, key))
        return [key, obj[key]]

      return null
    },
    arity: toFixedArity(2),
  },

  'dissoc': {
    evaluate: ([obj, key], sourceCodeInfo): Any => {
      assertObj(obj, sourceCodeInfo)
      assertString(key, sourceCodeInfo)
      const newObj = { ...obj }
      delete newObj[key]
      return newObj
    },
    arity: toFixedArity(2),
  },

  'merge': {
    evaluate: (params, sourceCodeInfo): Any => {
      if (params.length === 0)
        return null

      const [first, ...rest] = params
      assertObj(first, sourceCodeInfo)

      return rest.reduce(
        (result: Obj, obj) => {
          assertObj(obj, sourceCodeInfo)
          return { ...result, ...obj }
        },
        { ...first },
      )
    },
    arity: { min: 0 },
  },

  'merge-with': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      const first = params[0]
      const fn = params.at(-1)
      const rest = params.slice(1, -1)

      assertObj(first, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)

      return rest.reduce(
        (result: Obj, obj) => {
          assertObj(obj, sourceCodeInfo)
          Object.entries(obj).forEach((entry) => {
            const key = asString(entry[0], sourceCodeInfo)
            const val = toAny(entry[1])
            if (collHasKey(result, key))
              result[key] = executeFunction(fn, [result[key], val], contextStack, sourceCodeInfo)
            else
              result[key] = val
          })
          return result
        },
        { ...first },
      )
    },
    arity: { min: 2 },
  },

  'zipmap': {
    evaluate: ([keys, values], sourceCodeInfo): Any => {
      assertStringArray(keys, sourceCodeInfo)
      assertArray(values, sourceCodeInfo)

      const length = Math.min(keys.length, values.length)

      const result: Obj = {}

      for (let i = 0; i < length; i += 1) {
        const key = asString(keys[i], sourceCodeInfo)
        result[key] = toAny(values[i])
      }
      return result
    },
    arity: toFixedArity(2),
  },

  'select-keys': {
    evaluate: ([obj, keys], sourceCodeInfo): Any => {
      assertStringArray(keys, sourceCodeInfo)
      assertObj(obj, sourceCodeInfo)

      return keys.reduce((result: Obj, key) => {
        if (collHasKey(obj, key))
          result[key] = toAny(obj[key])

        return result
      }, {})
    },
    arity: toFixedArity(2),
  },
}
