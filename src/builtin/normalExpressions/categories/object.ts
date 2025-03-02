import type { Any, Arr, Obj } from '../../../interface'
import { assertEvenNumberOfParams, assertNumberOfParams } from '../../../typeGuards'
import { assertArray, assertStringArray } from '../../../typeGuards/array'
import { assertObj } from '../../../typeGuards/lits'
import { assertLitsFunction } from '../../../typeGuards/litsFunction'
import { asString, assertString } from '../../../typeGuards/string'
import { collHasKey, toAny } from '../../../utils'
import type { BuiltinNormalExpressions } from '../../interface'

export const objectNormalExpression: BuiltinNormalExpressions = {
  'object': {
    evaluate: (params, sourceCodeInfo): Obj => {
      const result: Obj = {}
      for (let i = 0; i < params.length; i += 2) {
        const key = params[i]
        const value = params[i + 1]
        assertString(key, sourceCodeInfo)
        result[key] = value
      }
      return result
    },
    validate: node => assertEvenNumberOfParams(node),
  },

  'keys': {
    evaluate: ([obj], sourceCodeInfo): string[] => {
      assertObj(obj, sourceCodeInfo)
      return Object.keys(obj)
    },
    validate: node => assertNumberOfParams(1, node),
  },

  'vals': {
    evaluate: ([obj], sourceCodeInfo): Arr => {
      assertObj(obj, sourceCodeInfo)
      return Object.values(obj)
    },
    validate: node => assertNumberOfParams(1, node),
  },

  'entries': {
    evaluate: ([obj], sourceCodeInfo): Array<[string, unknown]> => {
      assertObj(obj, sourceCodeInfo)
      return Object.entries(obj)
    },
    validate: node => assertNumberOfParams(1, node),
  },

  'find': {
    evaluate: ([obj, key], sourceCodeInfo): [string, unknown] | null => {
      assertObj(obj, sourceCodeInfo)
      assertString(key, sourceCodeInfo)
      if (collHasKey(obj, key))
        return [key, obj[key]]

      return null
    },
    validate: node => assertNumberOfParams(2, node),
  },

  'dissoc': {
    evaluate: ([obj, key], sourceCodeInfo): Any => {
      assertObj(obj, sourceCodeInfo)
      assertString(key, sourceCodeInfo)
      const newObj = { ...obj }
      delete newObj[key]
      return newObj
    },
    validate: node => assertNumberOfParams(2, node),
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
    validate: node => assertNumberOfParams({ min: 0 }, node),
  },

  'merge_with': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      const first = params[0]
      const fn = params.at(-1)
      const rest = params.slice(1, -1)

      assertObj(first, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)

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
    validate: node => assertNumberOfParams({ min: 2 }, node),
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
    validate: node => assertNumberOfParams(2, node),
  },

  'select_keys': {
    evaluate: ([obj, keys], sourceCodeInfo): Any => {
      assertStringArray(keys, sourceCodeInfo)
      assertObj(obj, sourceCodeInfo)

      return keys.reduce((result: Obj, key) => {
        if (collHasKey(obj, key))
          result[key] = toAny(obj[key])

        return result
      }, {})
    },
    validate: node => assertNumberOfParams(2, node),
  },
}
