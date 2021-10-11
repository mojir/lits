import { Arr, Coll, Obj } from '../../../interface'
import {
  assertArr,
  assertChar,
  assertColl,
  assertInteger,
  assertLength,
  assertNumberGte,
  assertNumberLte,
  assertObj,
  assertString,
  assertStringOrNumber,
  isArr,
  isObj,
  isString,
} from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
export const collectionNormalExpression: BuiltinNormalExpressions = {
  get: {
    evaluate: (params: Arr): unknown => {
      const [coll, key, defaultValue] = params
      const hasDefault = params.length === 3

      assertColl(coll)

      if (isArr(coll)) {
        assertInteger(key)
        if (key < coll.length) {
          return coll[key]
        }
      } else if (isObj(coll)) {
        assertString(key)
        if (Object.getOwnPropertyDescriptor(coll, key)) {
          return coll[key]
        }
      } else {
        assertInteger(key)
        return coll[key]
      }
      if (hasDefault) {
        return defaultValue
      }
      return undefined
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  count: {
    evaluate: ([coll]: Arr): number => {
      if (typeof coll === `string`) {
        return coll.length
      }
      assertColl(coll)
      if (Array.isArray(coll)) {
        return coll.length
      }
      return Object.keys(coll).length
    },
    validate: node => assertLength(1, node),
  },
  'contains?': {
    evaluate: ([coll, key]: Arr): boolean => {
      assertColl(coll)
      assertStringOrNumber(key)
      if (Array.isArray(coll)) {
        if (!Number.isInteger(key)) {
          return false
        }
        assertInteger(key)
        return key >= 0 && key < coll.length
      }
      if (typeof key !== `string`) {
        return false
      }
      return !!Object.getOwnPropertyDescriptor(coll, key)
    },
    validate: node => assertLength(2, node),
  },
  assoc: {
    evaluate: ([coll, key, value]: Arr): Coll => {
      assertColl(coll)
      assertStringOrNumber(key)
      if (Array.isArray(coll) || typeof coll === `string`) {
        assertInteger(key)
        assertNumberGte(key, 0)
        assertNumberLte(key, coll.length)
        if (typeof coll === `string`) {
          assertChar(value)
          return `${coll.slice(0, key)}${value}${coll.slice(key + 1)}`
        }
        const copy = [...coll]
        copy[key] = value
        return copy
      }
      assertString(key)
      const copy = { ...coll }
      copy[key] = value
      return copy
    },
    validate: node => assertLength(3, node),
  },
  concat: {
    evaluate: (params: Arr): unknown => {
      assertColl(params[0])
      if (isArr(params[0])) {
        return params.reduce((result: Arr, arr) => {
          assertArr(arr)
          return result.concat(arr)
        }, [])
      } else if (isString(params[0])) {
        return params.reduce((result: string, s) => {
          assertString(s)
          return `${result}${s}`
        }, ``)
      } else {
        return params.reduce((result: Obj, obj) => {
          assertObj(obj)
          return Object.assign(result, obj)
        }, {})
      }
    },
    validate: node => assertLength({ min: 1 }, node),
  },
  'empty?': {
    evaluate: ([first]: Arr): boolean => {
      assertColl(first)
      if (isString(first)) {
        return first.length === 0
      }
      if (Array.isArray(first)) {
        return first.length === 0
      }
      return Object.keys(first).length === 0
    },
    validate: node => assertLength(1, node),
  },
}
