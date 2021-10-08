import {
  assertArray,
  assertCollection,
  assertInteger,
  assertLength,
  assertNonNegativeNumber,
  assertNumberGte,
  assertNumberLte,
  assertObject,
  assertString,
  assertStringOrNumber,
} from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
export const collectionNormalExpression: BuiltinNormalExpressions = {
  get: {
    evaluate: (params: unknown[]): unknown => {
      const [coll, key, defaultValue] = params
      const hasDefault = params.length === 3
      assertCollection(coll)
      if (Array.isArray(coll)) {
        assertInteger(key)
        assertNonNegativeNumber(key)
        if (coll.length > key) {
          return coll[key]
        }
      } else {
        assertString(key)
        if (Object.getOwnPropertyDescriptor(coll, key)) {
          return coll[key]
        }
      }
      if (hasDefault) {
        return defaultValue
      }
      return undefined
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  count: {
    evaluate: ([coll]: unknown[]): number => {
      if (typeof coll === `string`) {
        return coll.length
      }
      assertCollection(coll)
      if (Array.isArray(coll)) {
        return coll.length
      }
      return Object.keys(coll).length
    },
    validate: node => assertLength(1, node),
  },
  'contains?': {
    evaluate: ([coll, key]: unknown[]): boolean => {
      assertCollection(coll)
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
    evaluate: ([coll, key, value]: unknown[]): unknown => {
      assertCollection(coll)
      assertStringOrNumber(key)
      if (Array.isArray(coll)) {
        assertInteger(key)
        assertNumberGte(key, 0)
        assertNumberLte(key, coll.length)
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
    evaluate: (params: unknown[]): unknown => {
      if (Array.isArray(params[0])) {
        return params.reduce((result: unknown[], arr) => {
          assertArray(arr)
          return result.concat(arr)
        }, [])
      } else {
        return params.reduce((result: Record<string, unknown>, obj) => {
          assertObject(obj)
          return Object.assign(result, obj)
        }, {})
      }
    },
    validate: node => assertLength({ min: 1 }, node),
  },
  'empty?': {
    evaluate: ([first]: unknown[]): boolean => {
      if (typeof first === `string`) {
        return first.length === 0
      }
      assertCollection(first)
      if (Array.isArray(first)) {
        return first.length === 0
      }
      return Object.keys(first).length === 0
    },
    validate: node => assertLength(1, node),
  },
}
