import {
  assertCollection,
  assertInteger,
  assertLength,
  assertNumberGte,
  assertNumberLte,
  assertString,
  assertStringOrNumber,
} from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
export const collectionNormalExpression: BuiltinNormalExpressions = {
  count: {
    evaluate: ([coll]: unknown[]): number => {
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
}
