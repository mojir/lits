import { assertLengthEven, assertLength, assertObject, assertString } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const objectNormalExpression: BuiltinNormalExpressions = {
  object: {
    evaluate: (params: unknown[]): Record<string, unknown> => {
      const result: Record<string, unknown> = {}
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
    evaluate: ([first]: unknown[]): string[] => {
      assertObject(first)
      return Object.keys(first)
    },
    validate: node => assertLength(1, node),
  },

  values: {
    evaluate: ([first]: unknown[]): unknown[] => {
      assertObject(first)
      return Object.values(first)
    },
    validate: node => assertLength(1, node),
  },

  entries: {
    evaluate: ([first]: unknown[]): Array<[string, unknown]> => {
      assertObject(first)
      return Object.entries(first)
    },
    validate: node => assertLength(1, node),
  },

  'has-attr': {
    evaluate: ([obj, key]: unknown[]): boolean => {
      assertObject(obj)
      assertString(key)
      return !!Object.getOwnPropertyDescriptor(obj, key)
    },
    validate: node => assertLength(2, node),
  },

  'get-attr': {
    evaluate: ([obj, key]: unknown[]): unknown => {
      assertObject(obj)
      assertString(key)
      return obj[key]
    },
    validate: node => assertLength(2, node),
  },

  'set-attr': {
    evaluate: ([obj, key, value]: unknown[]): unknown => {
      assertObject(obj)
      assertString(key)
      obj[key] = value
      return value
    },
    validate: node => assertLength(3, node),
  },

  'del-attr': {
    evaluate: ([obj, key]: unknown[]): unknown => {
      assertObject(obj)
      assertString(key)
      const result = obj[key]
      delete obj[key]
      return result
    },
    validate: node => assertLength(2, node),
  },

  merge: {
    evaluate: ([first, ...rest]: unknown[]): unknown => {
      assertObject(first)

      return rest.reduce(
        (result: Record<string, unknown>, obj) => {
          assertObject(obj)
          return { ...result, ...obj }
        },
        { ...first },
      )
    },
    validate: node => assertLength({ min: 1 }, node),
  },
}
