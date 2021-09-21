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
    validate: ({ params }) => assertLengthEven(params),
  },

  keys: {
    evaluate: ([first]: unknown[]): string[] => {
      assertObject(first)
      return Object.keys(first)
    },
    validate: ({ params }) => assertLength(1, params),
  },

  values: {
    evaluate: ([first]: unknown[]): unknown[] => {
      assertObject(first)
      return Object.values(first)
    },
    validate: ({ params }) => assertLength(1, params),
  },

  entries: {
    evaluate: ([first]: unknown[]): Array<[string, unknown]> => {
      assertObject(first)
      return Object.entries(first)
    },
    validate: ({ params }) => assertLength(1, params),
  },

  'has-attr': {
    evaluate: ([obj, key]: unknown[]): boolean => {
      assertObject(obj)
      assertString(key)
      return !!Object.getOwnPropertyDescriptor(obj, key)
    },
    validate: ({ params }) => assertLength(2, params),
  },

  'get-attr': {
    evaluate: ([obj, key]: unknown[]): unknown => {
      assertObject(obj)
      assertString(key)
      return obj[key]
    },
    validate: ({ params }) => assertLength(2, params),
  },

  'set-attr': {
    evaluate: ([obj, key, value]: unknown[]): unknown => {
      assertObject(obj)
      assertString(key)
      obj[key] = value
      return value
    },
    validate: ({ params }) => assertLength(3, params),
  },

  'del-attr': {
    evaluate: ([obj, key]: unknown[]): unknown => {
      assertObject(obj)
      assertString(key)
      const result = obj[key]
      delete obj[key]
      return result
    },
    validate: ({ params }) => assertLength(2, params),
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
    validate: ({ params }) => assertLength({ min: 1 }, params),
  },
}
