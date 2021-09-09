import {
  assertLengthEven,
  assertLengthOne,
  assertLengthOneOrMore,
  assertLengthThree,
  assertLengthTwo,
  assertObject,
  assertString,
} from '../../utils'
import { BuiltinNormalExpressions } from './interface'

export const object: BuiltinNormalExpressions = {
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
    validate: ({ params }) => assertLengthOne(params),
  },

  values: {
    evaluate: ([first]: unknown[]): unknown[] => {
      assertObject(first)
      return Object.values(first)
    },
    validate: ({ params }) => assertLengthOne(params),
  },

  entries: {
    evaluate: ([first]: unknown[]): Array<[string, unknown]> => {
      assertObject(first)
      return Object.entries(first)
    },
    validate: ({ params }) => assertLengthOne(params),
  },

  'has-attr': {
    evaluate: ([obj, key]: unknown[]): boolean => {
      assertObject(obj)
      assertString(key)
      return !!Object.getOwnPropertyDescriptor(obj, key)
    },
    validate: ({ params }) => assertLengthTwo(params),
  },

  'get-attr': {
    evaluate: ([obj, key]: unknown[]): unknown => {
      assertObject(obj)
      assertString(key)
      return obj[key]
    },
    validate: ({ params }) => assertLengthTwo(params),
  },

  'set-attr': {
    evaluate: ([obj, key, value]: unknown[]): unknown => {
      assertObject(obj)
      assertString(key)
      obj[key] = value
      return value
    },
    validate: ({ params }) => assertLengthThree(params),
  },

  'del-attr': {
    evaluate: ([obj, key]: unknown[]): unknown => {
      assertObject(obj)
      assertString(key)
      const result = obj[key]
      delete obj[key]
      return result
    },
    validate: ({ params }) => assertLengthTwo(params),
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
    validate: ({ params }) => assertLengthOneOrMore(params),
  },
}
