import get from 'lodash/get'
import {
  assertArray,
  assertLengthOne,
  assertLengthOneOrMore,
  assertLengthTwo,
  assertLengthZero,
  assertLispishFunction,
  assertObjectOrArray,
  assertString,
} from '../../utils'
import { BuiltinNormalExpressions } from './interface'

export const misc: BuiltinNormalExpressions = {
  write: {
    evaluate: (params: unknown[]): unknown => {
      // eslint-disable-next-line no-console
      console.log(...params)
      if (params.length > 0) {
        return params[params.length - 1]
      }
      return undefined
    },
  },

  now: {
    evaluate: (): number => {
      return Date.now()
    },
    validate: ({ params }) => assertLengthZero(params),
  },

  '=': {
    evaluate: ([first, ...rest]: unknown[]): boolean => {
      for (const param of rest) {
        if (param !== first) {
          return false
        }
      }
      return true
    },
    validate: ({ params }) => assertLengthOneOrMore(params),
  },

  '!=': {
    evaluate: (params: unknown[]): boolean => {
      for (let i = 0; i < params.length - 1; i += 1) {
        for (let j = i + 1; j < params.length; j += 1) {
          if (params[i] === params[j]) {
            return false
          }
        }
      }
      return true
    },
    validate: ({ params }) => assertLengthOneOrMore(params),
  },

  not: {
    evaluate: ([first]: unknown[]): boolean => !first,
    validate: ({ params }) => assertLengthOne(params),
  },

  'get-path': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertObjectOrArray(first)
      assertString(second)
      return get(first, second)
    },
    validate: ({ params }) => assertLengthTwo(params),
  },

  progn: {
    evaluate: (params: unknown[]): unknown => {
      return params[params.length - 1]
    },
  },

  apply: {
    evaluate: ([func, list]: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      assertLispishFunction(func)
      assertArray(list)
      return evaluateLispishFunction(func, list, contextStack)
    },
    validate: ({ params }) => assertLengthTwo(params),
  },
}
