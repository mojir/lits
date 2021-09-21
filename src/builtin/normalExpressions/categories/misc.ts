import get from 'lodash/get'
import { assertArray, assertLength, assertLispishFunction, assertObjectOrArray, assertString } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
export const miscNormalExpression: BuiltinNormalExpressions = {
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
    validate: ({ params }) => assertLength({ min: 1 }, params),
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
    validate: ({ params }) => assertLength({ min: 1 }, params),
  },
  apply: {
    evaluate: ([func, list]: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      assertLispishFunction(func)
      assertArray(list)
      return evaluateLispishFunction(func, list, contextStack)
    },
    validate: ({ params }) => assertLength(2, params),
  },
  'get-path': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertObjectOrArray(first)
      assertString(second)
      return get(first, second)
    },
    validate: ({ params }) => assertLength(2, params),
  },
  not: {
    evaluate: ([first]: unknown[]): boolean => !first,
    validate: ({ params }) => assertLength(1, params),
  },
  now: {
    evaluate: (): number => {
      return Date.now()
    },
    validate: ({ params }) => assertLength(0, params),
  },
  progn: {
    evaluate: (params: unknown[]): unknown => {
      return params[params.length - 1]
    },
  },
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
  debug: {
    evaluate: ([label], contextStack): undefined => {
      assertString(label)

      // eslint-disable-next-line no-console
      console.error(
        `LISPISH: ${label}.`,
        `Context stack (${contextStack
          .map(stack => `${Object.keys(stack.variables).length}:${Object.keys(stack.functions).length}`)
          .join(' ')})`,
        contextStack,
      )

      return undefined
    },
    validate: ({ params }) => assertLength(1, params),
  },
}
