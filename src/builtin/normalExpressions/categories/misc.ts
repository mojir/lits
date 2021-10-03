import { assertArray, assertLength, assertLispishFunction, assertObjectOrArray, assertString } from '../../../utils'
import { getPath } from '../../getPath'
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
    validate: node => assertLength({ min: 1 }, node),
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
    validate: node => assertLength({ min: 1 }, node),
  },
  apply: {
    evaluate: ([func, list]: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      assertLispishFunction(func)
      assertArray(list)
      return evaluateLispishFunction(func, list, contextStack)
    },
    validate: node => assertLength(2, node),
  },
  'get-path': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertObjectOrArray(first)
      assertString(second)
      return getPath(first, second)
    },
    validate: node => assertLength(2, node),
  },
  not: {
    evaluate: ([first]: unknown[]): boolean => !first,
    validate: node => assertLength(1, node),
  },
  now: {
    evaluate: (): number => {
      return Date.now()
    },
    validate: node => assertLength(0, node),
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
    evaluate: (params, contextStack): undefined => {
      const label = params[0] ?? ``
      if (params.length === 1) {
        assertString(params[0])
      }

      // eslint-disable-next-line no-console
      console.error(
        `LISPISH${label ? `: ${label}.` : ``}`,
        `Context stack (${contextStack
          .map(stack => `${Object.keys(stack.variables).length}:${Object.keys(stack.functions).length}`)
          .join(` `)})`,
        contextStack,
      )

      return undefined
    },
    validate: node => assertLength({ min: 0, max: 1 }, node),
  },
}
