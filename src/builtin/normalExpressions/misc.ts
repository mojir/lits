import { NormalExpressionNode } from '../../parser/interface'
import { assertLengthEven, assertLengthOne, assertLengthOneOrMore, assertLengthZero, assertString } from '../../utils'
import { BuiltinNormalExpressions } from './interface'

export const misc: BuiltinNormalExpressions = {
  write: {
    evaluate: ([first]: unknown[]): unknown => {
      // eslint-disable-next-line no-console
      console.log(first)
      return first
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  now: {
    evaluate: (): number => {
      return Date.now()
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthZero(params),
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
    validate: ({ params }: NormalExpressionNode): void => assertLengthOneOrMore(params),
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
    validate: ({ params }: NormalExpressionNode): void => assertLengthOneOrMore(params),
  },

  not: {
    evaluate: ([first]: unknown[]): boolean => !first,
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

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
    validate: ({ params }: NormalExpressionNode): void => assertLengthEven(params),
  },
}
