import { BasicExpressionNode } from '../parser/Parser.types'
import {
  assertLengthEven,
  assertLengthOne,
  assertLengthOneOrMore,
  assertLengthThree,
  assertLengthTwo,
  assertLengthTwoOrThree,
  assertNonNegativeNumber,
  assertNumber,
  assertNumberGte,
  assertNumberNotZero,
  assertString,
  assertStringOrArray,
} from '../utils'
import { StdLib, StdLibValidators, StdLibEvaluators } from './stdLib.types'

const stdLib: StdLib = {
  '+': {
    evaluate: (params: unknown[]): number =>
      params.reduce((result: number, param) => {
        assertNumber(param)
        return result + param
      }, 0),
  },

  '*': {
    evaluate: (params: unknown[]): number =>
      params.reduce((result: number, param) => {
        assertNumber(param)
        return result * param
      }, 1),
  },

  '/': {
    evaluate: ([first, ...rest]: unknown[]): number => {
      if (!first) {
        return 1
      }
      assertNumber(first)
      if (rest.length === 0) {
        return 1 / first
      }
      return rest.reduce((result: number, param) => {
        assertNumberNotZero(param)
        return result / param
      }, first)
    },
  },

  '-': {
    evaluate: ([first, ...rest]: unknown[]): number => {
      if (!first) {
        return 0
      }
      assertNumber(first)
      if (rest.length === 0) {
        return -first
      }
      return rest.reduce((result: number, param) => {
        assertNumber(param)
        return result - param
      }, first)
    },
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
    validate: ({ params }: BasicExpressionNode): void => assertLengthOneOrMore(params),
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
    validate: ({ params }: BasicExpressionNode): void => assertLengthOneOrMore(params),
  },

  '>': {
    evaluate: ([first, ...rest]: unknown[]): boolean => {
      assertNumber(first)

      if (rest.length === 0) {
        return true
      }

      let currentValue = first
      for (const param of rest) {
        assertNumber(param)
        if (currentValue <= param) {
          return false
        }
        currentValue = param
      }
      return true
    },
    validate: ({ params }: BasicExpressionNode): void => assertLengthOneOrMore(params),
  },

  '<': {
    evaluate: ([first, ...rest]: unknown[]): boolean => {
      assertNumber(first)

      if (rest.length === 0) {
        return true
      }

      let currentValue = first
      for (const param of rest) {
        assertNumber(param)
        if (currentValue >= param) {
          return false
        }
        currentValue = param
      }
      return true
    },
    validate: ({ params }: BasicExpressionNode): void => assertLengthOneOrMore(params),
  },

  '>=': {
    evaluate: ([first, ...rest]: unknown[]): boolean => {
      assertNumber(first)
      if (rest.length === 0) {
        return true
      }

      let currentValue = first
      for (const param of rest) {
        assertNumber(param)
        if (currentValue < param) {
          return false
        }
        currentValue = param
      }
      return true
    },
    validate: ({ params }: BasicExpressionNode): void => assertLengthOneOrMore(params),
  },

  '<=': {
    evaluate: ([first, ...rest]: unknown[]): boolean => {
      assertNumber(first)
      if (rest.length === 0) {
        return true
      }

      let currentValue = first
      for (const param of rest) {
        assertNumber(param)
        if (currentValue > param) {
          return false
        }
        currentValue = param
      }
      return true
    },
    validate: ({ params }: BasicExpressionNode): void => assertLengthOneOrMore(params),
  },

  and: {
    evaluate: (params: unknown[]): unknown => params.reduce((result: unknown, param) => result && param, true),
  },

  or: {
    evaluate: (params: unknown[]): unknown => params.reduce((result: unknown, param) => result || param, false),
  },

  not: {
    evaluate: ([first]: unknown[]): boolean => !first,
    validate: ({ params }: BasicExpressionNode): void => assertLengthOne(params),
  },

  stringp: {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'string',
    validate: ({ params }: BasicExpressionNode): void => assertLengthOne(params),
  },

  numberp: {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'number',
    validate: ({ params }: BasicExpressionNode): void => assertLengthOne(params),
  },

  booleanp: {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'boolean',
    validate: ({ params }: BasicExpressionNode): void => assertLengthOne(params),
  },

  undefinedp: {
    evaluate: ([first]: unknown[]): boolean => first === undefined,
    validate: ({ params }: BasicExpressionNode): void => assertLengthOne(params),
  },

  nullp: {
    evaluate: ([first]: unknown[]): boolean => first === null,
    validate: ({ params }: BasicExpressionNode): void => assertLengthOne(params),
  },

  arrayp: {
    evaluate: ([first]: unknown[]): boolean => Array.isArray(first),
    validate: ({ params }: BasicExpressionNode): void => assertLengthOne(params),
  },

  objectp: {
    evaluate: ([first]: unknown[]): boolean => first !== null && !Array.isArray(first) && typeof first === 'object',
    validate: ({ params }: BasicExpressionNode): void => assertLengthOne(params),
  },

  substring: {
    evaluate: ([first, second, third]: unknown[]): unknown => {
      assertString(first)
      assertNumber(second)
      assertNonNegativeNumber(second)

      if (third === undefined) {
        return (first as string).substring(second)
      }

      assertNumberGte(third, second)
      return (first as string).substring(second, third)
    },
    validate: ({ params }: BasicExpressionNode): void => assertLengthTwoOrThree(params),
  },

  length: {
    evaluate: ([first]: unknown[]): unknown => {
      assertStringOrArray(first)
      return first.length
    },
    validate: ({ params }: BasicExpressionNode): void => assertLengthOne(params),
  },

  'string-concat': {
    evaluate: (params: unknown[]): unknown => {
      return params.reduce((result: string, param) => {
        assertString(param)
        return result + param
      }, '')
    },
  },

  array: {
    evaluate: (params: unknown[]): unknown => params,
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
    validate: ({ params }: BasicExpressionNode): void => assertLengthEven(params),
  },

  aref: {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertStringOrArray(first)
      assertNonNegativeNumber(second)
      return first[second]
    },
    validate: ({ params }: BasicExpressionNode): void => assertLengthTwo(params),
  },

  write: {
    evaluate: ([first]: unknown[]): unknown => {
      // eslint-disable-next-line no-console
      console.log('LISP>', first)
      return first
    },
    validate: ({ params }: BasicExpressionNode): void => assertLengthOne(params),
  },

  'string>': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first > second
    },
    validate: ({ params }: BasicExpressionNode): void => assertLengthTwo(params),
  },

  'string<': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first < second
    },
    validate: ({ params }: BasicExpressionNode): void => assertLengthTwo(params),
  },

  'string>=': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first >= second
    },
    validate: ({ params }: BasicExpressionNode): void => assertLengthTwo(params),
  },

  'string<=': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first <= second
    },
    validate: ({ params }: BasicExpressionNode): void => assertLengthTwo(params),
  },

  setq: {
    evaluate: () => undefined, // Handled by parseBasicExpression
    validate: ({ params }: BasicExpressionNode): void => assertLengthTwo(params),
  },

  if: {
    evaluate: () => undefined, // Handled by parseBasicExpression
    validate: ({ params }: BasicExpressionNode): void => assertLengthThree(params),
  },
}

export const stdLibEvaluators: StdLibEvaluators = Object.entries(stdLib).reduce(
  (result: StdLibEvaluators, [key, record]) => {
    result[key] = record.evaluate
    return result
  },
  {},
)

export const stdLibValidators: StdLibValidators = Object.entries(stdLib).reduce(
  (result: StdLibValidators, [key, record]) => {
    if (record.validate) {
      result[key] = record.validate
    }
    return result
  },
  {},
)
