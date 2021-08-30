import { NormalExpressionNode } from '../../parser/interface'
import {
  assertLengthEven,
  assertLengthOne,
  assertLengthOneOrMore,
  assertLengthTwo,
  assertLengthTwoOrThree,
  assertNonNegativeNumber,
  assertNumber,
  assertNumberGte,
  assertNumberNotZero,
  assertString,
  assertStringOrArray,
} from '../../utils'

type Evaluate = (params: unknown[]) => unknown
type ValidateNode = (node: NormalExpressionNode) => void

type NormalExpressions = Record<
  string,
  {
    evaluate: Evaluate
    validate?: ValidateNode
    preEvaluate: boolean
  }
>

export const normalExpressions: NormalExpressions = {
  '+': {
    evaluate: (params: unknown[]): number =>
      params.reduce((result: number, param) => {
        assertNumber(param)
        return result + param
      }, 0),
    preEvaluate: true,
  },

  '*': {
    evaluate: (params: unknown[]): number =>
      params.reduce((result: number, param) => {
        assertNumber(param)
        return result * param
      }, 1),
    preEvaluate: true,
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
    preEvaluate: true,
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
    preEvaluate: true,
  },

  '%': {
    evaluate: ([first, second]: unknown[]): number => {
      assertNumber(first)
      assertNumberNotZero(second)
      return first % second
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
    preEvaluate: true,
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
    preEvaluate: true,
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
    preEvaluate: true,
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
    validate: ({ params }: NormalExpressionNode): void => assertLengthOneOrMore(params),
    preEvaluate: true,
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
    validate: ({ params }: NormalExpressionNode): void => assertLengthOneOrMore(params),
    preEvaluate: true,
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
    validate: ({ params }: NormalExpressionNode): void => assertLengthOneOrMore(params),
    preEvaluate: true,
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
    validate: ({ params }: NormalExpressionNode): void => assertLengthOneOrMore(params),
    preEvaluate: true,
  },

  and: {
    evaluate: (params: unknown[]): unknown => params.reduce((result: unknown, param) => result && param, true),
    preEvaluate: true,
  },

  or: {
    evaluate: (params: unknown[]): unknown => params.reduce((result: unknown, param) => result || param, false),
    preEvaluate: true,
  },

  not: {
    evaluate: ([first]: unknown[]): boolean => !first,
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
    preEvaluate: true,
  },

  stringp: {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'string',
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
    preEvaluate: true,
  },

  numberp: {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'number',
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
    preEvaluate: true,
  },

  booleanp: {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'boolean',
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
    preEvaluate: true,
  },

  undefinedp: {
    evaluate: ([first]: unknown[]): boolean => first === undefined,
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
    preEvaluate: true,
  },

  nullp: {
    evaluate: ([first]: unknown[]): boolean => first === null,
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
    preEvaluate: true,
  },

  arrayp: {
    evaluate: ([first]: unknown[]): boolean => {
      return Array.isArray(first)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
    preEvaluate: true,
  },

  objectp: {
    evaluate: ([first]: unknown[]): boolean => first !== null && !Array.isArray(first) && typeof first === 'object',
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
    preEvaluate: true,
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
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwoOrThree(params),
    preEvaluate: true,
  },

  length: {
    evaluate: ([first]: unknown[]): unknown => {
      assertStringOrArray(first)
      return first.length
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
    preEvaluate: true,
  },

  concat: {
    evaluate: (params: unknown[]): unknown => {
      return params.reduce((result: string, param) => {
        assertString(param)
        return result + param
      }, '')
    },
    preEvaluate: true,
  },

  array: {
    evaluate: (params: unknown[]): unknown => params,
    preEvaluate: true,
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
    preEvaluate: true,
  },

  aref: {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertStringOrArray(first)
      assertNonNegativeNumber(second)
      return first[second]
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
    preEvaluate: true,
  },

  write: {
    evaluate: ([first]: unknown[]): unknown => {
      // eslint-disable-next-line no-console
      console.log('LISPISH>', first)
      return first
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
    preEvaluate: false,
  },

  'string>': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first > second
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
    preEvaluate: true,
  },

  'string<': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first < second
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
    preEvaluate: true,
  },

  'string>=': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first >= second
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
    preEvaluate: true,
  },

  'string<=': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first <= second
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
    preEvaluate: true,
  },
}
