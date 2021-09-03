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
  }
>

export const normalExpressions: NormalExpressions = {
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

  '%': {
    evaluate: ([first, second]: unknown[]): number => {
      assertNumber(first)
      assertNumberNotZero(second)
      return first % second
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
  },

  sqrt: {
    evaluate: ([first]: unknown[]): number => {
      assertNonNegativeNumber(first)
      return Math.sqrt(first)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  expt: {
    evaluate: ([first, second]: unknown[]): number => {
      assertNumber(first)
      assertNumber(second)
      return Math.pow(first, second)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
  },

  round: {
    evaluate: ([first]: unknown[]): number => {
      assertNumber(first)
      return Math.round(first)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  floor: {
    evaluate: ([first]: unknown[]): number => {
      assertNumber(first)
      return Math.floor(first)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  ceil: {
    evaluate: ([first]: unknown[]): number => {
      assertNumber(first)
      return Math.ceil(first)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
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
  },

  not: {
    evaluate: ([first]: unknown[]): boolean => !first,
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  stringp: {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'string',
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  numberp: {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'number',
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  booleanp: {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'boolean',
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  undefinedp: {
    evaluate: ([first]: unknown[]): boolean => first === undefined,
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  nullp: {
    evaluate: ([first]: unknown[]): boolean => first === null,
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  arrayp: {
    evaluate: ([first]: unknown[]): boolean => {
      return Array.isArray(first)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  objectp: {
    evaluate: ([first]: unknown[]): boolean => first !== null && !Array.isArray(first) && typeof first === 'object',
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
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
  },

  length: {
    evaluate: ([first]: unknown[]): unknown => {
      assertStringOrArray(first)
      return first.length
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  concat: {
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
    validate: ({ params }: NormalExpressionNode): void => assertLengthEven(params),
  },

  aref: {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertStringOrArray(first)
      assertNonNegativeNumber(second)
      return first[second]
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
  },

  write: {
    evaluate: ([first]: unknown[]): unknown => {
      // eslint-disable-next-line no-console
      console.log(first)
      return first
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'string>': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first > second
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
  },

  'string<': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first < second
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
  },

  'string>=': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first >= second
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
  },

  'string<=': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first <= second
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
  },
}
