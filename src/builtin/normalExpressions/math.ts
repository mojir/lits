import { NormalExpressionNode } from '../../parser/interface'
import {
  assertLengthOne,
  assertLengthOneOrMore,
  assertLengthTwo,
  assertNonNegativeNumber,
  assertNumber,
  assertNumberNotZero,
  assertPositiveNumber,
} from '../../utils'
import { BuiltinNormalExpressions } from './interface'

export const math: BuiltinNormalExpressions = {
  '1+': {
    evaluate: ([first]: unknown[]): number => {
      assertNumber(first)
      return first + 1
    },
    validate: ({ params }) => assertLengthOne(params),
  },

  '1-': {
    evaluate: ([first]: unknown[]): number => {
      assertNumber(first)
      return first - 1
    },
    validate: ({ params }) => assertLengthOne(params),
  },

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
    evaluate: (params: unknown[]): number => {
      if (params.length === 0) {
        return 1
      }
      const [first, ...rest] = params
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

  random: {
    evaluate: ([first]: unknown[]): number => {
      assertPositiveNumber(first)
      return Math.random() * first
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
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
}
