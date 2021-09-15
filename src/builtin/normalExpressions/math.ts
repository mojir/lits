import { NormalExpressionNode } from '../../parser/interface'
import {
  asFiniteNumber,
  assertInteger,
  assertLengthOne,
  assertLengthOneOrMore,
  assertLengthOneOrTwo,
  assertLengthTwo,
  assertLengthZero,
  assertNonNegativeNumber,
  assertFiniteNumber,
  assertNumberNotZero,
  assertPositiveNumber,
} from '../../utils'
import { BuiltinNormalExpressions } from './interface'

export const math: BuiltinNormalExpressions = {
  '1+': {
    evaluate: ([first]: unknown[]): number => {
      assertFiniteNumber(first)
      return asFiniteNumber(first + 1)
    },
    validate: ({ params }) => assertLengthOne(params),
  },

  '1-': {
    evaluate: ([first]: unknown[]): number => {
      assertFiniteNumber(first)
      return asFiniteNumber(first - 1)
    },
    validate: ({ params }) => assertLengthOne(params),
  },

  '+': {
    evaluate: (params: unknown[]): number => {
      return asFiniteNumber(
        params.reduce((result: number, param) => {
          assertFiniteNumber(param)
          return result + param
        }, 0),
      )
    },
  },

  '*': {
    evaluate: (params: unknown[]): number => {
      return asFiniteNumber(
        params.reduce((result: number, param) => {
          assertFiniteNumber(param)
          return result * param
        }, 1),
      )
    },
  },

  '/': {
    evaluate: (params: unknown[]): number => {
      if (params.length === 0) {
        return 1
      }
      const [first, ...rest] = params
      assertFiniteNumber(first)
      if (rest.length === 0) {
        assertNumberNotZero(first)
        return 1 / first
      }
      return asFiniteNumber(
        rest.reduce((result: number, param) => {
          assertNumberNotZero(param)
          return result / param
        }, first),
      )
    },
  },

  '-': {
    evaluate: ([first, ...rest]: unknown[]): number => {
      if (!first) {
        return 0
      }
      assertFiniteNumber(first)
      if (rest.length === 0) {
        return -first
      }
      return asFiniteNumber(
        rest.reduce((result: number, param) => {
          assertFiniteNumber(param)
          return result - param
        }, first),
      )
    },
  },

  '%': {
    evaluate: ([first, second]: unknown[]): number => {
      assertFiniteNumber(first)
      assertNumberNotZero(second)
      return asFiniteNumber(first % second)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
  },

  sqrt: {
    evaluate: ([first]: unknown[]): number => {
      assertNonNegativeNumber(first)
      return asFiniteNumber(Math.sqrt(first))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  cbrt: {
    evaluate: ([first]: unknown[]): number => {
      assertNonNegativeNumber(first)
      return asFiniteNumber(Math.cbrt(first))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  pow: {
    evaluate: ([first, second]: unknown[]): number => {
      assertFiniteNumber(first)
      assertFiniteNumber(second)
      return asFiniteNumber(Math.pow(first, second))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
  },

  round: {
    evaluate: (params: unknown[]): number => {
      const [value, decimals] = params
      assertFiniteNumber(value)
      if (params.length === 1 || decimals === 0) {
        return asFiniteNumber(Math.round(value))
      }
      assertPositiveNumber(decimals)
      assertInteger(decimals)
      const factor = Math.pow(10, decimals)
      return asFiniteNumber(Math.round(value * factor) / factor)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOneOrTwo(params),
  },

  trunc: {
    evaluate: ([first]: unknown[]): number => {
      assertFiniteNumber(first)
      return asFiniteNumber(Math.trunc(first))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  floor: {
    evaluate: ([first]: unknown[]): number => {
      assertFiniteNumber(first)
      return asFiniteNumber(Math.floor(first))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  ceil: {
    evaluate: ([first]: unknown[]): number => {
      assertFiniteNumber(first)
      return asFiniteNumber(Math.ceil(first))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  random: {
    evaluate: ([first]: unknown[]): number => {
      assertPositiveNumber(first)
      return asFiniteNumber(Math.random() * first)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  '>': {
    evaluate: ([first, ...rest]: unknown[]): boolean => {
      assertFiniteNumber(first)

      if (rest.length === 0) {
        return true
      }

      let currentValue = first
      for (const param of rest) {
        assertFiniteNumber(param)
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
      assertFiniteNumber(first)

      if (rest.length === 0) {
        return true
      }

      let currentValue = first
      for (const param of rest) {
        assertFiniteNumber(param)
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
      assertFiniteNumber(first)
      if (rest.length === 0) {
        return true
      }

      let currentValue = first
      for (const param of rest) {
        assertFiniteNumber(param)
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
      assertFiniteNumber(first)
      if (rest.length === 0) {
        return true
      }

      let currentValue = first
      for (const param of rest) {
        assertFiniteNumber(param)
        if (currentValue > param) {
          return false
        }
        currentValue = param
      }
      return true
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOneOrMore(params),
  },

  min: {
    evaluate: ([first, ...rest]: unknown[]): number => {
      assertFiniteNumber(first)
      if (rest.length === 0) {
        return first
      }

      return rest.reduce((min: number, value) => {
        assertFiniteNumber(value)
        return Math.min(min, value)
      }, first)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOneOrMore(params),
  },

  max: {
    evaluate: ([first, ...rest]: unknown[]): number => {
      assertFiniteNumber(first)
      if (rest.length === 0) {
        return first
      }

      return rest.reduce((min: number, value) => {
        assertFiniteNumber(value)
        return Math.max(min, value)
      }, first)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOneOrMore(params),
  },

  abs: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return Math.abs(value)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  sign: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return Math.sign(value)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  e: {
    evaluate: (): number => {
      return Math.E
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthZero(params),
  },

  pi: {
    evaluate: (): number => {
      return Math.PI
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthZero(params),
  },

  exp: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.exp(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  log: {
    evaluate: ([value]: unknown[]): number => {
      assertPositiveNumber(value)
      return asFiniteNumber(Math.log(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  log2: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.log2(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  log10: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.log10(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  sin: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.sin(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  asin: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.asin(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  sinh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.sinh(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  asinh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.asinh(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  cos: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.cos(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  acos: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.acos(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  cosh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.cosh(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  acosh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.acosh(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  tan: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.tan(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  atan: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.atan(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  tanh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.tanh(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  atanh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.atanh(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },
}
