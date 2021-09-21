import { NormalExpressionNode } from '../../../parser/interface'
import {
  asFiniteNumber,
  assertInteger,
  assertNonNegativeNumber,
  assertFiniteNumber,
  assertNumberNotZero,
  assertPositiveNumber,
  assertLength,
} from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const mathNormalExpression: BuiltinNormalExpressions = {
  '1+': {
    evaluate: ([first]: unknown[]): number => {
      assertFiniteNumber(first)
      return asFiniteNumber(first + 1)
    },
    validate: ({ params }) => assertLength(1, params),
  },

  '1-': {
    evaluate: ([first]: unknown[]): number => {
      assertFiniteNumber(first)
      return asFiniteNumber(first - 1)
    },
    validate: ({ params }) => assertLength(1, params),
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
    validate: ({ params }: NormalExpressionNode): void => assertLength(2, params),
  },

  sqrt: {
    evaluate: ([first]: unknown[]): number => {
      assertNonNegativeNumber(first)
      return asFiniteNumber(Math.sqrt(first))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  cbrt: {
    evaluate: ([first]: unknown[]): number => {
      assertNonNegativeNumber(first)
      return asFiniteNumber(Math.cbrt(first))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  pow: {
    evaluate: ([first, second]: unknown[]): number => {
      assertFiniteNumber(first)
      assertFiniteNumber(second)
      return asFiniteNumber(Math.pow(first, second))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(2, params),
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
    validate: ({ params }: NormalExpressionNode): void => assertLength({ min: 1, max: 2 }, params),
  },

  trunc: {
    evaluate: ([first]: unknown[]): number => {
      assertFiniteNumber(first)
      return asFiniteNumber(Math.trunc(first))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  floor: {
    evaluate: ([first]: unknown[]): number => {
      assertFiniteNumber(first)
      return asFiniteNumber(Math.floor(first))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  ceil: {
    evaluate: ([first]: unknown[]): number => {
      assertFiniteNumber(first)
      return asFiniteNumber(Math.ceil(first))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  random: {
    evaluate: ([first]: unknown[]): number => {
      assertPositiveNumber(first)
      return asFiniteNumber(Math.random() * first)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
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
    validate: ({ params }: NormalExpressionNode): void => assertLength({ min: 1 }, params),
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
    validate: ({ params }: NormalExpressionNode): void => assertLength({ min: 1 }, params),
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
    validate: ({ params }: NormalExpressionNode): void => assertLength({ min: 1 }, params),
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
    validate: ({ params }: NormalExpressionNode): void => assertLength({ min: 1 }, params),
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
    validate: ({ params }: NormalExpressionNode): void => assertLength({ min: 1 }, params),
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
    validate: ({ params }: NormalExpressionNode): void => assertLength({ min: 1 }, params),
  },

  abs: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return Math.abs(value)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  sign: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return Math.sign(value)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  e: {
    evaluate: (): number => {
      return Math.E
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(0, params),
  },

  pi: {
    evaluate: (): number => {
      return Math.PI
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(0, params),
  },

  exp: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.exp(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  log: {
    evaluate: ([value]: unknown[]): number => {
      assertPositiveNumber(value)
      return asFiniteNumber(Math.log(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  log2: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.log2(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  log10: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.log10(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  sin: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.sin(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  asin: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.asin(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  sinh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.sinh(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  asinh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.asinh(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  cos: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.cos(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  acos: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.acos(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  cosh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.cosh(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  acosh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.acosh(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  tan: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.tan(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  atan: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.atan(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  tanh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.tanh(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  atanh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.atanh(value))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },
}
