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
  inc: {
    evaluate: ([first]: unknown[]): number => {
      assertFiniteNumber(first)
      return asFiniteNumber(first + 1)
    },
    validate: node => assertLength(1, node),
  },

  dec: {
    evaluate: ([first]: unknown[]): number => {
      assertFiniteNumber(first)
      return asFiniteNumber(first - 1)
    },
    validate: node => assertLength(1, node),
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

  mod: {
    evaluate: ([first, second]: unknown[]): number => {
      assertFiniteNumber(first)
      assertNumberNotZero(second)
      return asFiniteNumber(first % second)
    },
    validate: (node: NormalExpressionNode): void => assertLength(2, node),
  },

  sqrt: {
    evaluate: ([first]: unknown[]): number => {
      assertNonNegativeNumber(first)
      return asFiniteNumber(Math.sqrt(first))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  cbrt: {
    evaluate: ([first]: unknown[]): number => {
      assertNonNegativeNumber(first)
      return asFiniteNumber(Math.cbrt(first))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  pow: {
    evaluate: ([first, second]: unknown[]): number => {
      assertFiniteNumber(first)
      assertFiniteNumber(second)
      return asFiniteNumber(Math.pow(first, second))
    },
    validate: (node: NormalExpressionNode): void => assertLength(2, node),
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
    validate: (node: NormalExpressionNode): void => assertLength({ min: 1, max: 2 }, node),
  },

  trunc: {
    evaluate: ([first]: unknown[]): number => {
      assertFiniteNumber(first)
      return asFiniteNumber(Math.trunc(first))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  floor: {
    evaluate: ([first]: unknown[]): number => {
      assertFiniteNumber(first)
      return asFiniteNumber(Math.floor(first))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  ceil: {
    evaluate: ([first]: unknown[]): number => {
      assertFiniteNumber(first)
      return asFiniteNumber(Math.ceil(first))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  rand: {
    evaluate: (parameters: unknown[]): number => {
      const number = parameters.length === 1 ? parameters[0] : 1
      assertPositiveNumber(number)
      return asFiniteNumber(Math.random() * number)
    },
    validate: (node: NormalExpressionNode): void => assertLength({ min: 0, max: 1 }, node),
  },

  'rand-int': {
    evaluate: ([first]: unknown[]): number => {
      assertPositiveNumber(first)
      return asFiniteNumber(Math.floor(Math.random() * first))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
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
    validate: (node: NormalExpressionNode): void => assertLength({ min: 1 }, node),
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
    validate: (node: NormalExpressionNode): void => assertLength({ min: 1 }, node),
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
    validate: (node: NormalExpressionNode): void => assertLength({ min: 1 }, node),
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
    validate: (node: NormalExpressionNode): void => assertLength({ min: 1 }, node),
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
    validate: (node: NormalExpressionNode): void => assertLength({ min: 1 }, node),
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
    validate: (node: NormalExpressionNode): void => assertLength({ min: 1 }, node),
  },

  abs: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return Math.abs(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  sign: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return Math.sign(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  e: {
    evaluate: (): number => {
      return Math.E
    },
    validate: (node: NormalExpressionNode): void => assertLength(0, node),
  },

  pi: {
    evaluate: (): number => {
      return Math.PI
    },
    validate: (node: NormalExpressionNode): void => assertLength(0, node),
  },

  exp: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.exp(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  log: {
    evaluate: ([value]: unknown[]): number => {
      assertPositiveNumber(value)
      return asFiniteNumber(Math.log(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  log2: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.log2(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  log10: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.log10(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  sin: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.sin(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  asin: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.asin(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  sinh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.sinh(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  asinh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.asinh(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  cos: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.cos(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  acos: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.acos(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  cosh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.cosh(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  acosh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.acosh(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  tan: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.tan(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  atan: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.atan(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  tanh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.tanh(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  atanh: {
    evaluate: ([value]: unknown[]): number => {
      assertFiniteNumber(value)
      return asFiniteNumber(Math.atanh(value))
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },
}
