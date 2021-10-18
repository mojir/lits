import { Arr } from '../../../interface'
import { NormalExpressionNode } from '../../../parser/interface'
import { assertLength, assertNumber, assertNonNegativeInteger } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const mathNormalExpression: BuiltinNormalExpressions = {
  inc: {
    evaluate: ([first]: Arr): number => {
      assertNumber(first)
      return first + 1
    },
    validate: node => assertLength(1, node),
  },

  dec: {
    evaluate: ([first]: Arr): number => {
      assertNumber(first)
      return first - 1
    },
    validate: node => assertLength(1, node),
  },

  '+': {
    evaluate: (params: Arr): number => {
      return params.reduce((result: number, param) => {
        assertNumber(param)
        return result + param
      }, 0)
    },
  },

  '*': {
    evaluate: (params: Arr): number => {
      return params.reduce((result: number, param) => {
        assertNumber(param)
        return result * param
      }, 1)
    },
  },

  '/': {
    evaluate: (params: Arr): number => {
      if (params.length === 0) {
        return 1
      }
      const [first, ...rest] = params
      assertNumber(first)
      if (rest.length === 0) {
        assertNumber(first)
        return 1 / first
      }
      return rest.reduce((result: number, param) => {
        assertNumber(param)
        return result / param
      }, first)
    },
  },

  '-': {
    evaluate: ([first, ...rest]: Arr): number => {
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

  quot: {
    evaluate: ([dividend, divisor]: Arr): number => {
      assertNumber(dividend)
      assertNumber(divisor)
      const quotient = Math.trunc(dividend / divisor)
      return quotient
    },
    validate: (node: NormalExpressionNode): void => assertLength(2, node),
  },

  mod: {
    evaluate: ([dividend, divisor]: Arr): number => {
      assertNumber(dividend)
      assertNumber(divisor)
      const quotient = Math.floor(dividend / divisor)
      return dividend - divisor * quotient
    },
    validate: (node: NormalExpressionNode): void => assertLength(2, node),
  },

  rem: {
    evaluate: ([dividend, divisor]: Arr): number => {
      assertNumber(dividend)
      assertNumber(divisor)
      const quotient = Math.trunc(dividend / divisor)
      return dividend - divisor * quotient
    },
    validate: (node: NormalExpressionNode): void => assertLength(2, node),
  },

  sqrt: {
    evaluate: ([first]: Arr): number => {
      assertNumber(first)
      return Math.sqrt(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  cbrt: {
    evaluate: ([first]: Arr): number => {
      assertNumber(first)
      return Math.cbrt(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  pow: {
    evaluate: ([first, second]: Arr): number => {
      assertNumber(first)
      assertNumber(second)
      return Math.pow(first, second)
    },
    validate: (node: NormalExpressionNode): void => assertLength(2, node),
  },

  round: {
    evaluate: (params: Arr): number => {
      const [value, decimals] = params
      assertNumber(value)
      if (params.length === 1 || decimals === 0) {
        return Math.round(value)
      }
      assertNonNegativeInteger(decimals)
      const factor = Math.pow(10, decimals)
      return Math.round(value * factor) / factor
    },
    validate: (node: NormalExpressionNode): void => assertLength({ min: 1, max: 2 }, node),
  },

  trunc: {
    evaluate: ([first]: Arr): number => {
      assertNumber(first)
      return Math.trunc(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  floor: {
    evaluate: ([first]: Arr): number => {
      assertNumber(first)
      return Math.floor(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  ceil: {
    evaluate: ([first]: Arr): number => {
      assertNumber(first)
      return Math.ceil(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  rand: {
    evaluate: (parameters: Arr): number => {
      const number = parameters.length === 1 ? parameters[0] : 1
      assertNumber(number)
      return Math.random() * number
    },
    validate: (node: NormalExpressionNode): void => assertLength({ min: 0, max: 1 }, node),
  },

  'rand-int': {
    evaluate: ([first]: Arr): number => {
      assertNumber(first)
      return Math.floor(Math.random() * Math.abs(first)) * Math.sign(first)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  min: {
    evaluate: ([first, ...rest]: Arr): number => {
      assertNumber(first)
      if (rest.length === 0) {
        return first
      }

      return rest.reduce((min: number, value) => {
        assertNumber(value)
        return Math.min(min, value)
      }, first)
    },
    validate: (node: NormalExpressionNode): void => assertLength({ min: 1 }, node),
  },

  max: {
    evaluate: ([first, ...rest]: Arr): number => {
      assertNumber(first)
      if (rest.length === 0) {
        return first
      }

      return rest.reduce((min: number, value) => {
        assertNumber(value)
        return Math.max(min, value)
      }, first)
    },
    validate: (node: NormalExpressionNode): void => assertLength({ min: 1 }, node),
  },

  abs: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.abs(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  sign: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.sign(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'max-safe-integer': {
    evaluate: (): number => {
      return Number.MAX_SAFE_INTEGER
    },
    validate: (node: NormalExpressionNode): void => assertLength(0, node),
  },

  'min-safe-integer': {
    evaluate: (): number => {
      return Number.MIN_SAFE_INTEGER
    },
    validate: (node: NormalExpressionNode): void => assertLength(0, node),
  },

  'max-value': {
    evaluate: (): number => {
      return Number.MAX_VALUE
    },
    validate: (node: NormalExpressionNode): void => assertLength(0, node),
  },

  'min-value': {
    evaluate: (): number => {
      return Number.MIN_VALUE
    },
    validate: (node: NormalExpressionNode): void => assertLength(0, node),
  },

  epsilon: {
    evaluate: (): number => {
      return Number.EPSILON
    },
    validate: (node: NormalExpressionNode): void => assertLength(0, node),
  },

  infinity: {
    evaluate: (): number => {
      return Number.POSITIVE_INFINITY
    },
    validate: (node: NormalExpressionNode): void => assertLength(0, node),
  },

  '-infinity': {
    evaluate: (): number => {
      return Number.NEGATIVE_INFINITY
    },
    validate: (node: NormalExpressionNode): void => assertLength(0, node),
  },

  nan: {
    evaluate: (): number => {
      return Number.NaN
    },
    validate: (node: NormalExpressionNode): void => assertLength(0, node),
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
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.exp(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  log: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.log(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  log2: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.log2(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  log10: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.log10(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  sin: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.sin(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  asin: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.asin(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  sinh: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.sinh(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  asinh: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.asinh(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  cos: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.cos(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  acos: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.acos(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  cosh: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.cosh(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  acosh: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.acosh(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  tan: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.tan(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  atan: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.atan(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  tanh: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.tanh(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  atanh: {
    evaluate: ([value]: Arr): number => {
      assertNumber(value)
      return Math.atanh(value)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },
}
