import { NormalExpressionNode } from '../../../parser/interface'
import { assertNumberOfParams, number } from '../../../utils/assertion'
import { BuiltinNormalExpressions } from '../../interface'

export const mathNormalExpression: BuiltinNormalExpressions = {
  inc: {
    evaluate: ([first], debugInfo): number => {
      number.assert(first, debugInfo)
      return first + 1
    },
    validate: node => assertNumberOfParams(1, node),
  },

  dec: {
    evaluate: ([first], debugInfo): number => {
      number.assert(first, debugInfo)
      return first - 1
    },
    validate: node => assertNumberOfParams(1, node),
  },

  '+': {
    evaluate: (params, debugInfo): number => {
      return params.reduce((result: number, param) => {
        number.assert(param, debugInfo)
        return result + param
      }, 0)
    },
  },

  '*': {
    evaluate: (params, debugInfo): number => {
      return params.reduce((result: number, param) => {
        number.assert(param, debugInfo)
        return result * param
      }, 1)
    },
  },

  '/': {
    evaluate: (params, debugInfo): number => {
      if (params.length === 0) {
        return 1
      }
      const [first, ...rest] = params
      number.assert(first, debugInfo)
      if (rest.length === 0) {
        number.assert(first, debugInfo)
        return 1 / first
      }
      return rest.reduce((result: number, param) => {
        number.assert(param, debugInfo)
        return result / param
      }, first)
    },
  },

  '-': {
    evaluate: (params, debugInfo): number => {
      if (params.length === 0) {
        return 0
      }
      const [first, ...rest] = params
      number.assert(first, debugInfo)
      if (rest.length === 0) {
        return -first
      }
      return rest.reduce((result: number, param) => {
        number.assert(param, debugInfo)
        return result - param
      }, first)
    },
  },

  quot: {
    evaluate: ([dividend, divisor], debugInfo): number => {
      number.assert(dividend, debugInfo)
      number.assert(divisor, debugInfo)
      const quotient = Math.trunc(dividend / divisor)
      return quotient
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(2, node),
  },

  mod: {
    evaluate: ([dividend, divisor], debugInfo): number => {
      number.assert(dividend, debugInfo)
      number.assert(divisor, debugInfo)
      const quotient = Math.floor(dividend / divisor)
      return dividend - divisor * quotient
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(2, node),
  },

  rem: {
    evaluate: ([dividend, divisor], debugInfo): number => {
      number.assert(dividend, debugInfo)
      number.assert(divisor, debugInfo)
      const quotient = Math.trunc(dividend / divisor)
      return dividend - divisor * quotient
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(2, node),
  },

  sqrt: {
    evaluate: ([first], debugInfo): number => {
      number.assert(first, debugInfo)
      return Math.sqrt(first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  cbrt: {
    evaluate: ([first], debugInfo): number => {
      number.assert(first, debugInfo)
      return Math.cbrt(first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  pow: {
    evaluate: ([first, second], debugInfo): number => {
      number.assert(first, debugInfo)
      number.assert(second, debugInfo)
      return Math.pow(first, second)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(2, node),
  },

  round: {
    evaluate: (params, debugInfo): number => {
      const [value, decimals] = params
      number.assert(value, debugInfo)
      if (params.length === 1 || decimals === 0) {
        return Math.round(value)
      }
      number.assert(decimals, debugInfo, { integer: true, nonNegative: true })
      const factor = Math.pow(10, decimals)
      return Math.round(value * factor) / factor
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams({ min: 1, max: 2 }, node),
  },

  trunc: {
    evaluate: ([first], debugInfo): number => {
      number.assert(first, debugInfo)
      return Math.trunc(first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  floor: {
    evaluate: ([first], debugInfo): number => {
      number.assert(first, debugInfo)
      return Math.floor(first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  ceil: {
    evaluate: ([first], debugInfo): number => {
      number.assert(first, debugInfo)
      return Math.ceil(first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'rand!': {
    evaluate: (parameters, debugInfo): number => {
      const num = number.as(parameters.length === 1 ? parameters[0] : 1, debugInfo)
      return Math.random() * num
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams({ min: 0, max: 1 }, node),
  },

  'rand-int!': {
    evaluate: ([first], debugInfo): number => {
      number.assert(first, debugInfo)
      return Math.floor(Math.random() * Math.abs(first)) * Math.sign(first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  min: {
    evaluate: ([first, ...rest], debugInfo): number => {
      number.assert(first, debugInfo)
      if (rest.length === 0) {
        return first
      }

      return rest.reduce((min: number, value) => {
        number.assert(value, debugInfo)
        return Math.min(min, value)
      }, first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams({ min: 1 }, node),
  },

  max: {
    evaluate: ([first, ...rest], debugInfo): number => {
      number.assert(first, debugInfo)
      if (rest.length === 0) {
        return first
      }

      return rest.reduce((min: number, value) => {
        number.assert(value, debugInfo)
        return Math.max(min, value)
      }, first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams({ min: 1 }, node),
  },

  abs: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.abs(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  sign: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.sign(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'max-safe-integer': {
    evaluate: (): number => {
      return Number.MAX_SAFE_INTEGER
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  'min-safe-integer': {
    evaluate: (): number => {
      return Number.MIN_SAFE_INTEGER
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  'max-value': {
    evaluate: (): number => {
      return Number.MAX_VALUE
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  'min-value': {
    evaluate: (): number => {
      return Number.MIN_VALUE
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  epsilon: {
    evaluate: (): number => {
      return Number.EPSILON
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  'positive-infinity': {
    evaluate: (): number => {
      return Number.POSITIVE_INFINITY
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  'negative-infinity': {
    evaluate: (): number => {
      return Number.NEGATIVE_INFINITY
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  nan: {
    evaluate: (): number => {
      return Number.NaN
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  e: {
    evaluate: (): number => {
      return Math.E
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  pi: {
    evaluate: (): number => {
      return Math.PI
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  exp: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.exp(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  log: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.log(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  log2: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.log2(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  log10: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.log10(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  sin: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.sin(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  asin: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.asin(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  sinh: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.sinh(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  asinh: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.asinh(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  cos: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.cos(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  acos: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.acos(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  cosh: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.cosh(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  acosh: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.acosh(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  tan: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.tan(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  atan: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.atan(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  tanh: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.tanh(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  atanh: {
    evaluate: ([value], debugInfo): number => {
      number.assert(value, debugInfo)
      return Math.atanh(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },
}
