import type { NormalExpressionNode } from '../../../parser/interface'
import { assertNumber } from '../../../typeGuards/number'
import { assertNumberOfParams } from '../../../typeGuards'
import type { BuiltinNormalExpressions } from '../../interface'

export const mathNormalExpression: BuiltinNormalExpressions = {
  'inc': {
    evaluate: ([first], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      return first + 1
    },
    validate: node => assertNumberOfParams(1, node),
  },

  'dec': {
    evaluate: ([first], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      return first - 1
    },
    validate: node => assertNumberOfParams(1, node),
  },

  '+': {
    evaluate: (params, sourceCodeInfo): number => {
      return params.reduce((result: number, param) => {
        assertNumber(param, sourceCodeInfo)
        return result + param
      }, 0)
    },
  },

  '*': {
    evaluate: (params, sourceCodeInfo): number => {
      return params.reduce((result: number, param) => {
        assertNumber(param, sourceCodeInfo)
        return result * param
      }, 1)
    },
  },

  '/': {
    evaluate: (params, sourceCodeInfo): number => {
      if (params.length === 0)
        return 1

      const [first, ...rest] = params
      assertNumber(first, sourceCodeInfo)
      if (rest.length === 0) {
        assertNumber(first, sourceCodeInfo)
        return 1 / first
      }
      return rest.reduce((result: number, param) => {
        assertNumber(param, sourceCodeInfo)
        return result / param
      }, first)
    },
  },

  '-': {
    evaluate: (params, sourceCodeInfo): number => {
      if (params.length === 0)
        return 0

      const [first, ...rest] = params
      assertNumber(first, sourceCodeInfo)
      if (rest.length === 0)
        return -first

      return rest.reduce((result: number, param) => {
        assertNumber(param, sourceCodeInfo)
        return result - param
      }, first)
    },
  },

  'quot': {
    evaluate: ([dividend, divisor], sourceCodeInfo): number => {
      assertNumber(dividend, sourceCodeInfo)
      assertNumber(divisor, sourceCodeInfo)
      const quotient = Math.trunc(dividend / divisor)
      return quotient
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(2, node),
  },

  'mod': {
    evaluate: ([dividend, divisor], sourceCodeInfo): number => {
      assertNumber(dividend, sourceCodeInfo)
      assertNumber(divisor, sourceCodeInfo)
      const quotient = Math.floor(dividend / divisor)
      return dividend - divisor * quotient
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(2, node),
  },

  '%': {
    evaluate: ([dividend, divisor], sourceCodeInfo): number => {
      assertNumber(dividend, sourceCodeInfo)
      assertNumber(divisor, sourceCodeInfo)
      const quotient = Math.trunc(dividend / divisor)
      return dividend - divisor * quotient
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(2, node),
  },

  'sqrt': {
    evaluate: ([first], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      return Math.sqrt(first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'cbrt': {
    evaluate: ([first], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      return Math.cbrt(first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  '**': {
    evaluate: ([first, second], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      assertNumber(second, sourceCodeInfo)
      return first ** second
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(2, node),
  },

  'round': {
    evaluate: (params, sourceCodeInfo): number => {
      const [value, decimals] = params
      assertNumber(value, sourceCodeInfo)
      if (params.length === 1 || decimals === 0)
        return Math.round(value)

      assertNumber(decimals, sourceCodeInfo, { integer: true, nonNegative: true })
      const factor = 10 ** decimals
      return Math.round(value * factor) / factor
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams({ min: 1, max: 2 }, node),
  },

  'trunc': {
    evaluate: ([first], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      return Math.trunc(first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'floor': {
    evaluate: ([first], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      return Math.floor(first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'ceil': {
    evaluate: ([first], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      return Math.ceil(first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'min': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      if (rest.length === 0)
        return first

      return rest.reduce((min: number, value) => {
        assertNumber(value, sourceCodeInfo)
        return Math.min(min, value)
      }, first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams({ min: 1 }, node),
  },

  'max': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      if (rest.length === 0)
        return first

      return rest.reduce((min: number, value) => {
        assertNumber(value, sourceCodeInfo)
        return Math.max(min, value)
      }, first)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams({ min: 1 }, node),
  },

  'abs': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.abs(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'sign': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.sign(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'max_safe_integer': {
    evaluate: (): number => {
      return Number.MAX_SAFE_INTEGER
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  'min_safe_integer': {
    evaluate: (): number => {
      return Number.MIN_SAFE_INTEGER
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  'max_value': {
    evaluate: (): number => {
      return Number.MAX_VALUE
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  'min_value': {
    evaluate: (): number => {
      return Number.MIN_VALUE
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  'epsilon': {
    evaluate: (): number => {
      return Number.EPSILON
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  'positive_infinity': {
    evaluate: (): number => {
      return Number.POSITIVE_INFINITY
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  'negative_infinity': {
    evaluate: (): number => {
      return Number.NEGATIVE_INFINITY
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  'nan': {
    evaluate: (): number => {
      return Number.NaN
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  'e': {
    evaluate: (): number => {
      return Math.E
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  'pi': {
    evaluate: (): number => {
      return Math.PI
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(0, node),
  },

  'exp': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.exp(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'log': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.log(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'log2': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.log2(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'log10': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.log10(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'sin': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.sin(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'asin': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.asin(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'sinh': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.sinh(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'asinh': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.asinh(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'cos': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.cos(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'acos': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.acos(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'cosh': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.cosh(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'acosh': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.acosh(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'tan': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.tan(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'atan': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.atan(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'tanh': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.tanh(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'atanh': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.atanh(value)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },
}
