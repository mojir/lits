import { assertNumber } from '../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../interface'

export const mathNormalExpression: BuiltinNormalExpressions = {
  'inc': {
    evaluate: ([first], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      return first + 1
    },
    paramCount: 1,
  },

  'dec': {
    evaluate: ([first], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      return first - 1
    },
    paramCount: 1,
  },

  '+': {
    evaluate: (params, sourceCodeInfo): number => {
      return params.reduce((result: number, param) => {
        assertNumber(param, sourceCodeInfo)
        return result + param
      }, 0)
    },
    paramCount: {},
  },

  '*': {
    evaluate: (params, sourceCodeInfo): number => {
      return params.reduce((result: number, param) => {
        assertNumber(param, sourceCodeInfo)
        return result * param
      }, 1)
    },
    aliases: ['·'],
    paramCount: {},
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
    paramCount: {},
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
    paramCount: {},
  },

  'quot': {
    evaluate: ([dividend, divisor], sourceCodeInfo): number => {
      assertNumber(dividend, sourceCodeInfo)
      assertNumber(divisor, sourceCodeInfo)
      const quotient = Math.trunc(dividend / divisor)
      return quotient
    },
    paramCount: 2,
  },

  'mod': {
    evaluate: ([dividend, divisor], sourceCodeInfo): number => {
      assertNumber(dividend, sourceCodeInfo)
      assertNumber(divisor, sourceCodeInfo)
      const quotient = Math.floor(dividend / divisor)
      return dividend - divisor * quotient
    },
    paramCount: 2,
  },

  '%': {
    evaluate: ([dividend, divisor], sourceCodeInfo): number => {
      assertNumber(dividend, sourceCodeInfo)
      assertNumber(divisor, sourceCodeInfo)
      const quotient = Math.trunc(dividend / divisor)
      return dividend - divisor * quotient
    },
    paramCount: 2,
    aliases: ['rem'],
  },

  '√': {
    evaluate: ([first], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      return Math.sqrt(first)
    },
    paramCount: 1,
    aliases: ['sqrt'],
  },

  '∛': {
    evaluate: ([first], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      return Math.cbrt(first)
    },
    paramCount: 1,
    aliases: ['cbrt'],
  },

  '**': {
    evaluate: ([first, second], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      assertNumber(second, sourceCodeInfo)
      return first ** second
    },
    paramCount: 2,
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
    paramCount: { min: 1, max: 2 },
  },

  'trunc': {
    evaluate: ([first], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      return Math.trunc(first)
    },
    paramCount: 1,
  },

  'floor': {
    evaluate: ([first], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      return Math.floor(first)
    },
    paramCount: 1,
  },

  'ceil': {
    evaluate: ([first], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      return Math.ceil(first)
    },
    paramCount: 1,
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
    paramCount: { min: 1 },
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
    paramCount: { min: 1 },
  },

  'abs': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.abs(value)
    },
    paramCount: 1,
  },

  'sign': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.sign(value)
    },
    paramCount: 1,
  },

  'log': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.log(value)
    },
    paramCount: 1,
  },

  'log2': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.log2(value)
    },
    paramCount: 1,
  },

  'log10': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.log10(value)
    },
    paramCount: 1,
  },

  'sin': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.sin(value)
    },
    paramCount: 1,
  },

  'asin': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.asin(value)
    },
    paramCount: 1,
  },

  'sinh': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.sinh(value)
    },
    paramCount: 1,
  },

  'asinh': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.asinh(value)
    },
    paramCount: 1,
  },

  'cos': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.cos(value)
    },
    paramCount: 1,
  },

  'acos': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.acos(value)
    },
    paramCount: 1,
  },

  'cosh': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.cosh(value)
    },
    paramCount: 1,
  },

  'acosh': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.acosh(value)
    },
    paramCount: 1,
  },

  'tan': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.tan(value)
    },
    paramCount: 1,
  },

  'atan': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.atan(value)
    },
    paramCount: 1,
  },

  'tanh': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.tanh(value)
    },
    paramCount: 1,
  },

  'atanh': {
    evaluate: ([value], sourceCodeInfo): number => {
      assertNumber(value, sourceCodeInfo)
      return Math.atanh(value)
    },
    paramCount: 1,
  },
}
