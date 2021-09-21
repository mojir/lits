import { NormalExpressionNode } from '../../../parser/interface'
import {
  assertLength,
  assertNonNegativeNumber,
  assertFiniteNumber,
  assertNumberGte,
  assertString,
  assertInteger,
} from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const stringNormalExpression: BuiltinNormalExpressions = {
  substring: {
    evaluate: ([first, second, third]: unknown[]): unknown => {
      assertString(first)
      assertFiniteNumber(second)
      assertNonNegativeNumber(second)

      if (third === undefined) {
        return (first as string).substring(second)
      }

      assertNumberGte(third, second)
      return (first as string).substring(second, third)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength({ min: 2, max: 3 }, params),
  },

  'string-length': {
    evaluate: ([first]: unknown[]): unknown => {
      assertString(first)
      return first.length
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'string-repeat': {
    evaluate: ([string, count]: unknown[]): string => {
      assertString(string)
      assertNonNegativeNumber(count)
      assertInteger(count)

      return string.repeat(count)
    },
    validate: ({ params }) => assertLength(2, params),
  },

  concat: {
    evaluate: (params: unknown[]): unknown => {
      return params.reduce((result: string, param) => {
        assertString(param)
        return result + param
      }, '')
    },
  },

  aref: {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertNonNegativeNumber(second)
      return first[second]
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(2, params),
  },

  'string>': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first > second
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(2, params),
  },

  'string<': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first < second
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(2, params),
  },

  'string>=': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first >= second
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(2, params),
  },

  'string<=': {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertString(first)
      assertString(second)
      return first <= second
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(2, params),
  },

  'string-reverse': {
    evaluate: ([str]: unknown[]): string => {
      assertString(str)
      return str.split('').reverse().join('')
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'string-to-number': {
    evaluate: ([str]: unknown[]): number => {
      assertString(str)
      const number = Number(str)
      if (Number.isNaN(number)) {
        throw Error(`Could not convert '${str}' to a number`)
      }
      return number
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'number-to-string': {
    evaluate: (params: unknown[]): string => {
      const [number, base] = params
      assertFiniteNumber(number)
      if (params.length === 1) {
        return Number(number).toString()
      } else {
        assertFiniteNumber(base)
        if (base !== 2 && base !== 8 && base !== 10 && base !== 16) {
          throw Error(`Expected base 2, 8, 10 or 16, got: ${base}`)
        }
        if (base === 10) {
          return Number(number).toString(base)
        }
        assertNonNegativeNumber(number)
        assertInteger(number)
        return Number(number).toString(base)
      }
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength({ min: 1, max: 2 }, params),
  },

  'lower-case': {
    evaluate: ([str]: unknown[]): string => {
      assertString(str)
      return str.toLowerCase()
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  'upper-case': {
    evaluate: ([str]: unknown[]): string => {
      assertString(str)
      return str.toUpperCase()
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  capitalize: {
    evaluate: ([str]: unknown[]): string => {
      assertString(str)
      const firstChar = str[0]
      if (!firstChar) {
        return ''
      }
      return `${firstChar.toUpperCase()}${str.substring(1)}`
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },

  trim: {
    evaluate: ([str]: unknown[]): string => {
      assertString(str)
      return str.trim()
    },
    validate: ({ params }: NormalExpressionNode): void => assertLength(1, params),
  },
}
