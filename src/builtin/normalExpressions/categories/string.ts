import { NormalExpressionNode } from '../../../parser/interface'
import {
  assertLengthOne,
  assertLengthTwo,
  assertLengthTwoOrThree,
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
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwoOrThree(params),
  },

  'string-length': {
    evaluate: ([first]: unknown[]): unknown => {
      assertString(first)
      return first.length
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'string-repeat': {
    evaluate: ([string, count]: unknown[]): string => {
      assertString(string)
      assertNonNegativeNumber(count)
      assertInteger(count)

      return string.repeat(count)
    },
    validate: ({ params }) => assertLengthTwo(params),
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
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
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

  'string-reverse': {
    evaluate: ([str]: unknown[]): string => {
      assertString(str)
      return str.split('').reverse().join('')
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'string-to-number': {
    evaluate: ([str]: unknown[]): number => {
      assertString(str)
      const number = parseFloat(str)
      if (Number.isNaN(number)) {
        throw Error(`Could not convert '${str}' to a number`)
      }
      return number
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'number-to-string': {
    evaluate: ([nbr]: unknown[]): string => {
      assertFiniteNumber(nbr)
      return `${nbr}`
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'lower-case': {
    evaluate: ([str]: unknown[]): string => {
      assertString(str)
      return str.toLowerCase()
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  'upper-case': {
    evaluate: ([str]: unknown[]): string => {
      assertString(str)
      return str.toUpperCase()
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
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
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  trim: {
    evaluate: ([str]: unknown[]): string => {
      assertString(str)
      return str.trim()
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },
}