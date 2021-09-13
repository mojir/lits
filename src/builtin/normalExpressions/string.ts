import { NormalExpressionNode } from '../../parser/interface'
import {
  assertLengthOne,
  assertLengthTwo,
  assertLengthTwoOrThree,
  assertNonNegativeNumber,
  assertNumber,
  assertNumberGte,
  assertString,
} from '../../utils'
import { BuiltinNormalExpressions } from './interface'

export const string: BuiltinNormalExpressions = {
  substring: {
    evaluate: ([first, second, third]: unknown[]): unknown => {
      assertString(first)
      assertNumber(second)
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
      assertNumber(nbr)
      return `${nbr}`
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },
}
