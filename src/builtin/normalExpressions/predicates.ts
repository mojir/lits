import { NormalExpressionNode } from '../../parser/interface'
import { assertLengthOne, assertNumber } from '../../utils'
import { BuiltinNormalExpressions } from './interface'

export const predicates: BuiltinNormalExpressions = {
  stringp: {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'string',
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  numberp: {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'number',
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  booleanp: {
    evaluate: ([first]: unknown[]): boolean => typeof first === 'boolean',
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  undefinedp: {
    evaluate: ([first]: unknown[]): boolean => first === undefined,
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  nullp: {
    evaluate: ([first]: unknown[]): boolean => first === null,
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  zerop: {
    evaluate: ([first]: unknown[]): boolean => {
      assertNumber(first)
      return first === 0
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  evenp: {
    evaluate: ([first]: unknown[]): boolean => {
      assertNumber(first)
      return first % 2 === 0
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  oddp: {
    evaluate: ([first]: unknown[]): boolean => {
      assertNumber(first)
      return Number.isInteger(first) && first % 2 !== 0
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  arrayp: {
    evaluate: ([first]: unknown[]): boolean => {
      return Array.isArray(first)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  objectp: {
    evaluate: ([first]: unknown[]): boolean => first !== null && !Array.isArray(first) && typeof first === 'object',
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },
}
