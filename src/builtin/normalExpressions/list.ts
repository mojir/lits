import { NormalExpressionNode } from '../../parser/interface'
import {
  assertArray,
  assertInteger,
  assertLengthOne,
  assertLengthOneOrTwoOrThree,
  assertLengthThree,
  assertLengthTwo,
  assertLispishFunction,
  assertNonNegativeNumber,
} from '../../utils'
import { BuiltinNormalExpressions } from './interface'

export const list: BuiltinNormalExpressions = {
  array: {
    evaluate: (params: unknown[]): unknown => params,
  },

  length: {
    evaluate: ([first]: unknown[]): unknown => {
      assertArray(first)
      return first.length
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  append: {
    evaluate: (params: unknown[]): unknown => {
      if (params.length === 0) {
        return []
      }
      const [first, ...rest] = params
      assertArray(first)
      return rest.reduce((result: unknown[], param) => {
        return result.concat(param)
      }, first)
    },
  },

  elt: {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertArray(first)
      assertNonNegativeNumber(second)
      return first[second]
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
  },

  slice: {
    evaluate: (params: unknown[]): unknown => {
      const [first, second, third] = params
      assertArray(first)
      if (params.length === 1) {
        return [...first]
      }

      assertInteger(second)
      if (params.length === 2) {
        return first.slice(second)
      }

      assertInteger(third)
      return first.slice(second, third)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOneOrTwoOrThree(params),
  },

  reduce: {
    evaluate: ([first, second, third]: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      assertLispishFunction(first)
      assertArray(second)
      return second.reduce((result, elem) => {
        return evaluateLispishFunction(first, [result, elem], contextStack)
      }, third)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthThree(params),
  },

  reduceRight: {
    evaluate: ([first, second, third]: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      assertLispishFunction(first)
      assertArray(second)
      return second.reduceRight((result, elem) => {
        return evaluateLispishFunction(first, [result, elem], contextStack)
      }, third)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthThree(params),
  },

  map: {
    evaluate: ([first, second]: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      assertLispishFunction(first)
      assertArray(second)
      return second.map(elem => evaluateLispishFunction(first, [elem], contextStack))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
  },

  filter: {
    evaluate: ([first, second]: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      assertLispishFunction(first)
      assertArray(second)
      return second.filter(elem => evaluateLispishFunction(first, [elem], contextStack))
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
  },

  first: {
    evaluate: ([first]: unknown[]): unknown => {
      assertArray(first)
      return first[0]
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  rest: {
    evaluate: ([first]: unknown[]): unknown => {
      assertArray(first)
      if (first.length <= 1) {
        return []
      }
      return first.slice(1)
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthOne(params),
  },

  cons: {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertArray(second)
      return [first, ...second]
    },
    validate: ({ params }: NormalExpressionNode): void => assertLengthTwo(params),
  },
}
