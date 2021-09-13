import {
  assertArray,
  assertInteger,
  assertLengthOne,
  assertLengthOneOrMore,
  assertLengthOneOrTwoOrThree,
  assertLengthThree,
  assertLengthTwo,
  assertLengthTwoOrMore,
  assertLispishFunction,
  assertNegativeNumber,
  assertNonNegativeNumber,
  assertNumber,
  assertNumberLt,
  assertNumberNotZero,
  assertPositiveNumber,
} from '../../utils'
import { BuiltinNormalExpressions } from './interface'

export const list: BuiltinNormalExpressions = {
  list: {
    evaluate: (params: unknown[]): unknown[] => params,
  },

  listf: {
    evaluate: ([size, value]: unknown[]): unknown[] => {
      assertInteger(size)
      assertPositiveNumber(size)
      const result = []
      for (let i = 0; i < size; i += 1) {
        result.push(value)
      }
      return result
    },
    validate: ({ params }) => assertLengthTwo(params),
  },

  range: {
    evaluate: (params: unknown[]): unknown[] => {
      const [first, second, third] = params
      let from: number
      let to: number
      let step: number
      assertNumber(first)

      if (params.length === 1) {
        from = 0
        to = first
        step = to >= 0 ? 1 : -1
      } else if (params.length === 2) {
        assertNumber(second)
        from = first
        to = second
        step = to >= from ? 1 : -1
      } else {
        assertNumber(second)
        assertNumber(third)
        from = first
        to = second
        step = third
        if (to > from) {
          assertPositiveNumber(step)
        } else if (to < from) {
          assertNegativeNumber(step)
        } else {
          assertNumberNotZero(step)
        }
      }

      const result: number[] = []

      for (let i = from; step < 0 ? i > to : i < to; i += step) {
        result.push(i)
      }

      return result
    },
    validate: ({ params }) => assertLengthOneOrTwoOrThree(params),
  },

  length: {
    evaluate: ([first]: unknown[]): unknown => {
      assertArray(first)
      return first.length
    },
    validate: ({ params }) => assertLengthOne(params),
  },

  append: {
    evaluate: (params: unknown[]): unknown => {
      const [first, ...rest] = params
      assertArray(first)
      return rest.reduce((result: unknown[], arr) => {
        assertArray(arr)
        return result.concat(arr)
      }, first)
    },
    validate: ({ params }) => assertLengthOneOrMore(params),
  },

  elt: {
    evaluate: ([arr, index]: unknown[]): unknown => {
      assertArray(arr)
      assertNonNegativeNumber(index)
      return arr[index]
    },
    validate: ({ params }) => assertLengthTwo(params),
  },

  selt: {
    evaluate: ([arr, index, value]: unknown[]): unknown => {
      assertArray(arr)
      assertNonNegativeNumber(index)
      assertNumberLt(index, arr.length)
      arr[index] = value
      return arr
    },
    validate: ({ params }) => assertLengthThree(params),
  },

  push: {
    evaluate: ([arr, ...values]: unknown[]): unknown[] => {
      assertArray(arr)
      arr.push(...values)
      return arr
    },
    validate: ({ params }) => assertLengthTwoOrMore(params),
  },

  pop: {
    evaluate: ([arr]: unknown[]): unknown => {
      assertArray(arr)
      return arr.pop()
    },
    validate: ({ params }) => assertLengthOne(params),
  },

  shift: {
    evaluate: ([arr]: unknown[]): unknown => {
      assertArray(arr)
      return arr.shift()
    },
    validate: ({ params }) => assertLengthOne(params),
  },

  unshift: {
    evaluate: ([arr, ...values]: unknown[]): unknown[] => {
      assertArray(arr)
      arr.unshift(...values)
      return arr
    },
    validate: ({ params }) => assertLengthTwoOrMore(params),
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
    validate: ({ params }) => assertLengthOneOrTwoOrThree(params),
  },

  splice: {
    evaluate: (params: unknown[]): unknown => {
      const [list, start, deleteCount, ...values] = params
      assertArray(list)
      assertInteger(start)

      if (params.length === 2) {
        return list.splice(start)
      }

      assertInteger(deleteCount)
      return list.splice(start, deleteCount, ...values)
    },
    validate: ({ params }) => assertLengthTwoOrMore(params),
  },

  reduce: {
    evaluate: ([first, second, third]: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      assertLispishFunction(first)
      assertArray(second)
      return second.reduce((result, elem) => {
        return evaluateLispishFunction(first, [result, elem], contextStack)
      }, third)
    },
    validate: ({ params }) => assertLengthThree(params),
  },

  'reduce-right': {
    evaluate: ([first, second, third]: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      assertLispishFunction(first)
      assertArray(second)
      return second.reduceRight((result, elem) => {
        return evaluateLispishFunction(first, [result, elem], contextStack)
      }, third)
    },
    validate: ({ params }) => assertLengthThree(params),
  },

  map: {
    evaluate: ([first, second]: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      assertLispishFunction(first)
      assertArray(second)
      return second.map(elem => evaluateLispishFunction(first, [elem], contextStack))
    },
    validate: ({ params }) => assertLengthTwo(params),
  },

  filter: {
    evaluate: ([first, second]: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      assertLispishFunction(first)
      assertArray(second)
      return second.filter(elem => evaluateLispishFunction(first, [elem], contextStack))
    },
    validate: ({ params }) => assertLengthTwo(params),
  },

  reverse: {
    evaluate: ([first]: unknown[]): unknown => {
      assertArray(first)
      return first.reverse()
    },
    validate: ({ params }) => assertLengthOne(params),
  },

  first: {
    evaluate: ([first]: unknown[]): unknown => {
      assertArray(first)
      return first[0]
    },
    validate: ({ params }) => assertLengthOne(params),
  },

  last: {
    evaluate: ([first]: unknown[]): unknown => {
      assertArray(first)
      return first[first.length - 1]
    },
    validate: ({ params }) => assertLengthOne(params),
  },

  rest: {
    evaluate: ([first]: unknown[]): unknown => {
      assertArray(first)
      if (first.length <= 1) {
        return []
      }
      return first.slice(1)
    },
    validate: ({ params }) => assertLengthOne(params),
  },

  cons: {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertArray(second)
      return [first, ...second]
    },
    validate: ({ params }) => assertLengthTwo(params),
  },
}
