import {
  assertArray,
  assertInteger,
  assertLength,
  assertLispishFunction,
  assertNegativeNumber,
  assertNonNegativeNumber,
  assertFiniteNumber,
  assertNumberNotZero,
  assertPositiveNumber,
  assertStringOrArray,
} from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
export const arrayNormalExpression: BuiltinNormalExpressions = {
  cons: {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertArray(second)
      return [first, ...second]
    },
    validate: node => assertLength(2, node),
  },
  nth: {
    evaluate: ([indexable, i]: unknown[]): unknown => {
      assertStringOrArray(indexable)
      assertInteger(i)

      const index = i < 0 ? indexable.length + i : i

      return indexable[index]
    },
    validate: node => assertLength(2, node),
  },
  'every?': {
    evaluate: ([first, second]: unknown[], contextStack, { evaluateLispishFunction }): boolean => {
      assertLispishFunction(first)
      assertArray(second)

      if (second.length === 0) {
        return false
      }

      return second.every(elem => evaluateLispishFunction(first, [elem], contextStack))
    },
    validate: node => assertLength(2, node),
  },
  filter: {
    evaluate: ([first, second]: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      assertLispishFunction(first)
      assertArray(second)
      return second.filter(elem => evaluateLispishFunction(first, [elem], contextStack))
    },
    validate: node => assertLength(2, node),
  },
  first: {
    evaluate: ([array]: unknown[]): unknown => {
      assertArray(array)
      return array[0]
    },
    validate: node => assertLength(1, node),
  },
  last: {
    evaluate: ([first]: unknown[]): unknown => {
      assertArray(first)
      return first[first.length - 1]
    },
    validate: node => assertLength(1, node),
  },
  array: {
    evaluate: (params: unknown[]): unknown[] => params,
  },
  map: {
    evaluate: (params: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      const [fn, firstList] = params
      assertLispishFunction(fn)
      assertArray(firstList)
      const length = firstList.length
      if (params.length === 2) {
        return firstList.map(elem => evaluateLispishFunction(fn, [elem], contextStack))
      }
      params.slice(2).forEach(listParam => {
        assertArray(listParam)
        if (length !== listParam.length) {
          throw Error(`All array arguments to "map" must have the same length`)
        }
      })

      const result: unknown[] = []
      for (let i = 0; i < length; i += 1) {
        const fnParams = params.slice(1).map(l => (l as unknown[])[i])
        result.push(evaluateLispishFunction(fn, fnParams, contextStack))
      }
      return result
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  pop: {
    evaluate: ([arr]: unknown[]): unknown[] => {
      assertArray(arr)
      const copy = [...arr]
      copy.pop()
      return copy
    },
    validate: node => assertLength(1, node),
  },
  position: {
    evaluate: ([first, second]: unknown[], contextStack, { evaluateLispishFunction }): number | undefined => {
      assertLispishFunction(first)
      assertArray(second)
      const index = second.findIndex(elem => evaluateLispishFunction(first, [elem], contextStack))
      return index !== -1 ? index : undefined
    },
    validate: node => assertLength(2, node),
  },
  'index-of': {
    evaluate: ([value, array]: unknown[]): number | undefined => {
      assertArray(array)
      const index = array.indexOf(value)
      return index !== -1 ? index : undefined
    },
    validate: node => assertLength(2, node),
  },
  push: {
    evaluate: ([arr, ...values]: unknown[]): unknown[] => {
      assertArray(arr)
      return [...arr, ...values]
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  range: {
    evaluate: (params: unknown[]): unknown[] => {
      const [first, second, third] = params
      let from: number
      let to: number
      let step: number
      assertFiniteNumber(first)

      if (params.length === 1) {
        from = 0
        to = first
        step = to >= 0 ? 1 : -1
      } else if (params.length === 2) {
        assertFiniteNumber(second)
        from = first
        to = second
        step = to >= from ? 1 : -1
      } else {
        assertFiniteNumber(second)
        assertFiniteNumber(third)
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
    validate: node => assertLength({ min: 1, max: 3 }, node),
  },
  reduce: {
    evaluate: (params: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      const fn = params[0]
      assertLispishFunction(fn)

      if (params.length === 2) {
        const [, arr] = params
        assertArray(arr)
        if (arr.length === 0) {
          return evaluateLispishFunction(fn, [], contextStack)
        } else if (arr.length === 1) {
          return arr[0]
        }
        return arr.slice(1).reduce((result, elem) => {
          return evaluateLispishFunction(fn, [result, elem], contextStack)
        }, arr[0])
      } else {
        const [, val, arr] = params
        assertArray(arr)
        if (arr.length === 0) {
          return val
        }
        return arr.reduce((result, elem) => {
          return evaluateLispishFunction(fn, [result, elem], contextStack)
        }, val)
      }
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  'reduce-right': {
    evaluate: (params: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      const fn = params[0]
      assertLispishFunction(fn)

      if (params.length === 2) {
        const [, arr] = params
        assertArray(arr)
        if (arr.length === 0) {
          return evaluateLispishFunction(fn, [], contextStack)
        } else if (arr.length === 1) {
          return arr[0]
        }
        return arr.slice(0, arr.length - 1).reduceRight((result, elem) => {
          return evaluateLispishFunction(fn, [result, elem], contextStack)
        }, arr[arr.length - 1])
      } else {
        const [, val, arr] = params
        assertArray(arr)
        if (arr.length === 0) {
          return val
        }
        return arr.reduceRight((result, elem) => {
          return evaluateLispishFunction(fn, [result, elem], contextStack)
        }, val)
      }
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  rest: {
    evaluate: ([first]: unknown[]): unknown => {
      assertArray(first)

      if (first.length <= 1) {
        return undefined
      }

      return first.slice(1)
    },
    validate: node => assertLength(1, node),
  },
  reverse: {
    evaluate: ([first]: unknown[]): unknown => {
      assertArray(first)
      return [...first].reverse()
    },
    validate: node => assertLength(1, node),
  },
  second: {
    evaluate: ([array]: unknown[]): unknown => {
      assertArray(array)
      return array[1]
    },
    validate: node => assertLength(1, node),
  },
  shift: {
    evaluate: ([arr]: unknown[]): unknown => {
      assertArray(arr)
      const copy = [...arr]
      copy.shift()
      return copy
    },
    validate: node => assertLength(1, node),
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
    validate: node => assertLength({ min: 1, max: 3 }, node),
  },
  some: {
    evaluate: ([first, second]: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      assertLispishFunction(first)
      assertArray(second)

      if (second.length === 0) {
        return undefined
      }

      return second.find(elem => evaluateLispishFunction(first, [elem], contextStack))
    },
    validate: node => assertLength(2, node),
  },
  sort: {
    evaluate: ([sorter, array]: unknown[], contextStack, { evaluateLispishFunction }): unknown[] => {
      assertLispishFunction(sorter)
      assertArray(array)

      const result = [...array]
      result.sort((a, b) => {
        const compareValue = evaluateLispishFunction(sorter, [a, b], contextStack)
        assertFiniteNumber(compareValue)
        return compareValue
      })
      return result
    },
    validate: node => assertLength(2, node),
  },
  take: {
    evaluate: ([array, n]: unknown[]): unknown => {
      assertArray(array)
      assertNonNegativeNumber(n)
      assertInteger(n)

      return array.slice(0, n)
    },
    validate: node => assertLength(2, node),
  },
  'take-last': {
    evaluate: ([array, n]: unknown[]): unknown => {
      assertArray(array)
      assertNonNegativeNumber(n)
      assertInteger(n)

      const from = array.length - n
      return array.slice(from)
    },
    validate: node => assertLength(2, node),
  },
  'take-while': {
    evaluate: ([predicate, array]: unknown[], contextStack, { evaluateLispishFunction }): unknown => {
      assertArray(array)
      assertLispishFunction(predicate)

      const result: unknown[] = []
      for (const item of array) {
        if (evaluateLispishFunction(predicate, [item], contextStack)) {
          result.push(item)
        } else {
          break
        }
      }
      return result
    },
    validate: node => assertLength(2, node),
  },
  unshift: {
    evaluate: ([arr, ...values]: unknown[]): unknown[] => {
      assertArray(arr)
      const copy = [...arr]
      copy.unshift(...values)
      return copy
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'random-sample': {
    evaluate: ([prob, arr]: unknown[]): unknown[] => {
      assertFiniteNumber(prob)
      assertArray(arr)
      return arr.filter(() => Math.random() < prob)
    },
    validate: node => assertLength(2, node),
  },
  repeat: {
    evaluate: ([count, value]: unknown[]): unknown[] => {
      assertInteger(count)
      assertNonNegativeNumber(count)
      const result: unknown[] = []
      for (let i = 0; i < count; i += 1) {
        result.push(value)
      }
      return result
    },
    validate: node => assertLength(2, node),
  },
}
