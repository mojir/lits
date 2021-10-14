import { Arr, Seq } from '../../../interface'
import {
  assertArr,
  assertInteger,
  assertLength,
  assertLispishFunction,
  assertNonNegativeInteger,
  assertFiniteNumber,
  assertSeq,
  assertChar,
  isArr,
  assertString,
  assertCharArray,
  compare,
  isString,
} from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
export const sequenceNormalExpression: BuiltinNormalExpressions = {
  cons: {
    evaluate: ([seq, second]: Arr): unknown => {
      assertSeq(second)
      if (Array.isArray(second)) {
        return [seq, ...second]
      }
      assertChar(seq)
      return `${seq}${second}`
    },
    validate: node => assertLength(2, node),
  },
  nth: {
    evaluate: ([seq, i]: Arr): unknown => {
      assertSeq(seq)
      assertInteger(i)

      return seq[i]
    },
    validate: node => assertLength(2, node),
  },
  'every?': {
    evaluate: ([fn, seq]: Arr, contextStack, { evaluateLispishFunction }): boolean => {
      assertLispishFunction(fn)
      assertSeq(seq)

      if (seq.length === 0) {
        return false
      }
      if (Array.isArray(seq)) {
        return seq.every(elem => evaluateLispishFunction(fn, [elem], contextStack))
      }
      return seq.split(``).every(elem => evaluateLispishFunction(fn, [elem], contextStack))
    },
    validate: node => assertLength(2, node),
  },
  filter: {
    evaluate: ([fn, seq]: Arr, contextStack, { evaluateLispishFunction }): Seq => {
      assertLispishFunction(fn)
      assertSeq(seq)
      if (Array.isArray(seq)) {
        return seq.filter(elem => evaluateLispishFunction(fn, [elem], contextStack))
      }
      return seq
        .split(``)
        .filter(elem => evaluateLispishFunction(fn, [elem], contextStack))
        .join(``)
    },
    validate: node => assertLength(2, node),
  },
  first: {
    evaluate: ([array]: Arr): unknown => {
      assertSeq(array)
      return array[0]
    },
    validate: node => assertLength(1, node),
  },
  last: {
    evaluate: ([first]: Arr): unknown => {
      assertSeq(first)
      return first[first.length - 1]
    },
    validate: node => assertLength(1, node),
  },
  map: {
    evaluate: (params: Arr, contextStack, { evaluateLispishFunction }): unknown => {
      const [fn, firstList] = params
      assertLispishFunction(fn)
      assertSeq(firstList)
      const isStringSeq = isString(firstList)

      const length = firstList.length
      if (params.length === 2) {
        if (isArr(firstList)) {
          return firstList.map(elem => evaluateLispishFunction(fn, [elem], contextStack))
        } else {
          return firstList
            .split(``)
            .map(elem => {
              const newVal = evaluateLispishFunction(fn, [elem], contextStack)
              assertChar(newVal)
              return newVal
            })
            .join(``)
        }
      } else {
        params.slice(2).forEach(collParam => {
          if (isStringSeq) {
            assertString(collParam)
          } else {
            assertArr(collParam)
          }
          if (length !== collParam.length) {
            throw Error(`All arguments to "map" must have the same length`)
          }
        })

        if (isStringSeq) {
          let result = ``
          for (let i = 0; i < length; i += 1) {
            const fnParams = params.slice(1).map(l => (l as string)[i]) as string[]
            const newValue = evaluateLispishFunction(fn, fnParams, contextStack)
            assertChar(newValue)
            result += newValue
          }
          return result
        } else {
          const result: Arr = []
          for (let i = 0; i < length; i += 1) {
            const fnParams = params.slice(1).map(l => (l as Arr)[i])
            result.push(evaluateLispishFunction(fn, fnParams, contextStack))
          }
          return result
        }
      }
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  pop: {
    evaluate: ([seq]: Arr): Seq => {
      assertSeq(seq)
      if (isString(seq)) {
        return seq.substr(0, seq.length - 1)
      }
      const copy = [...seq]
      copy.pop()
      return copy
    },
    validate: node => assertLength(1, node),
  },
  position: {
    evaluate: ([fn, seq]: Arr, contextStack, { evaluateLispishFunction }): number | undefined => {
      assertLispishFunction(fn)
      assertSeq(seq)
      if (isString(seq)) {
        const index = seq.split(``).findIndex(elem => evaluateLispishFunction(fn, [elem], contextStack))
        return index !== -1 ? index : undefined
      } else {
        const index = seq.findIndex(elem => evaluateLispishFunction(fn, [elem], contextStack))
        return index !== -1 ? index : undefined
      }
    },
    validate: node => assertLength(2, node),
  },
  'index-of': {
    evaluate: ([seq, value]: Arr): number | undefined => {
      assertSeq(seq)
      if (isString(seq)) {
        assertString(value)
        const index = seq.indexOf(value)
        return index !== -1 ? index : undefined
      } else {
        const index = seq.indexOf(value)
        return index !== -1 ? index : undefined
      }
    },
    validate: node => assertLength(2, node),
  },
  push: {
    evaluate: ([seq, ...values]: Arr): Seq => {
      assertSeq(seq)
      if (isString(seq)) {
        assertCharArray(values)
        return [seq, ...values].join(``)
      } else {
        return [...seq, ...values]
      }
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  reduce: {
    evaluate: (params: Arr, contextStack, { evaluateLispishFunction }): unknown => {
      const fn = params[0]
      assertLispishFunction(fn)

      if (params.length === 2) {
        const [, arr] = params
        assertSeq(arr)
        if (arr.length === 0) {
          return evaluateLispishFunction(fn, [], contextStack)
        } else if (arr.length === 1) {
          return arr[0]
        }
        if (isString(arr)) {
          const chars = arr.split(``)
          return chars.slice(1).reduce((result: unknown, elem) => {
            const val = evaluateLispishFunction(fn, [result, elem], contextStack)
            return val
          }, chars[0] as unknown)
        } else {
          return arr.slice(1).reduce((result, elem) => {
            return evaluateLispishFunction(fn, [result, elem], contextStack)
          }, arr[0])
        }
      } else {
        const [, val, seq] = params
        assertSeq(seq)
        if (isString(seq)) {
          assertString(val)
          if (seq.length === 0) {
            return val
          }
          return seq.split(``).reduce((result, elem) => {
            const newVal = evaluateLispishFunction(fn, [result, elem], contextStack)
            return newVal
          }, val as unknown)
        } else {
          if (seq.length === 0) {
            return val
          }
          return seq.reduce((result, elem) => {
            return evaluateLispishFunction(fn, [result, elem], contextStack)
          }, val)
        }
      }
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  'reduce-right': {
    evaluate: (params: Arr, contextStack, { evaluateLispishFunction }): unknown => {
      const fn = params[0]
      assertLispishFunction(fn)

      if (params.length === 2) {
        const [, seq] = params
        assertSeq(seq)
        if (seq.length === 0) {
          return evaluateLispishFunction(fn, [], contextStack)
        } else if (seq.length === 1) {
          return seq[0]
        }
        if (isString(seq)) {
          const chars = seq.split(``)
          return chars.slice(0, chars.length - 1).reduceRight((result, elem) => {
            const newVal = evaluateLispishFunction(fn, [result, elem], contextStack)
            return newVal
          }, chars[chars.length - 1] as unknown)
        } else {
          return seq.slice(0, seq.length - 1).reduceRight((result, elem) => {
            return evaluateLispishFunction(fn, [result, elem], contextStack)
          }, seq[seq.length - 1])
        }
      } else {
        const [, val, seq] = params
        assertSeq(seq)
        if (isString(seq)) {
          if (seq.length === 0) {
            return val
          }
          return seq.split(``).reduceRight((result, elem) => {
            const newVal = evaluateLispishFunction(fn, [result, elem], contextStack)
            return newVal
          }, val as unknown)
        } else {
          if (seq.length === 0) {
            return val
          }
          return seq.reduceRight((result, elem) => {
            return evaluateLispishFunction(fn, [result, elem], contextStack)
          }, val)
        }
      }
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  rest: {
    evaluate: ([first]: Arr): Arr | string => {
      assertSeq(first)
      if (Array.isArray(first)) {
        if (first.length <= 1) {
          return []
        }

        return first.slice(1)
      }
      return first.substr(1)
    },
    validate: node => assertLength(1, node),
  },
  next: {
    evaluate: ([first]: Arr): Arr | string | undefined => {
      assertSeq(first)
      if (Array.isArray(first)) {
        if (first.length <= 1) {
          return undefined
        }

        return first.slice(1)
      }
      if (first.length <= 1) {
        return undefined
      }
      return first.substr(1)
    },
    validate: node => assertLength(1, node),
  },
  reverse: {
    evaluate: ([first]: Arr): unknown => {
      assertSeq(first)
      if (Array.isArray(first)) {
        return [...first].reverse()
      }
      return first.split(``).reverse().join(``)
    },
    validate: node => assertLength(1, node),
  },
  second: {
    evaluate: ([array]: Arr): unknown => {
      assertSeq(array)
      return array[1]
    },
    validate: node => assertLength(1, node),
  },
  shift: {
    evaluate: ([seq]: Arr): unknown => {
      assertSeq(seq)
      if (isString(seq)) {
        return seq.substr(1)
      }
      const copy = [...seq]
      copy.shift()
      return copy
    },
    validate: node => assertLength(1, node),
  },
  slice: {
    evaluate: (params: Arr): unknown => {
      const [seq, from, to] = params
      assertSeq(seq)

      if (params.length === 1) {
        return seq
      }

      assertInteger(from)

      if (params.length === 2) {
        return seq.slice(from)
      }

      assertInteger(to)
      return seq.slice(from, to)
    },
    validate: node => assertLength({ min: 1, max: 3 }, node),
  },
  some: {
    evaluate: ([fn, seq]: Arr, contextStack, { evaluateLispishFunction }): unknown => {
      assertLispishFunction(fn)
      assertSeq(seq)

      if (seq.length === 0) {
        return undefined
      }

      if (isString(seq)) {
        return seq.split(``).find(elem => evaluateLispishFunction(fn, [elem], contextStack))
      }

      return seq.find(elem => evaluateLispishFunction(fn, [elem], contextStack))
    },
    validate: node => assertLength(2, node),
  },
  sort: {
    evaluate: (params: Arr, contextStack, { evaluateLispishFunction }): Seq => {
      const defaultComparer = params.length === 1
      const seq = defaultComparer ? params[0] : params[1]
      const comparer = defaultComparer ? null : params[0]
      assertSeq(seq)

      if (isString(seq)) {
        const result = seq.split(``)
        if (defaultComparer) {
          result.sort(compare)
        } else {
          assertLispishFunction(comparer)
          result.sort((a, b) => {
            const compareValue = evaluateLispishFunction(comparer, [a, b], contextStack)
            assertFiniteNumber(compareValue)
            return compareValue
          })
        }
        return result.join(``)
      }

      const result = [...seq]
      if (defaultComparer) {
        result.sort(compare)
      } else {
        result.sort((a, b) => {
          assertLispishFunction(comparer)
          const compareValue = evaluateLispishFunction(comparer, [a, b], contextStack)
          assertFiniteNumber(compareValue)
          return compareValue
        })
      }
      return result
    },
    validate: node => assertLength({ min: 1, max: 2 }, node),
  },
  take: {
    evaluate: ([n, input]: Arr): Seq => {
      assertInteger(n)
      assertSeq(input)
      const nonNegativeCount = Math.max(0, n)
      return input.slice(0, nonNegativeCount)
    },
    validate: node => assertLength(2, node),
  },
  'take-last': {
    evaluate: ([n, array]: Arr): Seq => {
      assertSeq(array)
      assertNonNegativeInteger(n)

      const from = array.length - n
      return array.slice(from)
    },
    validate: node => assertLength(2, node),
  },
  'take-while': {
    evaluate: ([fn, seq]: Arr, contextStack, { evaluateLispishFunction }): unknown => {
      assertSeq(seq)
      assertLispishFunction(fn)

      const result: Arr = []
      for (const item of seq) {
        if (evaluateLispishFunction(fn, [item], contextStack)) {
          result.push(item)
        } else {
          break
        }
      }
      return isString(seq) ? result.join(``) : result
    },
    validate: node => assertLength(2, node),
  },
  unshift: {
    evaluate: ([seq, ...values]: Arr): Seq => {
      assertSeq(seq)
      if (isString(seq)) {
        assertCharArray(values)
        return [...values, seq].join(``)
      }
      const copy = [...seq]
      copy.unshift(...values)
      return copy
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'random-sample': {
    evaluate: ([prob, seq]: Arr): Seq => {
      assertFiniteNumber(prob)
      assertSeq(seq)

      if (isString(seq)) {
        return seq
          .split(``)
          .filter(() => Math.random() < prob)
          .join(``)
      } else {
        return seq.filter(() => Math.random() < prob)
      }
    },
    validate: node => assertLength(2, node),
  },
  shuffle: {
    evaluate: ([input]: Arr): Seq => {
      assertSeq(input)
      const array: Arr = isString(input) ? [...input.split(``)] : [...input]
      let remainingLength = array.length
      let arrayElement: unknown
      let pickedIndex: number

      // Fisher–Yates Shuffle
      while (remainingLength) {
        remainingLength -= 1

        // Pick a remaining element
        pickedIndex = Math.floor(Math.random() * remainingLength)

        // And swap it with the current element.
        arrayElement = array[remainingLength]
        array[remainingLength] = array[pickedIndex]
        array[pickedIndex] = arrayElement
      }

      return isString(input) ? array.join(``) : array
    },
    validate: node => assertLength(1, node),
  },
}
