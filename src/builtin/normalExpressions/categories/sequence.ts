import { Arr, Seq } from '../../../interface'
import {
  assertArr,
  assertInteger,
  assertLength,
  assertLispishFunction,
  assertFiniteNumber,
  assertSeq,
  assertChar,
  isArr,
  assertString,
  assertCharArray,
  compare,
  isString,
  assertNumber,
} from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
export const sequenceNormalExpression: BuiltinNormalExpressions = {
  cons: {
    evaluate: ([elem, seq]: Arr): unknown => {
      assertSeq(seq)
      if (Array.isArray(seq)) {
        return [elem, ...seq]
      }
      assertChar(elem)
      return `${elem}${seq}`
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
  filter: {
    evaluate: ([fn, seq]: Arr, contextStack, { executeFunction }): Seq => {
      assertLispishFunction(fn)
      assertSeq(seq)
      if (Array.isArray(seq)) {
        return seq.filter(elem => executeFunction(fn, [elem], contextStack))
      }
      return seq
        .split(``)
        .filter(elem => executeFunction(fn, [elem], contextStack))
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
    evaluate: (params: Arr, contextStack, { executeFunction }): unknown => {
      const [fn, firstList] = params
      assertLispishFunction(fn)
      assertSeq(firstList)
      const isStringSeq = isString(firstList)

      const length = firstList.length
      if (params.length === 2) {
        if (isArr(firstList)) {
          return firstList.map(elem => executeFunction(fn, [elem], contextStack))
        } else {
          return firstList
            .split(``)
            .map(elem => {
              const newVal = executeFunction(fn, [elem], contextStack)
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
            const newValue = executeFunction(fn, fnParams, contextStack)
            assertChar(newValue)
            result += newValue
          }
          return result
        } else {
          const result: Arr = []
          for (let i = 0; i < length; i += 1) {
            const fnParams = params.slice(1).map(l => (l as Arr)[i])
            result.push(executeFunction(fn, fnParams, contextStack))
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
    evaluate: ([fn, seq]: Arr, contextStack, { executeFunction }): number | undefined => {
      assertLispishFunction(fn)
      assertSeq(seq)
      if (isString(seq)) {
        const index = seq.split(``).findIndex(elem => executeFunction(fn, [elem], contextStack))
        return index !== -1 ? index : undefined
      } else {
        const index = seq.findIndex(elem => executeFunction(fn, [elem], contextStack))
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
    evaluate: (params: Arr, contextStack, { executeFunction }): unknown => {
      const fn = params[0]
      assertLispishFunction(fn)

      if (params.length === 2) {
        const [, arr] = params
        assertSeq(arr)
        if (arr.length === 0) {
          return executeFunction(fn, [], contextStack)
        } else if (arr.length === 1) {
          return arr[0]
        }
        if (isString(arr)) {
          const chars = arr.split(``)
          return chars.slice(1).reduce((result: unknown, elem) => {
            const val = executeFunction(fn, [result, elem], contextStack)
            return val
          }, chars[0] as unknown)
        } else {
          return arr.slice(1).reduce((result, elem) => {
            return executeFunction(fn, [result, elem], contextStack)
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
            const newVal = executeFunction(fn, [result, elem], contextStack)
            return newVal
          }, val as unknown)
        } else {
          if (seq.length === 0) {
            return val
          }
          return seq.reduce((result, elem) => {
            return executeFunction(fn, [result, elem], contextStack)
          }, val)
        }
      }
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  'reduce-right': {
    evaluate: (params: Arr, contextStack, { executeFunction }): unknown => {
      const fn = params[0]
      assertLispishFunction(fn)

      if (params.length === 2) {
        const [, seq] = params
        assertSeq(seq)
        if (seq.length === 0) {
          return executeFunction(fn, [], contextStack)
        } else if (seq.length === 1) {
          return seq[0]
        }
        if (isString(seq)) {
          const chars = seq.split(``)
          return chars.slice(0, chars.length - 1).reduceRight((result, elem) => {
            const newVal = executeFunction(fn, [result, elem], contextStack)
            return newVal
          }, chars[chars.length - 1] as unknown)
        } else {
          return seq.slice(0, seq.length - 1).reduceRight((result, elem) => {
            return executeFunction(fn, [result, elem], contextStack)
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
            const newVal = executeFunction(fn, [result, elem], contextStack)
            return newVal
          }, val as unknown)
        } else {
          if (seq.length === 0) {
            return val
          }
          return seq.reduceRight((result, elem) => {
            return executeFunction(fn, [result, elem], contextStack)
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
  nthrest: {
    evaluate: ([seq, count]: Arr): Arr | string => {
      assertSeq(seq)
      assertFiniteNumber(count)
      const integerCount = Math.max(Math.ceil(count), 0)
      if (Array.isArray(seq)) {
        return seq.slice(integerCount)
      }
      return seq.substr(integerCount)
    },
    validate: node => assertLength(2, node),
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
  nthnext: {
    evaluate: ([seq, count]: Arr): Arr | string | undefined => {
      assertSeq(seq)
      assertFiniteNumber(count)
      const integerCount = Math.max(Math.ceil(count), 0)
      if (seq.length <= count) {
        return undefined
      }
      if (Array.isArray(seq)) {
        return seq.slice(integerCount)
      }
      return seq.substr(integerCount)
    },
    validate: node => assertLength(2, node),
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
    evaluate: ([fn, seq]: Arr, contextStack, { executeFunction }): unknown => {
      assertLispishFunction(fn)
      assertSeq(seq)

      if (seq.length === 0) {
        return undefined
      }

      if (isString(seq)) {
        return seq.split(``).find(elem => executeFunction(fn, [elem], contextStack))
      }

      return seq.find(elem => executeFunction(fn, [elem], contextStack))
    },
    validate: node => assertLength(2, node),
  },
  sort: {
    evaluate: (params: Arr, contextStack, { executeFunction }): Seq => {
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
            const compareValue = executeFunction(comparer, [a, b], contextStack)
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
          const compareValue = executeFunction(comparer, [a, b], contextStack)
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
      assertNumber(n)
      assertSeq(input)
      const number = Math.max(Math.ceil(n), 0)
      return input.slice(0, number)
    },
    validate: node => assertLength(2, node),
  },
  'take-last': {
    evaluate: ([n, array]: Arr): Seq => {
      assertSeq(array)
      assertNumber(n)
      const number = Math.max(Math.ceil(n), 0)
      const from = array.length - number
      return array.slice(from)
    },
    validate: node => assertLength(2, node),
  },
  'take-while': {
    evaluate: ([fn, seq]: Arr, contextStack, { executeFunction }): unknown => {
      assertSeq(seq)
      assertLispishFunction(fn)

      const result: Arr = []
      for (const item of seq) {
        if (executeFunction(fn, [item], contextStack)) {
          result.push(item)
        } else {
          break
        }
      }
      return isString(seq) ? result.join(``) : result
    },
    validate: node => assertLength(2, node),
  },
  drop: {
    evaluate: ([n, input]: Arr): Seq => {
      assertNumber(n)
      const number = Math.max(Math.ceil(n), 0)
      assertSeq(input)
      return input.slice(number)
    },
    validate: node => assertLength(2, node),
  },
  'drop-last': {
    evaluate: ([n, array]: Arr): Seq => {
      assertSeq(array)
      assertNumber(n)
      const number = Math.max(Math.ceil(n), 0)

      const from = array.length - number
      return array.slice(0, from)
    },
    validate: node => assertLength(2, node),
  },
  'drop-while': {
    evaluate: ([fn, seq]: Arr, contextStack, { executeFunction }): unknown => {
      assertSeq(seq)
      assertLispishFunction(fn)

      if (Array.isArray(seq)) {
        const from = seq.findIndex(elem => !executeFunction(fn, [elem], contextStack))
        return seq.slice(from)
      }
      const charArray = seq.split(``)
      const from = charArray.findIndex(elem => !executeFunction(fn, [elem], contextStack))
      return charArray.slice(from).join(``)
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
  'rand-nth': {
    evaluate: ([seq]: Arr): unknown => {
      assertSeq(seq)
      if (seq.length === 0) {
        return undefined
      }

      const index = Math.floor(Math.random() * seq.length)

      if (isString(seq)) {
        return seq.split(``)[index]
      }
      return seq[index]
    },
    validate: node => assertLength(1, node),
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
  distinct: {
    evaluate: ([input]: Arr): Seq => {
      assertSeq(input)
      if (Array.isArray(input)) {
        return Array.from(new Set(input))
      }
      return Array.from(new Set(input.split(``))).join(``)
    },
    validate: node => assertLength(1, node),
  },
  remove: {
    evaluate: ([fn, input], contextStack, { executeFunction }): Seq => {
      assertLispishFunction(fn)
      assertSeq(input)
      if (Array.isArray(input)) {
        return input.filter(elem => !executeFunction(fn, [elem], contextStack))
      }
      return input
        .split(``)
        .filter(elem => !executeFunction(fn, [elem], contextStack))
        .join(``)
    },
    validate: node => assertLength(2, node),
  },
  'split-at': {
    evaluate: ([pos, seq]): Seq => {
      assertFiniteNumber(pos)
      const intPos = Math.max(0, Math.ceil(pos))
      assertSeq(seq)
      return [seq.slice(0, intPos), seq.slice(intPos)]
    },
    validate: node => assertLength(2, node),
  },
}
