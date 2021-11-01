import { LitsError } from '../../../errors'
import { Any, Arr, Obj, Seq } from '../../../interface'
import { TokenMeta } from '../../../tokenizer/interface'
import {
  assertLength,
  assertFiniteNumber,
  assertChar,
  assertString,
  assertCharArray,
  compare,
  isString,
  toAny,
  toNonNegativeInteger,
  collHasKey,
  assertPositiveNumber,
} from '../../../utils'
import { any, litsFunction, number, sequence, array } from '../../../utils/assertion'
import { BuiltinNormalExpressions, NormalExpressionEvaluator } from '../../interface'

export const evaluateMap: NormalExpressionEvaluator<Arr | string> = (
  params: Arr,
  meta,
  contextStack,
  { executeFunction },
) => {
  const [fn, firstList] = params
  litsFunction.assert(fn, meta)
  sequence.assert(firstList, meta)
  const isStringSeq = isString(firstList)

  const length = firstList.length
  if (params.length === 2) {
    if (array.is(firstList)) {
      return firstList.map(elem => executeFunction(fn, [elem], meta, contextStack))
    } else {
      return firstList
        .split(``)
        .map(elem => {
          const newVal = executeFunction(fn, [elem], meta, contextStack)
          assertChar(newVal, meta)
          return newVal
        })
        .join(``)
    }
  } else {
    params.slice(2).forEach(collParam => {
      if (isStringSeq) {
        assertString(collParam, meta)
      } else {
        array.assert(collParam, meta)
      }
      if (length !== collParam.length) {
        throw new LitsError(`All arguments to "map" must have the same length`, meta)
      }
    })

    if (isStringSeq) {
      let result = ``
      for (let i = 0; i < length; i += 1) {
        const fnParams = params.slice(1).map(l => (l as string)[i]) as string[]
        const newValue = executeFunction(fn, fnParams, meta, contextStack)
        assertChar(newValue, meta)
        result += newValue
      }
      return result
    } else {
      const result: Arr = []
      for (let i = 0; i < length; i += 1) {
        const fnParams = params.slice(1).map(l => toAny((l as Arr)[i]))
        result.push(executeFunction(fn, fnParams, meta, contextStack))
      }
      return result
    }
  }
}

export const sequenceNormalExpression: BuiltinNormalExpressions = {
  cons: {
    evaluate: ([elem, seq], meta): Any => {
      any.assert(elem, meta)
      sequence.assert(seq, meta)
      if (Array.isArray(seq)) {
        return [elem, ...seq]
      }
      assertChar(elem, meta)
      return `${elem}${seq}`
    },
    validate: node => assertLength(2, node),
  },
  nth: {
    evaluate: ([seq, i], meta): Any => {
      sequence.assert(seq, meta)
      number.assert(i, meta, { integer: true })

      return toAny(seq[i])
    },
    validate: node => assertLength(2, node),
  },
  filter: {
    evaluate: ([fn, seq]: Arr, meta, contextStack, { executeFunction }): Seq => {
      litsFunction.assert(fn, meta)
      sequence.assert(seq, meta)
      if (Array.isArray(seq)) {
        return seq.filter(elem => executeFunction(fn, [elem], meta, contextStack))
      }
      return seq
        .split(``)
        .filter(elem => executeFunction(fn, [elem], meta, contextStack))
        .join(``)
    },
    validate: node => assertLength(2, node),
  },
  first: {
    evaluate: ([array], meta): Any => {
      sequence.assert(array, meta)
      return toAny(array[0])
    },
    validate: node => assertLength(1, node),
  },
  last: {
    evaluate: ([first], meta): Any => {
      sequence.assert(first, meta)
      return toAny(first[first.length - 1])
    },
    validate: node => assertLength(1, node),
  },
  map: {
    evaluate: evaluateMap,
    validate: node => assertLength({ min: 2 }, node),
  },
  pop: {
    evaluate: ([seq], meta): Seq => {
      sequence.assert(seq, meta)
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
    evaluate: ([fn, seq]: Arr, meta, contextStack, { executeFunction }): number | null => {
      litsFunction.assert(fn, meta)
      sequence.assert(seq, meta)
      if (isString(seq)) {
        const index = seq.split(``).findIndex(elem => executeFunction(fn, [elem], meta, contextStack))
        return index !== -1 ? index : null
      } else {
        const index = seq.findIndex(elem => executeFunction(fn, [elem], meta, contextStack))
        return index !== -1 ? index : null
      }
    },
    validate: node => assertLength(2, node),
  },
  'index-of': {
    evaluate: ([seq, value], meta): number | null => {
      any.assert(value, meta)
      sequence.assert(seq, meta)
      if (isString(seq)) {
        assertString(value, meta)
        const index = seq.indexOf(value)
        return index !== -1 ? index : null
      } else {
        const index = seq.indexOf(value)
        return index !== -1 ? index : null
      }
    },
    validate: node => assertLength(2, node),
  },
  push: {
    evaluate: ([seq, ...values], meta): Seq => {
      sequence.assert(seq, meta)
      if (isString(seq)) {
        assertCharArray(values, meta)
        return [seq, ...values].join(``)
      } else {
        return [...seq, ...values]
      }
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  reduce: {
    evaluate: (params: Arr, meta, contextStack, { executeFunction }): Any => {
      const fn = params[0]
      litsFunction.assert(fn, meta)

      if (params.length === 2) {
        const [, arr] = params
        sequence.assert(arr, meta)
        if (arr.length === 0) {
          return executeFunction(fn, [], meta, contextStack)
        } else if (arr.length === 1) {
          return toAny(arr[0])
        }
        if (isString(arr)) {
          const chars = arr.split(``)
          return chars.slice(1).reduce((result: Any, elem) => {
            const val = executeFunction(fn, [result, elem], meta, contextStack)
            return val
          }, any.as(chars[0], meta))
        } else {
          return arr.slice(1).reduce((result: Any, elem) => {
            return executeFunction(fn, [result, elem], meta, contextStack)
          }, toAny(arr[0]))
        }
      } else {
        const [, val, seq] = params
        any.assert(val, meta)
        sequence.assert(seq, meta)
        if (isString(seq)) {
          assertString(val, meta)
          if (seq.length === 0) {
            return val
          }
          return seq.split(``).reduce((result: Any, elem) => {
            const newVal = executeFunction(fn, [result, elem], meta, contextStack)
            return newVal
          }, val)
        } else {
          if (seq.length === 0) {
            return val
          }
          return seq.reduce((result: Any, elem) => {
            return executeFunction(fn, [result, elem], meta, contextStack)
          }, val)
        }
      }
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  'reduce-right': {
    evaluate: (params: Arr, meta, contextStack, { executeFunction }): Any => {
      const fn = params[0]
      litsFunction.assert(fn, meta)

      if (params.length === 2) {
        const [, seq] = params
        sequence.assert(seq, meta)
        if (seq.length === 0) {
          return executeFunction(fn, [], meta, contextStack)
        } else if (seq.length === 1) {
          return toAny(seq[0])
        }
        if (isString(seq)) {
          const chars = seq.split(``)
          return chars.slice(0, chars.length - 1).reduceRight((result, elem) => {
            const newVal = executeFunction(fn, [result, elem], meta, contextStack)
            assertString(newVal, meta)
            return newVal
          }, chars[chars.length - 1] as string)
        } else {
          return seq.slice(0, seq.length - 1).reduceRight((result: Any, elem) => {
            return executeFunction(fn, [result, elem], meta, contextStack)
          }, any.as(seq[seq.length - 1], meta))
        }
      } else {
        const [, val, seq] = params
        any.assert(val, meta)
        sequence.assert(seq, meta)
        if (isString(seq)) {
          if (seq.length === 0) {
            return val
          }
          return seq.split(``).reduceRight((result: Any, elem) => {
            const newVal = executeFunction(fn, [result, elem], meta, contextStack)
            return newVal
          }, val)
        } else {
          if (seq.length === 0) {
            return val
          }
          return seq.reduceRight((result: Any, elem) => {
            return executeFunction(fn, [result, elem], meta, contextStack)
          }, val)
        }
      }
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  rest: {
    evaluate: ([first], meta): Arr | string => {
      sequence.assert(first, meta)
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
    evaluate: ([seq, count], meta): Arr | string => {
      sequence.assert(seq, meta)
      assertFiniteNumber(count, meta)
      const integerCount = Math.max(Math.ceil(count), 0)
      if (Array.isArray(seq)) {
        return seq.slice(integerCount)
      }
      return seq.substr(integerCount)
    },
    validate: node => assertLength(2, node),
  },
  next: {
    evaluate: ([first], meta): Arr | string | null => {
      sequence.assert(first, meta)
      if (Array.isArray(first)) {
        if (first.length <= 1) {
          return null
        }

        return first.slice(1)
      }
      if (first.length <= 1) {
        return null
      }
      return first.substr(1)
    },
    validate: node => assertLength(1, node),
  },
  nthnext: {
    evaluate: ([seq, count], meta): Arr | string | null => {
      sequence.assert(seq, meta)
      assertFiniteNumber(count, meta)
      const integerCount = Math.max(Math.ceil(count), 0)
      if (seq.length <= count) {
        return null
      }
      if (Array.isArray(seq)) {
        return seq.slice(integerCount)
      }
      return seq.substr(integerCount)
    },
    validate: node => assertLength(2, node),
  },
  reverse: {
    evaluate: ([first], meta): Any => {
      sequence.assert(first, meta)
      if (Array.isArray(first)) {
        return [...first].reverse()
      }
      return first.split(``).reverse().join(``)
    },
    validate: node => assertLength(1, node),
  },
  second: {
    evaluate: ([array], meta): Any => {
      sequence.assert(array, meta)
      return toAny(array[1])
    },
    validate: node => assertLength(1, node),
  },
  shift: {
    evaluate: ([seq], meta): Any => {
      sequence.assert(seq, meta)
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
    evaluate: (params, meta): Any => {
      const [seq, from, to] = params
      sequence.assert(seq, meta)

      if (params.length === 1) {
        return seq
      }

      number.assert(from, meta, { integer: true })

      if (params.length === 2) {
        return seq.slice(from)
      }

      number.assert(to, meta, { integer: true })
      return seq.slice(from, to)
    },
    validate: node => assertLength({ min: 1, max: 3 }, node),
  },
  some: {
    evaluate: ([fn, seq]: Arr, meta, contextStack, { executeFunction }): Any => {
      litsFunction.assert(fn, meta)
      sequence.assert(seq, meta)

      if (seq.length === 0) {
        return null
      }

      if (isString(seq)) {
        return seq.split(``).find(elem => executeFunction(fn, [elem], meta, contextStack)) ?? null
      }

      return toAny(seq.find(elem => executeFunction(fn, [elem], meta, contextStack)))
    },
    validate: node => assertLength(2, node),
  },
  sort: {
    evaluate: (params: Arr, meta, contextStack, { executeFunction }): Seq => {
      const defaultComparer = params.length === 1
      const seq = defaultComparer ? params[0] : params[1]
      const comparer = defaultComparer ? null : params[0]
      sequence.assert(seq, meta)

      if (isString(seq)) {
        const result = seq.split(``)
        if (defaultComparer) {
          result.sort(compare)
        } else {
          litsFunction.assert(comparer, meta)
          result.sort((a, b) => {
            const compareValue = executeFunction(comparer, [a, b], meta, contextStack)
            assertFiniteNumber(compareValue, meta)
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
          litsFunction.assert(comparer, meta)
          const compareValue = executeFunction(comparer, [a, b], meta, contextStack)
          assertFiniteNumber(compareValue, meta)
          return compareValue
        })
      }
      return result
    },
    validate: node => assertLength({ min: 1, max: 2 }, node),
  },
  'sort-by': {
    evaluate: (params: Arr, meta, contextStack, { executeFunction }): Seq => {
      const defaultComparer = params.length === 2

      const keyfn = any.as(params[0], meta)
      const comparer = defaultComparer ? null : params[1]
      const seq = sequence.as(defaultComparer ? params[1] : params[2], meta)

      if (isString(seq)) {
        const result = seq.split(``)
        if (defaultComparer) {
          result.sort((a, b) => {
            const aKey = executeFunction(keyfn, [a], meta, contextStack)
            const bKey = executeFunction(keyfn, [b], meta, contextStack)
            return compare(aKey, bKey)
          })
        } else {
          litsFunction.assert(comparer, meta)
          result.sort((a, b) => {
            const aKey = executeFunction(keyfn, [a], meta, contextStack)
            const bKey = executeFunction(keyfn, [b], meta, contextStack)
            const compareValue = executeFunction(comparer, [aKey, bKey], meta, contextStack)
            assertFiniteNumber(compareValue, meta)
            return compareValue
          })
        }
        return result.join(``)
      }

      const result = [...seq]
      if (defaultComparer) {
        result.sort((a, b) => {
          const aKey = executeFunction(keyfn, [a], meta, contextStack)
          const bKey = executeFunction(keyfn, [b], meta, contextStack)
          return compare(aKey, bKey)
        })
      } else {
        litsFunction.assert(comparer, meta)
        result.sort((a, b) => {
          const aKey = executeFunction(keyfn, [a], meta, contextStack)
          const bKey = executeFunction(keyfn, [b], meta, contextStack)
          const compareValue = executeFunction(comparer, [aKey, bKey], meta, contextStack)
          assertFiniteNumber(compareValue, meta)
          return compareValue
        })
      }
      return result
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  take: {
    evaluate: ([n, input], meta): Seq => {
      number.assert(n, meta)
      sequence.assert(input, meta)
      const num = Math.max(Math.ceil(n), 0)
      return input.slice(0, num)
    },
    validate: node => assertLength(2, node),
  },
  'take-last': {
    evaluate: ([n, array], meta): Seq => {
      sequence.assert(array, meta)
      number.assert(n, meta)
      const num = Math.max(Math.ceil(n), 0)
      const from = array.length - num
      return array.slice(from)
    },
    validate: node => assertLength(2, node),
  },
  'take-while': {
    evaluate: ([fn, seq]: Arr, meta, contextStack, { executeFunction }): Any => {
      sequence.assert(seq, meta)
      litsFunction.assert(fn, meta)

      const result: Arr = []
      for (const item of seq) {
        if (executeFunction(fn, [item], meta, contextStack)) {
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
    evaluate: ([n, input], meta): Seq => {
      number.assert(n, meta)
      const num = Math.max(Math.ceil(n), 0)
      sequence.assert(input, meta)
      return input.slice(num)
    },
    validate: node => assertLength(2, node),
  },
  'drop-last': {
    evaluate: ([n, array], meta): Seq => {
      sequence.assert(array, meta)
      number.assert(n, meta)
      const num = Math.max(Math.ceil(n), 0)

      const from = array.length - num
      return array.slice(0, from)
    },
    validate: node => assertLength(2, node),
  },
  'drop-while': {
    evaluate: ([fn, seq]: Arr, meta, contextStack, { executeFunction }): Any => {
      sequence.assert(seq, meta)
      litsFunction.assert(fn, meta)

      if (Array.isArray(seq)) {
        const from = seq.findIndex(elem => !executeFunction(fn, [elem], meta, contextStack))
        return seq.slice(from)
      }
      const charArray = seq.split(``)
      const from = charArray.findIndex(elem => !executeFunction(fn, [elem], meta, contextStack))
      return charArray.slice(from).join(``)
    },
    validate: node => assertLength(2, node),
  },
  unshift: {
    evaluate: ([seq, ...values], meta): Seq => {
      sequence.assert(seq, meta)
      if (isString(seq)) {
        assertCharArray(values, meta)
        return [...values, seq].join(``)
      }
      const copy = [...seq]
      copy.unshift(...values)
      return copy
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'random-sample!': {
    evaluate: ([prob, seq], meta): Seq => {
      assertFiniteNumber(prob, meta)
      sequence.assert(seq, meta)

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
  'rand-nth!': {
    evaluate: ([seq], meta): Any => {
      sequence.assert(seq, meta)
      if (seq.length === 0) {
        return null
      }

      const index = Math.floor(Math.random() * seq.length)

      if (isString(seq)) {
        return toAny(seq.split(``)[index])
      }
      return toAny(seq[index])
    },
    validate: node => assertLength(1, node),
  },
  shuffle: {
    evaluate: ([input], meta): Seq => {
      sequence.assert(input, meta)
      const array: Arr = isString(input) ? [...input.split(``)] : [...input]
      let remainingLength = array.length
      let arrayElement: Any
      let pickedIndex: number

      // Fisher–Yates Shuffle
      while (remainingLength) {
        remainingLength -= 1

        // Pick a remaining element
        pickedIndex = Math.floor(Math.random() * remainingLength)

        // And swap it with the current element.
        arrayElement = toAny(array[remainingLength])
        array[remainingLength] = toAny(array[pickedIndex])
        array[pickedIndex] = arrayElement
      }

      return isString(input) ? array.join(``) : array
    },
    validate: node => assertLength(1, node),
  },
  distinct: {
    evaluate: ([input], meta): Seq => {
      sequence.assert(input, meta)
      if (Array.isArray(input)) {
        return Array.from(new Set(input))
      }
      return Array.from(new Set(input.split(``))).join(``)
    },
    validate: node => assertLength(1, node),
  },
  remove: {
    evaluate: ([fn, input], meta, contextStack, { executeFunction }): Seq => {
      litsFunction.assert(fn, meta)
      sequence.assert(input, meta)
      if (Array.isArray(input)) {
        return input.filter(elem => !executeFunction(fn, [elem], meta, contextStack))
      }
      return input
        .split(``)
        .filter(elem => !executeFunction(fn, [elem], meta, contextStack))
        .join(``)
    },
    validate: node => assertLength(2, node),
  },
  'remove-at': {
    evaluate: ([index, input], meta): Seq => {
      number.assert(index, meta)
      sequence.assert(input, meta)

      const intIndex = Math.ceil(index)
      if (intIndex < 0 || intIndex >= input.length) {
        return input
      }

      if (Array.isArray(input)) {
        const copy = [...input]
        copy.splice(index, 1)
        return copy
      }
      return `${input.substring(0, index)}${input.substring(index + 1)}`
    },
    validate: node => assertLength(2, node),
  },
  'split-at': {
    evaluate: ([pos, seq], meta): Seq => {
      assertFiniteNumber(pos, meta)
      const intPos = toNonNegativeInteger(pos)
      sequence.assert(seq, meta)
      return [seq.slice(0, intPos), seq.slice(intPos)]
    },
    validate: node => assertLength(2, node),
  },

  'split-with': {
    evaluate: ([fn, seq], meta, contextStack, { executeFunction }): Seq => {
      litsFunction.assert(fn, meta)
      sequence.assert(seq, meta)
      const seqIsArray = Array.isArray(seq)
      const arr = seqIsArray ? seq : seq.split(``)
      const index = arr.findIndex(elem => !executeFunction(fn, [elem], meta, contextStack))
      if (index === -1) {
        return [seq, seqIsArray ? [] : ``]
      }
      return [seq.slice(0, index), seq.slice(index)]
    },
    validate: node => assertLength(2, node),
  },

  frequencies: {
    evaluate: ([seq], meta): Obj => {
      sequence.assert(seq, meta)

      const arr = isString(seq) ? seq.split(``) : seq

      return arr.reduce((result: Obj, val) => {
        assertString(val, meta)
        if (collHasKey(result, val)) {
          result[val] = (result[val] as number) + 1
        } else {
          result[val] = 1
        }
        return result
      }, {})
    },
    validate: node => assertLength(1, node),
  },

  'group-by': {
    evaluate: ([fn, seq], meta, contextStack, { executeFunction }): Obj => {
      any.assert(fn, meta)
      sequence.assert(seq, meta)
      const arr = Array.isArray(seq) ? seq : seq.split(``)

      return arr.reduce((result: Obj, val) => {
        const key = executeFunction(fn, [val], meta, contextStack)
        assertString(key, meta)
        if (!collHasKey(result, key)) {
          result[key] = []
        }
        ;(result[key] as Arr).push(val)
        return result
      }, {})
    },
    validate: node => assertLength(2, node),
  },

  partition: {
    evaluate: (params, meta): Seq => {
      const len = params.length
      const n = toNonNegativeInteger(number.as(params[0], meta))
      const seq =
        len === 2
          ? sequence.as(params[1], meta)
          : len === 3
          ? sequence.as(params[2], meta)
          : sequence.as(params[3], meta)
      const step = len >= 3 ? toNonNegativeInteger(number.as(params[1], meta)) : n
      const pad = len === 4 ? (params[2] === null ? [] : array.as(params[2], meta)) : undefined

      return partition(n, step, seq, pad, meta)
    },
    validate: node => assertLength({ min: 2, max: 4 }, node),
  },

  'partition-all': {
    evaluate: (params, meta): Seq => {
      const len = params.length
      const n = toNonNegativeInteger(number.as(params[0], meta))
      const seq = len === 2 ? sequence.as(params[1], meta) : sequence.as(params[2], meta)
      const step = len >= 3 ? toNonNegativeInteger(number.as(params[1], meta)) : n

      return partition(n, step, seq, [], meta)
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },

  'partition-by': {
    evaluate: ([fn, seq], meta, contextStack, { executeFunction }): Seq => {
      litsFunction.assert(fn, meta)
      sequence.assert(seq, meta)
      const isStringSeq = isString(seq)
      let oldValue: unknown = undefined

      const result = (isStringSeq ? seq.split(``) : seq).reduce((result: Arr, elem) => {
        const value = executeFunction(fn, [elem], meta, contextStack)
        if (value !== oldValue) {
          result.push([])
          oldValue = value
        }
        ;(result[result.length - 1] as Arr).push(elem)
        return result
      }, [])

      return isStringSeq ? result.map(elem => (elem as Arr).join(``)) : result
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
}

function partition(n: number, step: number, seq: Seq, pad: Arr | undefined, meta: TokenMeta) {
  assertPositiveNumber(step, meta)
  const isStringSeq = isString(seq)

  const result: Arr[] = []
  let start = 0
  outer: while (start < seq.length) {
    const innerArr: Arr = []
    for (let i = start; i < start + n; i += 1) {
      if (i >= seq.length) {
        const padIndex = i - seq.length
        if (!pad) {
          start += step
          continue outer
        }
        if (padIndex >= pad.length) {
          break
        }
        innerArr.push(pad[padIndex])
      } else {
        innerArr.push(seq[i])
      }
    }
    result.push(innerArr)
    start += step
  }
  return isStringSeq ? result.map(x => x.join(``)) : result
}
