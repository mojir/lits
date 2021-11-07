import { LitsError } from '../../../errors'
import { Any, Arr, Obj, Seq } from '../../../interface'
import { SourceCodeInfo } from '../../../tokenizer/interface'
import { compare, toAny, toNonNegativeInteger, collHasKey } from '../../../utils'
import {
  any,
  litsFunction,
  number,
  sequence,
  array,
  string,
  assertNumberOfParams,
  charArray,
} from '../../../utils/assertion'
import { BuiltinNormalExpressions, NormalExpressionEvaluator } from '../../interface'

export const evaluateMap: NormalExpressionEvaluator<Arr | string> = (
  params: Arr,
  sourceCodeInfo,
  contextStack,
  { executeFunction },
) => {
  const [fn, firstList] = params
  litsFunction.assert(fn, sourceCodeInfo)
  sequence.assert(firstList, sourceCodeInfo)
  const isStringSeq = string.is(firstList)

  const length = firstList.length
  if (params.length === 2) {
    if (array.is(firstList)) {
      return firstList.map(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
    } else {
      return firstList
        .split(``)
        .map(elem => {
          const newVal = executeFunction(fn, [elem], sourceCodeInfo, contextStack)
          string.assert(newVal, sourceCodeInfo, { char: true })
          return newVal
        })
        .join(``)
    }
  } else {
    params.slice(2).forEach(collParam => {
      if (isStringSeq) {
        string.assert(collParam, sourceCodeInfo)
      } else {
        array.assert(collParam, sourceCodeInfo)
      }
      if (length !== collParam.length) {
        throw new LitsError(`All arguments to "map" must have the same length.`, sourceCodeInfo)
      }
    })

    if (isStringSeq) {
      let result = ``
      for (let i = 0; i < length; i += 1) {
        const fnParams = params.slice(1).map(l => (l as string)[i]) as string[]
        const newValue = executeFunction(fn, fnParams, sourceCodeInfo, contextStack)
        string.assert(newValue, sourceCodeInfo, { char: true })
        result += newValue
      }
      return result
    } else {
      const result: Arr = []
      for (let i = 0; i < length; i += 1) {
        const fnParams = params.slice(1).map(l => toAny((l as Arr)[i]))
        result.push(executeFunction(fn, fnParams, sourceCodeInfo, contextStack))
      }
      return result
    }
  }
}

export const sequenceNormalExpression: BuiltinNormalExpressions = {
  cons: {
    evaluate: ([elem, seq], sourceCodeInfo): Any => {
      any.assert(elem, sourceCodeInfo)
      sequence.assert(seq, sourceCodeInfo)
      if (Array.isArray(seq)) {
        return [elem, ...seq]
      }
      string.assert(elem, sourceCodeInfo, { char: true })
      return `${elem}${seq}`
    },
    validate: node => assertNumberOfParams(2, node),
  },
  nth: {
    evaluate: ([seq, i], sourceCodeInfo): Any => {
      sequence.assert(seq, sourceCodeInfo)
      number.assert(i, sourceCodeInfo, { integer: true })

      return toAny(seq[i])
    },
    validate: node => assertNumberOfParams(2, node),
  },
  filter: {
    evaluate: ([fn, seq]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      litsFunction.assert(fn, sourceCodeInfo)
      sequence.assert(seq, sourceCodeInfo)
      if (Array.isArray(seq)) {
        return seq.filter(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
      }
      return seq
        .split(``)
        .filter(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
        .join(``)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  first: {
    evaluate: ([array], sourceCodeInfo): Any => {
      sequence.assert(array, sourceCodeInfo)
      return toAny(array[0])
    },
    validate: node => assertNumberOfParams(1, node),
  },
  last: {
    evaluate: ([first], sourceCodeInfo): Any => {
      sequence.assert(first, sourceCodeInfo)
      return toAny(first[first.length - 1])
    },
    validate: node => assertNumberOfParams(1, node),
  },
  map: {
    evaluate: evaluateMap,
    validate: node => assertNumberOfParams({ min: 2 }, node),
  },
  pop: {
    evaluate: ([seq], sourceCodeInfo): Seq => {
      sequence.assert(seq, sourceCodeInfo)
      if (string.is(seq)) {
        return seq.substr(0, seq.length - 1)
      }
      const copy = [...seq]
      copy.pop()
      return copy
    },
    validate: node => assertNumberOfParams(1, node),
  },
  position: {
    evaluate: ([fn, seq]: Arr, sourceCodeInfo, contextStack, { executeFunction }): number | null => {
      litsFunction.assert(fn, sourceCodeInfo)
      sequence.assert(seq, sourceCodeInfo)
      if (string.is(seq)) {
        const index = seq.split(``).findIndex(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
        return index !== -1 ? index : null
      } else {
        const index = seq.findIndex(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
        return index !== -1 ? index : null
      }
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'index-of': {
    evaluate: ([seq, value], sourceCodeInfo): number | null => {
      any.assert(value, sourceCodeInfo)
      sequence.assert(seq, sourceCodeInfo)
      if (string.is(seq)) {
        string.assert(value, sourceCodeInfo)
        const index = seq.indexOf(value)
        return index !== -1 ? index : null
      } else {
        const index = seq.indexOf(value)
        return index !== -1 ? index : null
      }
    },
    validate: node => assertNumberOfParams(2, node),
  },
  push: {
    evaluate: ([seq, ...values], sourceCodeInfo): Seq => {
      sequence.assert(seq, sourceCodeInfo)
      if (string.is(seq)) {
        charArray.assert(values, sourceCodeInfo)
        return [seq, ...values].join(``)
      } else {
        return [...seq, ...values]
      }
    },
    validate: node => assertNumberOfParams({ min: 2 }, node),
  },
  reductions: {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any[] => {
      const fn = params[0]
      litsFunction.assert(fn, sourceCodeInfo)

      if (params.length === 2) {
        const [, arr] = params
        sequence.assert(arr, sourceCodeInfo)
        if (arr.length === 0) {
          return [executeFunction(fn, [], sourceCodeInfo, contextStack)]
        } else if (arr.length === 1) {
          return [toAny(arr[0])]
        }
        if (string.is(arr)) {
          const chars = arr.split(``)
          const resultArray: Any[] = [any.as(chars[0], sourceCodeInfo)]
          chars.slice(1).reduce((result: Any, elem) => {
            const newVal = executeFunction(fn, [result, elem], sourceCodeInfo, contextStack)
            resultArray.push(newVal)
            return newVal
          }, any.as(chars[0], sourceCodeInfo))
          return resultArray
        } else {
          const resultArray: Any[] = [toAny(arr[0])]
          arr.slice(1).reduce((result: Any, elem) => {
            const newVal = executeFunction(fn, [result, elem], sourceCodeInfo, contextStack)
            resultArray.push(newVal)
            return newVal
          }, toAny(arr[0]))
          return resultArray
        }
      } else {
        const [, val, seq] = params
        any.assert(val, sourceCodeInfo)
        sequence.assert(seq, sourceCodeInfo)
        if (string.is(seq)) {
          string.assert(val, sourceCodeInfo)
          if (seq.length === 0) {
            return [val]
          }
          const resultArray: Any[] = [val]
          seq.split(``).reduce((result: Any, elem) => {
            const newVal = executeFunction(fn, [result, elem], sourceCodeInfo, contextStack)
            resultArray.push(newVal)
            return newVal
          }, val)
          return resultArray
        } else {
          if (seq.length === 0) {
            return [val]
          }
          const resultArray: Any[] = [val]
          seq.reduce((result: Any, elem) => {
            const newVal = executeFunction(fn, [result, elem], sourceCodeInfo, contextStack)
            resultArray.push(newVal)
            return newVal
          }, val)
          return resultArray
        }
      }
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  reduce: {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      const fn = params[0]
      litsFunction.assert(fn, sourceCodeInfo)

      if (params.length === 2) {
        const [, arr] = params
        sequence.assert(arr, sourceCodeInfo)
        if (arr.length === 0) {
          return executeFunction(fn, [], sourceCodeInfo, contextStack)
        } else if (arr.length === 1) {
          return toAny(arr[0])
        }
        if (string.is(arr)) {
          const chars = arr.split(``)
          return chars.slice(1).reduce((result: Any, elem) => {
            const val = executeFunction(fn, [result, elem], sourceCodeInfo, contextStack)
            return val
          }, any.as(chars[0], sourceCodeInfo))
        } else {
          return arr.slice(1).reduce((result: Any, elem) => {
            return executeFunction(fn, [result, elem], sourceCodeInfo, contextStack)
          }, toAny(arr[0]))
        }
      } else {
        const [, val, seq] = params
        any.assert(val, sourceCodeInfo)
        sequence.assert(seq, sourceCodeInfo)
        if (string.is(seq)) {
          string.assert(val, sourceCodeInfo)
          if (seq.length === 0) {
            return val
          }
          return seq.split(``).reduce((result: Any, elem) => {
            const newVal = executeFunction(fn, [result, elem], sourceCodeInfo, contextStack)
            return newVal
          }, val)
        } else {
          if (seq.length === 0) {
            return val
          }
          return seq.reduce((result: Any, elem) => {
            return executeFunction(fn, [result, elem], sourceCodeInfo, contextStack)
          }, val)
        }
      }
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'reduce-right': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      const fn = params[0]
      litsFunction.assert(fn, sourceCodeInfo)

      if (params.length === 2) {
        const [, seq] = params
        sequence.assert(seq, sourceCodeInfo)
        if (seq.length === 0) {
          return executeFunction(fn, [], sourceCodeInfo, contextStack)
        } else if (seq.length === 1) {
          return toAny(seq[0])
        }
        if (string.is(seq)) {
          const chars = seq.split(``)
          return chars.slice(0, chars.length - 1).reduceRight((result, elem) => {
            const newVal = executeFunction(fn, [result, elem], sourceCodeInfo, contextStack)
            string.assert(newVal, sourceCodeInfo)
            return newVal
          }, chars[chars.length - 1] as string)
        } else {
          return seq.slice(0, seq.length - 1).reduceRight((result: Any, elem) => {
            return executeFunction(fn, [result, elem], sourceCodeInfo, contextStack)
          }, any.as(seq[seq.length - 1], sourceCodeInfo))
        }
      } else {
        const [, val, seq] = params
        any.assert(val, sourceCodeInfo)
        sequence.assert(seq, sourceCodeInfo)
        if (string.is(seq)) {
          if (seq.length === 0) {
            return val
          }
          return seq.split(``).reduceRight((result: Any, elem) => {
            const newVal = executeFunction(fn, [result, elem], sourceCodeInfo, contextStack)
            return newVal
          }, val)
        } else {
          if (seq.length === 0) {
            return val
          }
          return seq.reduceRight((result: Any, elem) => {
            return executeFunction(fn, [result, elem], sourceCodeInfo, contextStack)
          }, val)
        }
      }
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  rest: {
    evaluate: ([first], sourceCodeInfo): Arr | string => {
      sequence.assert(first, sourceCodeInfo)
      if (Array.isArray(first)) {
        if (first.length <= 1) {
          return []
        }

        return first.slice(1)
      }
      return first.substr(1)
    },
    validate: node => assertNumberOfParams(1, node),
  },
  nthrest: {
    evaluate: ([seq, count], sourceCodeInfo): Arr | string => {
      sequence.assert(seq, sourceCodeInfo)
      number.assert(count, sourceCodeInfo, { finite: true })
      const integerCount = Math.max(Math.ceil(count), 0)
      if (Array.isArray(seq)) {
        return seq.slice(integerCount)
      }
      return seq.substr(integerCount)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  next: {
    evaluate: ([first], sourceCodeInfo): Arr | string | null => {
      sequence.assert(first, sourceCodeInfo)
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
    validate: node => assertNumberOfParams(1, node),
  },
  nthnext: {
    evaluate: ([seq, count], sourceCodeInfo): Arr | string | null => {
      sequence.assert(seq, sourceCodeInfo)
      number.assert(count, sourceCodeInfo, { finite: true })
      const integerCount = Math.max(Math.ceil(count), 0)
      if (seq.length <= count) {
        return null
      }
      if (Array.isArray(seq)) {
        return seq.slice(integerCount)
      }
      return seq.substr(integerCount)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  reverse: {
    evaluate: ([first], sourceCodeInfo): Any => {
      sequence.assert(first, sourceCodeInfo)
      if (Array.isArray(first)) {
        return [...first].reverse()
      }
      return first.split(``).reverse().join(``)
    },
    validate: node => assertNumberOfParams(1, node),
  },
  second: {
    evaluate: ([array], sourceCodeInfo): Any => {
      sequence.assert(array, sourceCodeInfo)
      return toAny(array[1])
    },
    validate: node => assertNumberOfParams(1, node),
  },
  shift: {
    evaluate: ([seq], sourceCodeInfo): Any => {
      sequence.assert(seq, sourceCodeInfo)
      if (string.is(seq)) {
        return seq.substr(1)
      }
      const copy = [...seq]
      copy.shift()
      return copy
    },
    validate: node => assertNumberOfParams(1, node),
  },
  slice: {
    evaluate: (params, sourceCodeInfo): Any => {
      const [seq, from, to] = params
      sequence.assert(seq, sourceCodeInfo)

      if (params.length === 1) {
        return seq
      }

      number.assert(from, sourceCodeInfo, { integer: true })

      if (params.length === 2) {
        return seq.slice(from)
      }

      number.assert(to, sourceCodeInfo, { integer: true })
      return seq.slice(from, to)
    },
    validate: node => assertNumberOfParams({ min: 1, max: 3 }, node),
  },
  some: {
    evaluate: ([fn, seq]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      litsFunction.assert(fn, sourceCodeInfo)
      sequence.assert(seq, sourceCodeInfo)

      if (seq.length === 0) {
        return null
      }

      if (string.is(seq)) {
        return seq.split(``).find(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack)) ?? null
      }

      return toAny(seq.find(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack)))
    },
    validate: node => assertNumberOfParams(2, node),
  },
  sort: {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      const defaultComparer = params.length === 1
      const seq = defaultComparer ? params[0] : params[1]
      const comparer = defaultComparer ? null : params[0]
      sequence.assert(seq, sourceCodeInfo)

      if (string.is(seq)) {
        const result = seq.split(``)
        if (defaultComparer) {
          result.sort(compare)
        } else {
          litsFunction.assert(comparer, sourceCodeInfo)
          result.sort((a, b) => {
            const compareValue = executeFunction(comparer, [a, b], sourceCodeInfo, contextStack)
            number.assert(compareValue, sourceCodeInfo, { finite: true })
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
          litsFunction.assert(comparer, sourceCodeInfo)
          const compareValue = executeFunction(comparer, [a, b], sourceCodeInfo, contextStack)
          number.assert(compareValue, sourceCodeInfo, { finite: true })
          return compareValue
        })
      }
      return result
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
  'sort-by': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      const defaultComparer = params.length === 2

      const keyfn = any.as(params[0], sourceCodeInfo)
      const comparer = defaultComparer ? null : params[1]
      const seq = sequence.as(defaultComparer ? params[1] : params[2], sourceCodeInfo)

      if (string.is(seq)) {
        const result = seq.split(``)
        if (defaultComparer) {
          result.sort((a, b) => {
            const aKey = executeFunction(keyfn, [a], sourceCodeInfo, contextStack)
            const bKey = executeFunction(keyfn, [b], sourceCodeInfo, contextStack)
            return compare(aKey, bKey)
          })
        } else {
          litsFunction.assert(comparer, sourceCodeInfo)
          result.sort((a, b) => {
            const aKey = executeFunction(keyfn, [a], sourceCodeInfo, contextStack)
            const bKey = executeFunction(keyfn, [b], sourceCodeInfo, contextStack)
            const compareValue = executeFunction(comparer, [aKey, bKey], sourceCodeInfo, contextStack)
            number.assert(compareValue, sourceCodeInfo, { finite: true })
            return compareValue
          })
        }
        return result.join(``)
      }

      const result = [...seq]
      if (defaultComparer) {
        result.sort((a, b) => {
          const aKey = executeFunction(keyfn, [a], sourceCodeInfo, contextStack)
          const bKey = executeFunction(keyfn, [b], sourceCodeInfo, contextStack)
          return compare(aKey, bKey)
        })
      } else {
        litsFunction.assert(comparer, sourceCodeInfo)
        result.sort((a, b) => {
          const aKey = executeFunction(keyfn, [a], sourceCodeInfo, contextStack)
          const bKey = executeFunction(keyfn, [b], sourceCodeInfo, contextStack)
          const compareValue = executeFunction(comparer, [aKey, bKey], sourceCodeInfo, contextStack)
          number.assert(compareValue, sourceCodeInfo, { finite: true })
          return compareValue
        })
      }
      return result
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  take: {
    evaluate: ([n, input], sourceCodeInfo): Seq => {
      number.assert(n, sourceCodeInfo)
      sequence.assert(input, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)
      return input.slice(0, num)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'take-last': {
    evaluate: ([n, array], sourceCodeInfo): Seq => {
      sequence.assert(array, sourceCodeInfo)
      number.assert(n, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)
      const from = array.length - num
      return array.slice(from)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'take-while': {
    evaluate: ([fn, seq]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      sequence.assert(seq, sourceCodeInfo)
      litsFunction.assert(fn, sourceCodeInfo)

      const result: Arr = []
      for (const item of seq) {
        if (executeFunction(fn, [item], sourceCodeInfo, contextStack)) {
          result.push(item)
        } else {
          break
        }
      }
      return string.is(seq) ? result.join(``) : result
    },
    validate: node => assertNumberOfParams(2, node),
  },
  drop: {
    evaluate: ([n, input], sourceCodeInfo): Seq => {
      number.assert(n, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)
      sequence.assert(input, sourceCodeInfo)
      return input.slice(num)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'drop-last': {
    evaluate: ([n, array], sourceCodeInfo): Seq => {
      sequence.assert(array, sourceCodeInfo)
      number.assert(n, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)

      const from = array.length - num
      return array.slice(0, from)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'drop-while': {
    evaluate: ([fn, seq]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      sequence.assert(seq, sourceCodeInfo)
      litsFunction.assert(fn, sourceCodeInfo)

      if (Array.isArray(seq)) {
        const from = seq.findIndex(elem => !executeFunction(fn, [elem], sourceCodeInfo, contextStack))
        return seq.slice(from)
      }
      const charArray = seq.split(``)
      const from = charArray.findIndex(elem => !executeFunction(fn, [elem], sourceCodeInfo, contextStack))
      return charArray.slice(from).join(``)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  unshift: {
    evaluate: ([seq, ...values], sourceCodeInfo): Seq => {
      sequence.assert(seq, sourceCodeInfo)
      if (string.is(seq)) {
        charArray.assert(values, sourceCodeInfo)
        return [...values, seq].join(``)
      }
      const copy = [...seq]
      copy.unshift(...values)
      return copy
    },
    validate: node => assertNumberOfParams({ min: 2 }, node),
  },
  'random-sample!': {
    evaluate: ([prob, seq], sourceCodeInfo): Seq => {
      number.assert(prob, sourceCodeInfo, { finite: true })
      sequence.assert(seq, sourceCodeInfo)

      if (string.is(seq)) {
        return seq
          .split(``)
          .filter(() => Math.random() < prob)
          .join(``)
      } else {
        return seq.filter(() => Math.random() < prob)
      }
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'rand-nth!': {
    evaluate: ([seq], sourceCodeInfo): Any => {
      sequence.assert(seq, sourceCodeInfo)
      if (seq.length === 0) {
        return null
      }

      const index = Math.floor(Math.random() * seq.length)

      if (string.is(seq)) {
        return toAny(seq.split(``)[index])
      }
      return toAny(seq[index])
    },
    validate: node => assertNumberOfParams(1, node),
  },
  shuffle: {
    evaluate: ([input], sourceCodeInfo): Seq => {
      sequence.assert(input, sourceCodeInfo)
      const array: Arr = string.is(input) ? [...input.split(``)] : [...input]
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

      return string.is(input) ? array.join(``) : array
    },
    validate: node => assertNumberOfParams(1, node),
  },
  distinct: {
    evaluate: ([input], sourceCodeInfo): Seq => {
      sequence.assert(input, sourceCodeInfo)
      if (Array.isArray(input)) {
        return Array.from(new Set(input))
      }
      return Array.from(new Set(input.split(``))).join(``)
    },
    validate: node => assertNumberOfParams(1, node),
  },
  remove: {
    evaluate: ([fn, input], sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      litsFunction.assert(fn, sourceCodeInfo)
      sequence.assert(input, sourceCodeInfo)
      if (Array.isArray(input)) {
        return input.filter(elem => !executeFunction(fn, [elem], sourceCodeInfo, contextStack))
      }
      return input
        .split(``)
        .filter(elem => !executeFunction(fn, [elem], sourceCodeInfo, contextStack))
        .join(``)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'remove-at': {
    evaluate: ([index, input], sourceCodeInfo): Seq => {
      number.assert(index, sourceCodeInfo)
      sequence.assert(input, sourceCodeInfo)

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
    validate: node => assertNumberOfParams(2, node),
  },
  'split-at': {
    evaluate: ([pos, seq], sourceCodeInfo): Seq => {
      number.assert(pos, sourceCodeInfo, { finite: true })
      const intPos = toNonNegativeInteger(pos)
      sequence.assert(seq, sourceCodeInfo)
      return [seq.slice(0, intPos), seq.slice(intPos)]
    },
    validate: node => assertNumberOfParams(2, node),
  },

  'split-with': {
    evaluate: ([fn, seq], sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      litsFunction.assert(fn, sourceCodeInfo)
      sequence.assert(seq, sourceCodeInfo)
      const seqIsArray = Array.isArray(seq)
      const arr = seqIsArray ? seq : seq.split(``)
      const index = arr.findIndex(elem => !executeFunction(fn, [elem], sourceCodeInfo, contextStack))
      if (index === -1) {
        return [seq, seqIsArray ? [] : ``]
      }
      return [seq.slice(0, index), seq.slice(index)]
    },
    validate: node => assertNumberOfParams(2, node),
  },

  frequencies: {
    evaluate: ([seq], sourceCodeInfo): Obj => {
      sequence.assert(seq, sourceCodeInfo)

      const arr = string.is(seq) ? seq.split(``) : seq

      return arr.reduce((result: Obj, val) => {
        string.assert(val, sourceCodeInfo)
        if (collHasKey(result, val)) {
          result[val] = (result[val] as number) + 1
        } else {
          result[val] = 1
        }
        return result
      }, {})
    },
    validate: node => assertNumberOfParams(1, node),
  },

  'group-by': {
    evaluate: ([fn, seq], sourceCodeInfo, contextStack, { executeFunction }): Obj => {
      any.assert(fn, sourceCodeInfo)
      sequence.assert(seq, sourceCodeInfo)
      const arr = Array.isArray(seq) ? seq : seq.split(``)

      return arr.reduce((result: Obj, val) => {
        const key = executeFunction(fn, [val], sourceCodeInfo, contextStack)
        string.assert(key, sourceCodeInfo)
        if (!collHasKey(result, key)) {
          result[key] = []
        }
        ;(result[key] as Arr).push(val)
        return result
      }, {})
    },
    validate: node => assertNumberOfParams(2, node),
  },

  partition: {
    evaluate: (params, sourceCodeInfo): Seq => {
      const len = params.length
      const n = toNonNegativeInteger(number.as(params[0], sourceCodeInfo))
      const seq =
        len === 2
          ? sequence.as(params[1], sourceCodeInfo)
          : len === 3
          ? sequence.as(params[2], sourceCodeInfo)
          : sequence.as(params[3], sourceCodeInfo)
      const step = len >= 3 ? toNonNegativeInteger(number.as(params[1], sourceCodeInfo)) : n
      const pad = len === 4 ? (params[2] === null ? [] : array.as(params[2], sourceCodeInfo)) : undefined

      return partition(n, step, seq, pad, sourceCodeInfo)
    },
    validate: node => assertNumberOfParams({ min: 2, max: 4 }, node),
  },

  'partition-all': {
    evaluate: (params, sourceCodeInfo): Seq => {
      const len = params.length
      const n = toNonNegativeInteger(number.as(params[0], sourceCodeInfo))
      const seq = len === 2 ? sequence.as(params[1], sourceCodeInfo) : sequence.as(params[2], sourceCodeInfo)
      const step = len >= 3 ? toNonNegativeInteger(number.as(params[1], sourceCodeInfo)) : n

      return partition(n, step, seq, [], sourceCodeInfo)
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },

  'partition-by': {
    evaluate: ([fn, seq], sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      litsFunction.assert(fn, sourceCodeInfo)
      sequence.assert(seq, sourceCodeInfo)
      const isStringSeq = string.is(seq)
      let oldValue: unknown = undefined

      const result = (isStringSeq ? seq.split(``) : seq).reduce((result: Arr, elem) => {
        const value = executeFunction(fn, [elem], sourceCodeInfo, contextStack)
        if (value !== oldValue) {
          result.push([])
          oldValue = value
        }
        ;(result[result.length - 1] as Arr).push(elem)
        return result
      }, [])

      return isStringSeq ? result.map(elem => (elem as Arr).join(``)) : result
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
}

function partition(n: number, step: number, seq: Seq, pad: Arr | undefined, sourceCodeInfo: SourceCodeInfo) {
  number.assert(step, sourceCodeInfo, { positive: true })
  const isStringSeq = string.is(seq)

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
