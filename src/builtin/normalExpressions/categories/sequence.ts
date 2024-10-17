import { LitsError } from '../../../errors'
import type { Any, Arr, Obj, Seq } from '../../../interface'
import type { SourceCodeInfo } from '../../../tokenizer/interface'
import { collHasKey, compare, toAny, toNonNegativeInteger } from '../../../utils'
import { assertLitsFunction } from '../../../typeGuards/litsFunction'
import type { BuiltinNormalExpressions, NormalExpressionEvaluator } from '../../interface'
import { asArray, assertArray, assertCharArray } from '../../../typeGuards/array'
import { asAny, asSeq, assertAny, assertSeq } from '../../../typeGuards/lits'
import { asNumber, assertNumber } from '../../../typeGuards/number'
import { assertString } from '../../../typeGuards/string'
import { assertNumberOfParams } from '../../../typeGuards'

export const evaluateMap: NormalExpressionEvaluator<Arr | string> = (
  params: Arr,
  sourceCodeInfo,
  contextStack,
  { executeFunction },
) => {
  const [fn, firstList] = params
  assertLitsFunction(fn, sourceCodeInfo)
  assertSeq(firstList, sourceCodeInfo)
  const isStringSeq = typeof firstList === 'string'

  const length = firstList.length
  if (params.length === 2) {
    if (Array.isArray(firstList)) {
      return firstList.map(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
    }
    else {
      return firstList
        .split('')
        .map((elem) => {
          const newVal = executeFunction(fn, [elem], contextStack, sourceCodeInfo)
          assertString(newVal, sourceCodeInfo, { char: true })
          return newVal
        })
        .join('')
    }
  }
  else {
    params.slice(2).forEach((collParam) => {
      if (isStringSeq)
        assertString(collParam, sourceCodeInfo)
      else
        assertArray(collParam, sourceCodeInfo)

      if (length !== collParam.length)
        throw new LitsError('All arguments to "map" must have the same length.', sourceCodeInfo)
    })

    if (isStringSeq) {
      let result = ''
      for (let i = 0; i < length; i += 1) {
        const fnParams = params.slice(1).map(l => (l as string)[i]) as string[]
        const newValue = executeFunction(fn, fnParams, contextStack, sourceCodeInfo)
        assertString(newValue, sourceCodeInfo, { char: true })
        result += newValue
      }
      return result
    }
    else {
      const result: Arr = []
      for (let i = 0; i < length; i += 1) {
        const fnParams = params.slice(1).map(l => toAny((l as Arr)[i]))
        result.push(executeFunction(fn, fnParams, contextStack, sourceCodeInfo))
      }
      return result
    }
  }
}

export const sequenceNormalExpression: BuiltinNormalExpressions = {
  'cons': {
    evaluate: ([elem, seq], sourceCodeInfo): Any => {
      assertAny(elem, sourceCodeInfo)
      assertSeq(seq, sourceCodeInfo)
      if (Array.isArray(seq))
        return [elem, ...seq]

      assertString(elem, sourceCodeInfo, { char: true })
      return `${elem}${seq}`
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'nth': {
    evaluate: (params, sourceCodeInfo): Any => {
      const [seq, i] = params
      const defaultValue = toAny(params[2])

      assertNumber(i, sourceCodeInfo, { integer: true })

      if (seq === null)
        return defaultValue

      assertSeq(seq, sourceCodeInfo)
      return i >= 0 && i < seq.length ? toAny(seq[i]) : defaultValue
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'filter': {
    evaluate: ([fn, seq]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      assertLitsFunction(fn, sourceCodeInfo)
      assertSeq(seq, sourceCodeInfo)
      if (Array.isArray(seq))
        return seq.filter(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      return seq
        .split('')
        .filter(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
        .join('')
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'first': {
    evaluate: ([array], sourceCodeInfo): Any => {
      if (array === null)
        return null

      assertSeq(array, sourceCodeInfo)
      return toAny(array[0])
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'last': {
    evaluate: ([array], sourceCodeInfo): Any => {
      if (array === null)
        return null

      assertSeq(array, sourceCodeInfo)
      return toAny(array[array.length - 1])
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'map': {
    evaluate: evaluateMap,
    validate: node => assertNumberOfParams({ min: 2 }, node),
  },
  'pop': {
    evaluate: ([seq], sourceCodeInfo): Seq => {
      assertSeq(seq, sourceCodeInfo)
      if (typeof seq === 'string')
        return seq.substr(0, seq.length - 1)

      const copy = [...seq]
      copy.pop()
      return copy
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'position': {
    evaluate: ([fn, seq]: Arr, sourceCodeInfo, contextStack, { executeFunction }): number | null => {
      assertLitsFunction(fn, sourceCodeInfo)
      if (seq === null)
        return null

      assertSeq(seq, sourceCodeInfo)
      if (typeof seq === 'string') {
        const index = seq.split('').findIndex(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
        return index !== -1 ? index : null
      }
      else {
        const index = seq.findIndex(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
        return index !== -1 ? index : null
      }
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'index-of': {
    evaluate: ([seq, value], sourceCodeInfo): number | null => {
      assertAny(value, sourceCodeInfo)
      if (seq === null)
        return null

      assertSeq(seq, sourceCodeInfo)
      if (typeof seq === 'string') {
        assertString(value, sourceCodeInfo)
        const index = seq.indexOf(value)
        return index !== -1 ? index : null
      }
      else {
        const index = seq.indexOf(value)
        return index !== -1 ? index : null
      }
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'push': {
    evaluate: ([seq, ...values], sourceCodeInfo): Seq => {
      assertSeq(seq, sourceCodeInfo)
      if (typeof seq === 'string') {
        assertCharArray(values, sourceCodeInfo)
        return [seq, ...values].join('')
      }
      else {
        return [...seq, ...values]
      }
    },
    validate: node => assertNumberOfParams({ min: 2 }, node),
  },
  'reductions': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any[] => {
      const fn = params[0]
      assertLitsFunction(fn, sourceCodeInfo)

      if (params.length === 2) {
        const [, arr] = params
        assertSeq(arr, sourceCodeInfo)
        if (arr.length === 0)
          return [executeFunction(fn, [], contextStack, sourceCodeInfo)]
        else if (arr.length === 1)
          return [toAny(arr[0])]

        if (typeof arr === 'string') {
          const chars = arr.split('')
          const resultArray: Any[] = [asAny(chars[0], sourceCodeInfo)]
          chars.slice(1).reduce(
            (result: Any, elem) => {
              const newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
              resultArray.push(newVal)
              return newVal
            },
            asAny(chars[0], sourceCodeInfo),
          )
          return resultArray
        }
        else {
          const resultArray: Any[] = [toAny(arr[0])]
          arr.slice(1).reduce((result: Any, elem) => {
            const newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
            resultArray.push(newVal)
            return newVal
          }, toAny(arr[0]))
          return resultArray
        }
      }
      else {
        const [, val, seq] = params
        assertAny(val, sourceCodeInfo)
        assertSeq(seq, sourceCodeInfo)
        if (typeof seq === 'string') {
          assertString(val, sourceCodeInfo)
          if (seq.length === 0)
            return [val]

          const resultArray: Any[] = [val]
          seq.split('').reduce((result: Any, elem) => {
            const newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
            resultArray.push(newVal)
            return newVal
          }, val)
          return resultArray
        }
        else {
          if (seq.length === 0)
            return [val]

          const resultArray: Any[] = [val]
          seq.reduce((result: Any, elem) => {
            const newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
            resultArray.push(newVal)
            return newVal
          }, val)
          return resultArray
        }
      }
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'reduce': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      const fn = params[0]
      assertLitsFunction(fn, sourceCodeInfo)

      if (params.length === 2) {
        const [, arr] = params
        assertSeq(arr, sourceCodeInfo)
        if (arr.length === 0)
          return executeFunction(fn, [], contextStack, sourceCodeInfo)
        else if (arr.length === 1)
          return toAny(arr[0])

        if (typeof arr === 'string') {
          const chars = arr.split('')
          return chars.slice(1).reduce(
            (result: Any, elem) => {
              const val = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
              return val
            },
            asAny(chars[0], sourceCodeInfo),
          )
        }
        else {
          return arr.slice(1).reduce((result: Any, elem) => {
            return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
          }, toAny(arr[0]))
        }
      }
      else {
        const [, val, seq] = params
        assertAny(val, sourceCodeInfo)
        assertSeq(seq, sourceCodeInfo)
        if (typeof seq === 'string') {
          assertString(val, sourceCodeInfo)
          if (seq.length === 0)
            return val

          return seq.split('').reduce((result: Any, elem) => {
            const newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
            return newVal
          }, val)
        }
        else {
          if (seq.length === 0)
            return val

          return seq.reduce((result: Any, elem) => {
            return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
          }, val)
        }
      }
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'reduce-right': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      const fn = params[0]
      assertLitsFunction(fn, sourceCodeInfo)

      if (params.length === 2) {
        const [, seq] = params
        assertSeq(seq, sourceCodeInfo)
        if (seq.length === 0)
          return executeFunction(fn, [], contextStack, sourceCodeInfo)
        else if (seq.length === 1)
          return toAny(seq[0])

        if (typeof seq === 'string') {
          const chars = seq.split('')
          return chars.slice(0, chars.length - 1).reduceRight(
            (result, elem) => {
              const newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
              assertString(newVal, sourceCodeInfo)
              return newVal
            },
            chars[chars.length - 1] as string,
          )
        }
        else {
          return seq.slice(0, seq.length - 1).reduceRight(
            (result: Any, elem) => {
              return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
            },
            asAny(seq[seq.length - 1], sourceCodeInfo),
          )
        }
      }
      else {
        const [, val, seq] = params
        assertAny(val, sourceCodeInfo)
        assertSeq(seq, sourceCodeInfo)
        if (typeof seq === 'string') {
          if (seq.length === 0)
            return val

          return seq.split('').reduceRight((result: Any, elem) => {
            const newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
            return newVal
          }, val)
        }
        else {
          if (seq.length === 0)
            return val

          return seq.reduceRight((result: Any, elem) => {
            return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
          }, val)
        }
      }
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'rest': {
    evaluate: ([first], sourceCodeInfo): Arr | string => {
      assertSeq(first, sourceCodeInfo)
      if (Array.isArray(first)) {
        if (first.length <= 1)
          return []

        return first.slice(1)
      }
      return first.substr(1)
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'nthrest': {
    evaluate: ([seq, count], sourceCodeInfo): Arr | string => {
      assertSeq(seq, sourceCodeInfo)
      assertNumber(count, sourceCodeInfo, { finite: true })
      const integerCount = Math.max(Math.ceil(count), 0)
      if (Array.isArray(seq))
        return seq.slice(integerCount)

      return seq.substr(integerCount)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'next': {
    evaluate: ([first], sourceCodeInfo): Arr | string | null => {
      assertSeq(first, sourceCodeInfo)
      if (Array.isArray(first)) {
        if (first.length <= 1)
          return null

        return first.slice(1)
      }
      if (first.length <= 1)
        return null

      return first.substr(1)
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'nthnext': {
    evaluate: ([seq, count], sourceCodeInfo): Arr | string | null => {
      assertSeq(seq, sourceCodeInfo)
      assertNumber(count, sourceCodeInfo, { finite: true })
      const integerCount = Math.max(Math.ceil(count), 0)
      if (seq.length <= count)
        return null

      if (Array.isArray(seq))
        return seq.slice(integerCount)

      return seq.substr(integerCount)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'reverse': {
    evaluate: ([seq], sourceCodeInfo): Any => {
      if (seq === null)
        return null

      assertSeq(seq, sourceCodeInfo)
      if (Array.isArray(seq))
        return [...seq].reverse()

      return seq.split('').reverse().join('')
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'second': {
    evaluate: ([array], sourceCodeInfo): Any => {
      if (array === null)
        return null

      assertSeq(array, sourceCodeInfo)
      return toAny(array[1])
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'shift': {
    evaluate: ([seq], sourceCodeInfo): Any => {
      assertSeq(seq, sourceCodeInfo)
      if (typeof seq === 'string')
        return seq.substr(1)

      const copy = [...seq]
      copy.shift()
      return copy
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'slice': {
    evaluate: (params, sourceCodeInfo): Any => {
      const [seq, from, to] = params
      assertSeq(seq, sourceCodeInfo)

      if (params.length === 1)
        return seq

      assertNumber(from, sourceCodeInfo, { integer: true })

      if (params.length === 2)
        return seq.slice(from)

      assertNumber(to, sourceCodeInfo, { integer: true })
      return seq.slice(from, to)
    },
    validate: node => assertNumberOfParams({ min: 1, max: 3 }, node),
  },
  'some': {
    evaluate: ([fn, seq]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertLitsFunction(fn, sourceCodeInfo)
      if (seq === null)
        return null

      assertSeq(seq, sourceCodeInfo)

      if (seq.length === 0)
        return null

      if (typeof seq === 'string')
        return seq.split('').find(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo)) ?? null

      return toAny(seq.find(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo)))
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'sort': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      const defaultComparer = params.length === 1
      const seq = defaultComparer ? params[0] : params[1]
      const comparer = defaultComparer ? null : params[0]
      assertSeq(seq, sourceCodeInfo)

      if (typeof seq === 'string') {
        const result = seq.split('')
        if (defaultComparer) {
          result.sort(compare)
        }
        else {
          assertLitsFunction(comparer, sourceCodeInfo)
          result.sort((a, b) => {
            const compareValue = executeFunction(comparer, [a, b], contextStack, sourceCodeInfo)
            assertNumber(compareValue, sourceCodeInfo, { finite: true })
            return compareValue
          })
        }
        return result.join('')
      }

      const result = [...seq]
      if (defaultComparer) {
        result.sort(compare)
      }
      else {
        result.sort((a, b) => {
          assertLitsFunction(comparer, sourceCodeInfo)
          const compareValue = executeFunction(comparer, [a, b], contextStack, sourceCodeInfo)
          assertNumber(compareValue, sourceCodeInfo, { finite: true })
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

      const keyfn = asAny(params[0], sourceCodeInfo)
      const comparer = defaultComparer ? null : params[1]
      const seq = asSeq(defaultComparer ? params[1] : params[2], sourceCodeInfo)

      if (typeof seq === 'string') {
        const result = seq.split('')
        if (defaultComparer) {
          result.sort((a, b) => {
            const aKey = executeFunction(keyfn, [a], contextStack, sourceCodeInfo)
            const bKey = executeFunction(keyfn, [b], contextStack, sourceCodeInfo)
            return compare(aKey, bKey)
          })
        }
        else {
          assertLitsFunction(comparer, sourceCodeInfo)
          result.sort((a, b) => {
            const aKey = executeFunction(keyfn, [a], contextStack, sourceCodeInfo)
            const bKey = executeFunction(keyfn, [b], contextStack, sourceCodeInfo)
            const compareValue = executeFunction(comparer, [aKey, bKey], contextStack, sourceCodeInfo)
            assertNumber(compareValue, sourceCodeInfo, { finite: true })
            return compareValue
          })
        }
        return result.join('')
      }

      const result = [...seq]
      if (defaultComparer) {
        result.sort((a, b) => {
          const aKey = executeFunction(keyfn, [a], contextStack, sourceCodeInfo)
          const bKey = executeFunction(keyfn, [b], contextStack, sourceCodeInfo)
          return compare(aKey, bKey)
        })
      }
      else {
        assertLitsFunction(comparer, sourceCodeInfo)
        result.sort((a, b) => {
          const aKey = executeFunction(keyfn, [a], contextStack, sourceCodeInfo)
          const bKey = executeFunction(keyfn, [b], contextStack, sourceCodeInfo)
          const compareValue = executeFunction(comparer, [aKey, bKey], contextStack, sourceCodeInfo)
          assertNumber(compareValue, sourceCodeInfo, { finite: true })
          return compareValue
        })
      }
      return result
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'take': {
    evaluate: ([n, input], sourceCodeInfo): Seq => {
      assertNumber(n, sourceCodeInfo)
      assertSeq(input, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)
      return input.slice(0, num)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'take-last': {
    evaluate: ([n, array], sourceCodeInfo): Seq => {
      assertSeq(array, sourceCodeInfo)
      assertNumber(n, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)
      const from = array.length - num
      return array.slice(from)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'take-while': {
    evaluate: ([fn, seq]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertSeq(seq, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)

      const result: Arr = []
      for (const item of seq) {
        if (executeFunction(fn, [item], contextStack, sourceCodeInfo))
          result.push(item)
        else
          break
      }
      return typeof seq === 'string' ? result.join('') : result
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'drop': {
    evaluate: ([n, input], sourceCodeInfo): Seq => {
      assertNumber(n, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)
      assertSeq(input, sourceCodeInfo)
      return input.slice(num)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'drop-last': {
    evaluate: ([n, array], sourceCodeInfo): Seq => {
      assertSeq(array, sourceCodeInfo)
      assertNumber(n, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)

      const from = array.length - num
      return array.slice(0, from)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'drop-while': {
    evaluate: ([fn, seq]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertSeq(seq, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)

      if (Array.isArray(seq)) {
        const from = seq.findIndex(elem => !executeFunction(fn, [elem], contextStack, sourceCodeInfo))
        return seq.slice(from)
      }
      const charArray = seq.split('')
      const from = charArray.findIndex(elem => !executeFunction(fn, [elem], contextStack, sourceCodeInfo))
      return charArray.slice(from).join('')
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'unshift': {
    evaluate: ([seq, ...values], sourceCodeInfo): Seq => {
      assertSeq(seq, sourceCodeInfo)
      if (typeof seq === 'string') {
        assertCharArray(values, sourceCodeInfo)
        return [...values, seq].join('')
      }
      const copy = [...seq]
      copy.unshift(...values)
      return copy
    },
    validate: node => assertNumberOfParams({ min: 2 }, node),
  },
  'random-sample!': {
    evaluate: ([prob, seq], sourceCodeInfo): Seq => {
      assertNumber(prob, sourceCodeInfo, { finite: true })
      assertSeq(seq, sourceCodeInfo)

      if (typeof seq === 'string') {
        return seq
          .split('')
          .filter(() => Math.random() < prob)
          .join('')
      }
      else {
        return seq.filter(() => Math.random() < prob)
      }
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'rand-nth!': {
    evaluate: ([seq], sourceCodeInfo): Any => {
      assertSeq(seq, sourceCodeInfo)
      if (seq.length === 0)
        return null

      const index = Math.floor(Math.random() * seq.length)

      if (typeof seq === 'string')
        return toAny(seq.split('')[index])

      return toAny(seq[index])
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'shuffle!': {
    evaluate: ([input], sourceCodeInfo): Seq => {
      assertSeq(input, sourceCodeInfo)
      const array: Arr = typeof input === 'string' ? [...input.split('')] : [...input]
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

      return typeof input === 'string' ? array.join('') : array
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'distinct': {
    evaluate: ([input], sourceCodeInfo): Seq => {
      assertSeq(input, sourceCodeInfo)
      if (Array.isArray(input))
        return Array.from(new Set(input))

      return Array.from(new Set(input.split(''))).join('')
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'remove': {
    evaluate: ([fn, input], sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      assertLitsFunction(fn, sourceCodeInfo)
      assertSeq(input, sourceCodeInfo)
      if (Array.isArray(input))
        return input.filter(elem => !executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      return input
        .split('')
        .filter(elem => !executeFunction(fn, [elem], contextStack, sourceCodeInfo))
        .join('')
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'remove-at': {
    evaluate: ([index, input], sourceCodeInfo): Seq => {
      assertNumber(index, sourceCodeInfo)
      assertSeq(input, sourceCodeInfo)

      const intIndex = Math.ceil(index)
      if (intIndex < 0 || intIndex >= input.length)
        return input

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
      assertNumber(pos, sourceCodeInfo, { finite: true })
      const intPos = toNonNegativeInteger(pos)
      assertSeq(seq, sourceCodeInfo)
      return [seq.slice(0, intPos), seq.slice(intPos)]
    },
    validate: node => assertNumberOfParams(2, node),
  },

  'split-with': {
    evaluate: ([fn, seq], sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      assertLitsFunction(fn, sourceCodeInfo)
      assertSeq(seq, sourceCodeInfo)
      const seqIsArray = Array.isArray(seq)
      const arr = seqIsArray ? seq : seq.split('')
      const index = arr.findIndex(elem => !executeFunction(fn, [elem], contextStack, sourceCodeInfo))
      if (index === -1)
        return [seq, seqIsArray ? [] : '']

      return [seq.slice(0, index), seq.slice(index)]
    },
    validate: node => assertNumberOfParams(2, node),
  },

  'frequencies': {
    evaluate: ([seq], sourceCodeInfo): Obj => {
      assertSeq(seq, sourceCodeInfo)

      const arr = typeof seq === 'string' ? seq.split('') : seq

      return arr.reduce((result: Obj, val) => {
        assertString(val, sourceCodeInfo)
        if (collHasKey(result, val))
          result[val] = (result[val] as number) + 1
        else
          result[val] = 1

        return result
      }, {})
    },
    validate: node => assertNumberOfParams(1, node),
  },

  'group-by': {
    evaluate: ([fn, seq], sourceCodeInfo, contextStack, { executeFunction }): Obj => {
      assertAny(fn, sourceCodeInfo)
      assertSeq(seq, sourceCodeInfo)
      const arr = Array.isArray(seq) ? seq : seq.split('')

      return arr.reduce((result: Obj, val) => {
        const key = executeFunction(fn, [val], contextStack, sourceCodeInfo)
        assertString(key, sourceCodeInfo)
        if (!collHasKey(result, key))
          result[key] = []

        ;(result[key] as Arr).push(val)
        return result
      }, {})
    },
    validate: node => assertNumberOfParams(2, node),
  },

  'partition': {
    evaluate: (params, sourceCodeInfo): Seq => {
      const len = params.length
      const n = toNonNegativeInteger(asNumber(params[0], sourceCodeInfo))
      const seq
        = len === 2
          ? asSeq(params[1], sourceCodeInfo)
          : len === 3
            ? asSeq(params[2], sourceCodeInfo)
            : asSeq(params[3], sourceCodeInfo)
      const step = len >= 3 ? toNonNegativeInteger(asNumber(params[1], sourceCodeInfo)) : n
      const pad = len === 4 ? (params[2] === null ? [] : asArray(params[2], sourceCodeInfo)) : undefined

      return partition(n, step, seq, pad, sourceCodeInfo)
    },
    validate: node => assertNumberOfParams({ min: 2, max: 4 }, node),
  },

  'partition-all': {
    evaluate: (params, sourceCodeInfo): Seq => {
      const len = params.length
      const n = toNonNegativeInteger(asNumber(params[0], sourceCodeInfo))
      const seq = len === 2 ? asSeq(params[1], sourceCodeInfo) : asSeq(params[2], sourceCodeInfo)
      const step = len >= 3 ? toNonNegativeInteger(asNumber(params[1], sourceCodeInfo)) : n

      return partition(n, step, seq, [], sourceCodeInfo)
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },

  'partition-by': {
    evaluate: ([fn, seq], sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      assertLitsFunction(fn, sourceCodeInfo)
      assertSeq(seq, sourceCodeInfo)
      const isStringSeq = typeof seq === 'string'
      let oldValue: unknown

      const result = (isStringSeq ? seq.split('') : seq).reduce((acc: Arr, elem) => {
        const value = executeFunction(fn, [elem], contextStack, sourceCodeInfo)
        if (value !== oldValue) {
          acc.push([])
          oldValue = value
        }
        ;(acc[acc.length - 1] as Arr).push(elem)
        return acc
      }, [])

      return isStringSeq ? result.map(elem => (elem as Arr).join('')) : result
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
}

function partition(n: number, step: number, seq: Seq, pad: Arr | undefined, sourceCodeInfo?: SourceCodeInfo) {
  assertNumber(step, sourceCodeInfo, { positive: true })
  const isStringSeq = typeof seq === 'string'

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
        if (padIndex >= pad.length)
          break

        innerArr.push(pad[padIndex])
      }
      else {
        innerArr.push(seq[i])
      }
    }
    result.push(innerArr)
    start += step
  }
  return isStringSeq ? result.map(x => x.join('')) : result
}
