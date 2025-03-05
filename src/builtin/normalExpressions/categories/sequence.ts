import type { Any, Arr, Obj, Seq } from '../../../interface'
import type { SourceCodeInfo } from '../../../tokenizer/interface'
import { asArray, assertArray, assertCharArray } from '../../../typeGuards/array'
import { asAny, asSeq, assertAny, assertSeq } from '../../../typeGuards/lits'
import { assertLitsFunction } from '../../../typeGuards/litsFunction'
import { asNumber, assertNumber } from '../../../typeGuards/number'
import { assertString } from '../../../typeGuards/string'
import { collHasKey, compare, toAny, toNonNegativeInteger } from '../../../utils'
import type { BuiltinNormalExpressions } from '../../interface'

export const sequenceNormalExpression: BuiltinNormalExpressions = {
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
    paramCount: { min: 2, max: 3 },
  },
  'filter': {
    evaluate: ([seq, fn]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      assertSeq(seq, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)
      if (Array.isArray(seq))
        return seq.filter(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      return seq
        .split('')
        .filter(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
        .join('')
    },
    paramCount: 2,
  },
  'first': {
    evaluate: ([array], sourceCodeInfo): Any => {
      if (array === null)
        return null

      assertSeq(array, sourceCodeInfo)
      return toAny(array[0])
    },
    paramCount: 1,
  },
  'last': {
    evaluate: ([array], sourceCodeInfo): Any => {
      if (array === null)
        return null

      assertSeq(array, sourceCodeInfo)
      return toAny(array[array.length - 1])
    },
    paramCount: 1,
  },
  'map': {
    evaluate: ([seq, fn], sourceCodeInfo, contextStack, { executeFunction }) => {
      assertSeq(seq, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)

      if (Array.isArray(seq)) {
        return seq.map(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
      }
      else {
        return seq
          .split('')
          .map((elem) => {
            const newVal = executeFunction(fn, [elem], contextStack, sourceCodeInfo)
            assertString(newVal, sourceCodeInfo, { char: true })
            return newVal
          })
          .join('')
      }
    },
    paramCount: 2,
  },
  'pop': {
    evaluate: ([seq], sourceCodeInfo): Seq => {
      assertSeq(seq, sourceCodeInfo)
      if (typeof seq === 'string') {
        return seq.substring(0, seq.length - 1)
      }

      return seq.slice(0, seq.length - 1)
    },
    paramCount: 1,
  },
  'position': {
    evaluate: ([seq, fn]: Arr, sourceCodeInfo, contextStack, { executeFunction }): number | null => {
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
    paramCount: 2,
  },
  'index_of': {
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
    paramCount: 2,
  },
  'last_index_of': {
    evaluate: ([seq, value], sourceCodeInfo): number | null => {
      assertAny(value, sourceCodeInfo)
      if (seq === null)
        return null

      assertSeq(seq, sourceCodeInfo)
      if (typeof seq === 'string') {
        assertString(value, sourceCodeInfo)
        const index = seq.lastIndexOf(value)
        return index !== -1 ? index : null
      }
      else {
        const index = seq.lastIndexOf(value)
        return index !== -1 ? index : null
      }
    },
    paramCount: 2,
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
    paramCount: { min: 2 },
  },
  'reductions': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any[] => {
      const [seq, fn] = params
      assertSeq(seq, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)

      if (params.length === 2) {
        if (seq.length === 0)
          return [executeFunction(fn, [], contextStack, sourceCodeInfo)]
        else if (seq.length === 1)
          return [toAny(seq[0])]

        if (typeof seq === 'string') {
          const chars = seq.split('')
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
          const resultArray: Any[] = [toAny(seq[0])]
          seq.slice(1).reduce((result: Any, elem) => {
            const newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
            resultArray.push(newVal)
            return newVal
          }, toAny(seq[0]))
          return resultArray
        }
      }
      else {
        const val = params[2]
        assertAny(val, sourceCodeInfo)
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
    paramCount: { min: 2, max: 3 },
  },
  'reduce': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      const [seq, fn] = params
      assertSeq(seq, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)

      if (params.length === 2) {
        if (seq.length === 0)
          return executeFunction(fn, [], contextStack, sourceCodeInfo)
        else if (seq.length === 1)
          return toAny(seq[0])

        if (typeof seq === 'string') {
          const chars = seq.split('')
          return chars.slice(1).reduce(
            (result: Any, elem) => {
              const val = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
              return val
            },
            asAny(chars[0], sourceCodeInfo),
          )
        }
        else {
          return seq.slice(1).reduce((result: Any, elem) => {
            return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
          }, toAny(seq[0]))
        }
      }
      else {
        const val = params[2]
        assertAny(val, sourceCodeInfo)
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
    paramCount: { min: 2, max: 3 },
  },
  'reduce_right': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      const [seq, fn] = params
      assertSeq(seq, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)

      if (params.length === 2) {
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
        const val = params[2]
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
    paramCount: { min: 2, max: 3 },
  },
  'rest': {
    evaluate: ([seq], sourceCodeInfo): Arr | string => {
      assertSeq(seq, sourceCodeInfo)
      if (Array.isArray(seq)) {
        if (seq.length <= 1)
          return []

        return seq.slice(1)
      }
      return seq.substring(1)
    },
    paramCount: 1,
  },
  'nthrest': {
    evaluate: ([seq, count], sourceCodeInfo): Arr | string => {
      assertSeq(seq, sourceCodeInfo)
      assertNumber(count, sourceCodeInfo, { finite: true })
      const integerCount = Math.max(Math.ceil(count), 0)
      if (Array.isArray(seq))
        return seq.slice(integerCount)

      return seq.substring(integerCount)
    },
    paramCount: 2,
  },
  'next': {
    evaluate: ([seq], sourceCodeInfo): Arr | string | null => {
      assertSeq(seq, sourceCodeInfo)
      if (Array.isArray(seq)) {
        if (seq.length <= 1)
          return null

        return seq.slice(1)
      }
      if (seq.length <= 1)
        return null

      return seq.substring(1)
    },
    paramCount: 1,
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

      return seq.substring(integerCount)
    },
    paramCount: 2,
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
    paramCount: 1,
  },
  'second': {
    evaluate: ([seq], sourceCodeInfo): Any => {
      if (seq === null)
        return null

      assertSeq(seq, sourceCodeInfo)
      return toAny(seq[1])
    },
    paramCount: 1,
  },
  'shift': {
    evaluate: ([seq], sourceCodeInfo): Any => {
      assertSeq(seq, sourceCodeInfo)
      if (typeof seq === 'string')
        return seq.substring(1)

      const copy = [...seq]
      copy.shift()
      return copy
    },
    paramCount: 1,
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
    paramCount: { min: 1, max: 3 },
  },
  'some': {
    evaluate: ([seq, fn]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
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
    paramCount: 2,
  },
  'sort': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      const [seq] = params
      const defaultComparer = params.length === 1
      const comparer = defaultComparer ? null : params[1]
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
    paramCount: { min: 1, max: 2 },
  },
  'sort_by': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      const [seq, keyfn] = params
      const defaultComparer = params.length === 2

      assertSeq(seq, sourceCodeInfo)
      assertAny(keyfn, sourceCodeInfo)
      const comparer = defaultComparer ? null : params[2]

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
    paramCount: { min: 2, max: 3 },
  },
  'take': {
    evaluate: ([input, n], sourceCodeInfo): Seq => {
      assertNumber(n, sourceCodeInfo)
      assertSeq(input, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)
      return input.slice(0, num)
    },
    paramCount: 2,
  },
  'take_last': {
    evaluate: ([array, n], sourceCodeInfo): Seq => {
      assertSeq(array, sourceCodeInfo)
      assertNumber(n, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)
      const from = array.length - num
      return array.slice(from)
    },
    paramCount: 2,
  },
  'take_while': {
    evaluate: ([seq, fn]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
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
    paramCount: 2,
  },
  'drop': {
    evaluate: ([input, n], sourceCodeInfo): Seq => {
      assertNumber(n, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)
      assertSeq(input, sourceCodeInfo)
      return input.slice(num)
    },
    paramCount: 2,
  },
  'drop_last': {
    evaluate: ([array, n], sourceCodeInfo): Seq => {
      assertSeq(array, sourceCodeInfo)
      assertNumber(n, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)

      const from = array.length - num
      return array.slice(0, from)
    },
    paramCount: 2,
  },
  'drop_while': {
    evaluate: ([seq, fn]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
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
    paramCount: 2,
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
    paramCount: { min: 2 },
  },
  'distinct': {
    evaluate: ([input], sourceCodeInfo): Seq => {
      assertSeq(input, sourceCodeInfo)
      if (Array.isArray(input))
        return Array.from(new Set(input))

      return Array.from(new Set(input.split(''))).join('')
    },
    paramCount: 1,
  },
  'remove': {
    evaluate: ([input, fn], sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      assertLitsFunction(fn, sourceCodeInfo)
      assertSeq(input, sourceCodeInfo)
      if (Array.isArray(input))
        return input.filter(elem => !executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      return input
        .split('')
        .filter(elem => !executeFunction(fn, [elem], contextStack, sourceCodeInfo))
        .join('')
    },
    paramCount: 2,
  },
  'remove_at': {
    evaluate: ([input, index], sourceCodeInfo): Seq => {
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
    paramCount: 2,
  },
  'split_at': {
    evaluate: ([seq, pos], sourceCodeInfo): Seq => {
      assertNumber(pos, sourceCodeInfo, { finite: true })
      const intPos = toNonNegativeInteger(pos)
      assertSeq(seq, sourceCodeInfo)
      return [seq.slice(0, intPos), seq.slice(intPos)]
    },
    paramCount: 2,
  },

  'split_with': {
    evaluate: ([seq, fn], sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      assertLitsFunction(fn, sourceCodeInfo)
      assertSeq(seq, sourceCodeInfo)
      const seqIsArray = Array.isArray(seq)
      const arr = seqIsArray ? seq : seq.split('')
      const index = arr.findIndex(elem => !executeFunction(fn, [elem], contextStack, sourceCodeInfo))
      if (index === -1)
        return [seq, seqIsArray ? [] : '']

      return [seq.slice(0, index), seq.slice(index)]
    },
    paramCount: 2,
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
    paramCount: 1,
  },

  'group_by': {
    evaluate: ([seq, fn], sourceCodeInfo, contextStack, { executeFunction }): Obj => {
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
    paramCount: 2,
  },

  'partition': {
    evaluate: (params, sourceCodeInfo): Seq => {
      const seq = asSeq(params[0], sourceCodeInfo)
      const n = toNonNegativeInteger(asNumber(params[1], sourceCodeInfo))
      const step = params.length >= 3 ? toNonNegativeInteger(asNumber(params[2], sourceCodeInfo)) : n
      const pad = params.length === 4
        ? params[3] === null ? [] : asArray(params[3], sourceCodeInfo)
        : undefined

      return partition(n, step, seq, pad, sourceCodeInfo)
    },
    paramCount: { min: 2, max: 4 },
  },

  'partition_all': {
    evaluate: (params, sourceCodeInfo): Seq => {
      const seq = asSeq(params[0], sourceCodeInfo)
      const n = toNonNegativeInteger(asNumber(params[1], sourceCodeInfo))
      const step = params.length === 3 ? toNonNegativeInteger(asNumber(params[2], sourceCodeInfo)) : n

      return partition(n, step, seq, [], sourceCodeInfo)
    },
    paramCount: { min: 2, max: 3 },
  },

  'partition_by': {
    evaluate: ([seq, fn], sourceCodeInfo, contextStack, { executeFunction }): Seq => {
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
    paramCount: 2,
  },
  'ends_with?': {
    evaluate: ([str, search], sourceCodeInfo): boolean => {
      assertSeq(str, sourceCodeInfo)

      if (typeof str === 'string') {
        assertString(search, sourceCodeInfo)
        return str.endsWith(search)
      }

      return str.at(-1) === search
    },
    paramCount: 2,
  },
  'starts_with?': {
    evaluate: ([str, search], sourceCodeInfo): boolean => {
      assertSeq(str, sourceCodeInfo)

      if (typeof str === 'string') {
        assertString(search, sourceCodeInfo)
        return str.startsWith(search)
      }

      return str[0] === search
    },
    paramCount: 2,
  },
  'interleave': {
    evaluate: ([...seqs], sourceCodeInfo): Seq => {
      const isStringSeq = typeof seqs[0] === 'string'

      const seqsArr = isStringSeq
        ? seqs.map((seq) => {
            assertString(seq, sourceCodeInfo)
            if (typeof seq === 'string')
              return seq.split('')

            return seq
          })
        : seqs.map((seq) => {
            assertArray(seq, sourceCodeInfo)
            return seq
          })

      const maxLength = Math.min(...seqsArr.map(seq => seq.length))
      const result: Arr = []
      for (let i = 0; i < maxLength; i += 1) {
        for (const seq of seqsArr) {
          if (i < seq.length)
            result.push(seq[i])
        }
      }
      return isStringSeq ? result.join('') : result
    },
    paramCount: { min: 1 },
  },
  'interpose': {
    evaluate: ([seq, separator], sourceCodeInfo): Seq => {
      assertSeq(seq, sourceCodeInfo)
      if (typeof seq === 'string') {
        assertString(separator, sourceCodeInfo)
        return seq.split('').join(separator)
      }

      if (seq.length === 0)
        return []

      const result: Arr = []
      for (let i = 0; i < seq.length - 1; i += 1) {
        result.push(seq[i], separator)
      }
      result.push(seq[seq.length - 1])
      return result
    },
    paramCount: 2,
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
