import type { Any, Arr, Obj, Seq } from '../../../interface'
import type { SourceCodeInfo } from '../../../tokenizer/token'
import { asArray, assertArray, assertCharArray } from '../../../typeGuards/array'
import { asAny, asSeq, assertAny, assertFunctionLike, assertSeq } from '../../../typeGuards/lits'
import { asNumber, assertNumber } from '../../../typeGuards/number'
import { assertString, assertStringOrNumber } from '../../../typeGuards/string'
import { collHasKey, compare, deepEqual, toAny, toNonNegativeInteger } from '../../../utils'
import { toFixedArity } from '../../../utils/arity'
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
      if (i >= 0 && i < seq.length) {
        const result = toAny(seq[i])
        return result
      }
      else {
        return defaultValue
      }
    },
    arity: { min: 2, max: 3 },
  },
  'first': {
    evaluate: ([array], sourceCodeInfo): Any => {
      if (array === null)
        return null

      assertSeq(array, sourceCodeInfo)
      const result = toAny(array[0])

      return result
    },
    arity: toFixedArity(1),
  },
  'last': {
    evaluate: ([array], sourceCodeInfo): Any => {
      if (array === null)
        return null

      assertSeq(array, sourceCodeInfo)
      const result = toAny(array.at(-1))

      return result
    },
    arity: toFixedArity(1),
  },
  'pop': {
    evaluate: ([seq], sourceCodeInfo): Seq => {
      assertSeq(seq, sourceCodeInfo)
      if (typeof seq === 'string') {
        return seq.substring(0, seq.length - 1)
      }

      return seq.slice(0, seq.length - 1)
    },
    arity: toFixedArity(1),
  },
  'position': {
    evaluate: ([seq, fn]: Arr, sourceCodeInfo, contextStack, { executeFunction }): number | null => {
      assertFunctionLike(fn, sourceCodeInfo)
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
    arity: toFixedArity(2),
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
        const index = seq.findIndex(item => deepEqual(asAny(item, sourceCodeInfo), value), sourceCodeInfo)
        return index !== -1 ? index : null
      }
    },
    arity: toFixedArity(2),
  },
  'last-index-of': {
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
        const index = seq.findLastIndex(item => deepEqual(asAny(item, sourceCodeInfo), value), sourceCodeInfo)
        return index !== -1 ? index : null
      }
    },
    arity: toFixedArity(2),
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
    arity: { min: 2 },
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
    arity: toFixedArity(1),
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
    arity: toFixedArity(1),
  },
  'reverse': {
    evaluate: ([seq], sourceCodeInfo): Any => {
      if (seq === null)
        return null

      assertSeq(seq, sourceCodeInfo)
      if (Array.isArray(seq)) {
        return [...seq].reverse()
      }

      return seq.split('').reverse().join('')
    },
    arity: toFixedArity(1),
  },
  'second': {
    evaluate: ([seq], sourceCodeInfo): Any => {
      if (seq === null)
        return null

      assertSeq(seq, sourceCodeInfo)
      return toAny(seq[1])
    },
    arity: toFixedArity(1),
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
    arity: toFixedArity(1),
  },
  'slice': {
    evaluate: (params, sourceCodeInfo): Any => {
      const [seq, from, to] = params
      assertSeq(seq, sourceCodeInfo)
      assertNumber(from, sourceCodeInfo, { integer: true })

      if (params.length === 2) {
        if (Array.isArray(seq)) {
          return seq.slice(from)
        }
        return seq.slice(from)
      }

      assertNumber(to, sourceCodeInfo, { integer: true })
      if (Array.isArray(seq)) {
        return seq.slice(from, to)
      }
      return seq.slice(from, to)
    },
    arity: { min: 2, max: 3 },
  },
  'splice': {
    evaluate: (params, sourceCodeInfo): Any => {
      const [seq, start, deleteCount, ...rest] = params
      assertSeq(seq, sourceCodeInfo)
      assertNumber(start, sourceCodeInfo, { integer: true })
      assertNumber(deleteCount, sourceCodeInfo, { integer: true, nonNegative: true })

      const from = start < 0 ? seq.length + start : start

      if (Array.isArray(seq)) {
        return [...seq.slice(0, from), ...rest, ...seq.slice(from + deleteCount)]
      }

      rest.forEach(elem => assertString(elem, sourceCodeInfo))
      return `${seq.substring(0, from)}${rest.join('')}${seq.substring(from + deleteCount)}`
    },
    arity: { min: 3 },
  },
  'some': {
    evaluate: ([seq, fn]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertFunctionLike(fn, sourceCodeInfo)
      if (seq === null)
        return null

      assertSeq(seq, sourceCodeInfo)

      if (seq.length === 0)
        return null

      if (typeof seq === 'string')
        return seq.split('').find(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo)) ?? null

      return toAny(seq.find(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo)))
    },
    arity: toFixedArity(2),
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
          result.sort((a, b) => compare(a, b, sourceCodeInfo))
        }
        else {
          assertFunctionLike(comparer, sourceCodeInfo)
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
        result.sort((a, b) => {
          assertStringOrNumber(a, sourceCodeInfo)
          assertStringOrNumber(b, sourceCodeInfo)
          return compare(a, b, sourceCodeInfo)
        })
      }
      else {
        result.sort((a, b) => {
          assertFunctionLike(comparer, sourceCodeInfo)
          const compareValue = executeFunction(comparer, [a, b], contextStack, sourceCodeInfo)
          assertNumber(compareValue, sourceCodeInfo, { finite: true })
          return compareValue
        })
      }
      return result
    },
    arity: { min: 1, max: 2 },
  },
  'sort-by': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      const [seq, keyfn] = params
      const defaultComparer = params.length === 2

      assertSeq(seq, sourceCodeInfo)
      assertFunctionLike(keyfn, sourceCodeInfo)
      const comparer = defaultComparer ? null : params[2]

      if (typeof seq === 'string') {
        const result = seq.split('')
        if (defaultComparer) {
          result.sort((a, b) => {
            const aKey = executeFunction(keyfn, [a], contextStack, sourceCodeInfo)
            assertStringOrNumber(aKey, sourceCodeInfo)
            const bKey = executeFunction(keyfn, [b], contextStack, sourceCodeInfo)
            assertStringOrNumber(bKey, sourceCodeInfo)
            return compare(aKey, bKey, sourceCodeInfo)
          })
        }
        else {
          assertFunctionLike(comparer, sourceCodeInfo)
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
          assertStringOrNumber(aKey, sourceCodeInfo)
          const bKey = executeFunction(keyfn, [b], contextStack, sourceCodeInfo)
          assertStringOrNumber(bKey, sourceCodeInfo)
          return compare(aKey, bKey, sourceCodeInfo)
        })
      }
      else {
        assertFunctionLike(comparer, sourceCodeInfo)
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
    arity: { min: 2, max: 3 },
  },
  'take': {
    evaluate: ([input, n], sourceCodeInfo): Seq => {
      assertNumber(n, sourceCodeInfo)
      assertSeq(input, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)
      return input.slice(0, num)
    },
    arity: toFixedArity(2),
  },
  'take-last': {
    evaluate: ([array, n], sourceCodeInfo): Seq => {
      assertSeq(array, sourceCodeInfo)
      assertNumber(n, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)
      const from = array.length - num
      return array.slice(from)
    },
    arity: toFixedArity(2),
  },
  'take-while': {
    evaluate: ([seq, fn]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertSeq(seq, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)

      const result: Arr = []
      for (const item of seq) {
        if (executeFunction(fn, [item], contextStack, sourceCodeInfo))
          result.push(item)
        else
          break
      }
      return typeof seq === 'string' ? result.join('') : result
    },
    arity: toFixedArity(2),
  },
  'drop': {
    evaluate: ([input, n], sourceCodeInfo): Seq => {
      assertNumber(n, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)
      assertSeq(input, sourceCodeInfo)
      return input.slice(num)
    },
    arity: toFixedArity(2),
  },
  'drop-last': {
    evaluate: ([array, n], sourceCodeInfo): Seq => {
      assertSeq(array, sourceCodeInfo)
      assertNumber(n, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)

      const from = array.length - num
      return array.slice(0, from)
    },
    arity: toFixedArity(2),
  },
  'drop-while': {
    evaluate: ([seq, fn]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertSeq(seq, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)

      if (Array.isArray(seq)) {
        const from = seq.findIndex(elem => !executeFunction(fn, [elem], contextStack, sourceCodeInfo))
        return seq.slice(from)
      }
      const charArray = seq.split('')
      const from = charArray.findIndex(elem => !executeFunction(fn, [elem], contextStack, sourceCodeInfo))
      return charArray.slice(from).join('')
    },
    arity: toFixedArity(2),
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
    arity: { min: 2 },
  },
  'distinct': {
    evaluate: ([input], sourceCodeInfo): Seq => {
      assertSeq(input, sourceCodeInfo)

      if (Array.isArray(input)) {
        const result: Any[] = []
        for (const item of input) {
          assertAny(item, sourceCodeInfo)
          if (!result.some(existingItem => deepEqual(existingItem, item, sourceCodeInfo))) {
            result.push(item)
          }
        }
        return result
      }

      return Array.from(new Set(input.split(''))).join('')
    },
    arity: toFixedArity(1),
  },
  'remove': {
    evaluate: ([input, fn], sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      assertFunctionLike(fn, sourceCodeInfo)
      assertSeq(input, sourceCodeInfo)
      if (Array.isArray(input))
        return input.filter(elem => !executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      return input
        .split('')
        .filter(elem => !executeFunction(fn, [elem], contextStack, sourceCodeInfo))
        .join('')
    },
    arity: toFixedArity(2),
  },
  'remove-at': {
    evaluate: ([input, index], sourceCodeInfo): Seq => {
      assertNumber(index, sourceCodeInfo, { integer: true })
      assertSeq(input, sourceCodeInfo)

      const at = index < 0 ? input.length + index : index
      if (at < 0 || at >= input.length)
        return input

      if (Array.isArray(input)) {
        return input.filter((_, i) => i !== at)
      }
      return `${input.substring(0, at)}${input.substring(at + 1)}`
    },
    arity: toFixedArity(2),
  },
  'split-at': {
    evaluate: ([seq, pos], sourceCodeInfo): Seq => {
      assertNumber(pos, sourceCodeInfo, { integer: true })
      assertSeq(seq, sourceCodeInfo)

      const at = pos < 0 ? seq.length + pos : pos
      return [seq.slice(0, at), seq.slice(at)]
    },
    arity: toFixedArity(2),
  },

  'split-with': {
    evaluate: ([seq, fn], sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      assertFunctionLike(fn, sourceCodeInfo)
      assertSeq(seq, sourceCodeInfo)
      const seqIsArray = Array.isArray(seq)
      const arr = seqIsArray ? seq : seq.split('')
      const index = arr.findIndex(elem => !executeFunction(fn, [elem], contextStack, sourceCodeInfo))
      if (index === -1)
        return [seq, seqIsArray ? [] : '']

      return [seq.slice(0, index), seq.slice(index)]
    },
    arity: toFixedArity(2),
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
    arity: toFixedArity(1),
  },

  'group-by': {
    evaluate: ([seq, fn], sourceCodeInfo, contextStack, { executeFunction }): Obj => {
      assertFunctionLike(fn, sourceCodeInfo)
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
    arity: toFixedArity(2),
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
    arity: { min: 2, max: 4 },
  },

  'partition-all': {
    evaluate: (params, sourceCodeInfo): Seq => {
      const seq = asSeq(params[0], sourceCodeInfo)
      const n = toNonNegativeInteger(asNumber(params[1], sourceCodeInfo))
      const step = params.length === 3 ? toNonNegativeInteger(asNumber(params[2], sourceCodeInfo)) : n

      return partition(n, step, seq, [], sourceCodeInfo)
    },
    arity: { min: 2, max: 3 },
  },

  'partition-by': {
    evaluate: ([seq, fn], sourceCodeInfo, contextStack, { executeFunction }): Seq => {
      assertFunctionLike(fn, sourceCodeInfo)
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
    arity: toFixedArity(2),
  },
  'ends-with?': {
    evaluate: ([str, search], sourceCodeInfo): boolean => {
      assertSeq(str, sourceCodeInfo)

      if (typeof str === 'string') {
        assertString(search, sourceCodeInfo)
        return str.endsWith(search)
      }

      return deepEqual(asAny(str.at(-1), sourceCodeInfo), asAny(search, sourceCodeInfo), sourceCodeInfo)
    },
    arity: toFixedArity(2),
  },
  'starts-with?': {
    evaluate: ([seq, search], sourceCodeInfo): boolean => {
      assertSeq(seq, sourceCodeInfo)

      if (typeof seq === 'string') {
        assertString(search, sourceCodeInfo)
        return seq.startsWith(search)
      }

      return deepEqual(asAny(seq[0], sourceCodeInfo), asAny(search, sourceCodeInfo), sourceCodeInfo)
    },
    arity: toFixedArity(2),
  },
  'interleave': {
    evaluate: ([...seqs], sourceCodeInfo): Seq => {
      const isStringSeq = typeof seqs[0] === 'string'

      const seqsArr = isStringSeq
        ? seqs.map((seq) => {
            assertString(seq, sourceCodeInfo)
            return seq.split('')
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
    arity: { min: 1 },
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
    arity: toFixedArity(2),
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
