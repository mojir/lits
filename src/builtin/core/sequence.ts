import type { Any, Arr, Seq } from '../../interface'
import { assertCharArray } from '../../typeGuards/array'
import { asAny, assertAny, assertFunctionLike, assertSeq } from '../../typeGuards/lits'
import { assertNumber } from '../../typeGuards/number'
import { assertString, assertStringOrNumber } from '../../typeGuards/string'
import { compare, deepEqual, toAny } from '../../utils'
import { toFixedArity } from '../../utils/arity'
import type { BuiltinNormalExpressions } from '../interface'

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
    docs: {
      category: 'Sequence',
      returns: { type: 'any' },
      args: {
        'a': { type: 'sequence' },
        'b': { type: 'integer' },
        'seq': { type: ['sequence', 'null'] },
        'n': { type: 'integer' },
        'not-found': { type: 'any' },
      },
      variants: [
        { argumentNames: ['seq', 'n'] },
        { argumentNames: ['seq', 'n', 'not-found'] },
      ],
      description: 'Accesses element $n of $seq. Accessing out-of-bounds indices returns $not-found, if present, else `null`.',
      seeAlso: ['first', 'second', 'last', 'get', 'slice'],
      examples: [
        '[1, 2, 3] nth 1',
        '"A string" nth 3',
        'nth([1, 2, 3], 1)',
        'nth([1, 2, 3], 3)',
        'nth([1, 2, 3], -1)',
        'nth([1, 2, 3], 3, 99)',
        'nth("A string", 1)',
        'nth("A string", 3)',
        'nth("A string", -3)',
        'nth("A string", 30, "X")',
        'nth(null, 1)',
        'nth(null, 1, "Default value")',
      ],
    },
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
    docs: {
      category: 'Sequence',
      returns: { type: 'any' },
      args: { seq: { type: ['sequence', 'null'] } },
      variants: [{ argumentNames: ['seq'] }],
      description: 'Returns the first element of $seq. If $seq is empty or `null`, `null` is returned.',
      seeAlso: ['second', 'last', 'nth', 'rest', 'next'],
      examples: [
        'first(["Albert", "Mojir", 160, [1, 2]])',
        'first([])',
        'first(null)',
      ],
    },
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
    docs: {
      category: 'Sequence',
      returns: { type: 'any' },
      args: { seq: { type: ['sequence', 'null'] } },
      variants: [{ argumentNames: ['seq'] }],
      description: 'Returns the last element of $seq. If $seq is empty, `null` is returned.',
      seeAlso: ['first', 'second', 'nth', 'pop'],
      examples: [
        'last(["Albert", "Mojir", 160, [1, 2]])',
        'last([1, 2])',
        'last([1])',
        'last([])',
        'last(null)',
      ],
    },
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
    docs: {
      category: 'Sequence',
      returns: { type: ['sequence', 'null'], rest: true },
      args: { seq: { type: 'sequence' } },
      variants: [{ argumentNames: ['seq'] }],
      description: 'Returns a copy of $seq with last element removed. If $seq is empty `null` is returned.',
      seeAlso: ['push', 'Sequence-Utils.shift', 'last'],
      examples: [
        'pop([1, 2, 3])',
        'pop([])',
      ],
    },
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
    docs: {
      category: 'Sequence',
      returns: { type: ['number', 'null'] },
      args: {
        a: { type: 'sequence' },
        b: { type: 'any' },
        seq: { type: ['sequence', 'null'] },
        x: { type: 'any' },
      },
      variants: [{ argumentNames: ['seq', 'x'] }],
      description: 'Returns the index of $x in $seq. If element is not present in $seq `null` is returned.',
      seeAlso: ['Sequence-Utils.last-index-of', 'Sequence-Utils.position', 'contains?'],
      examples: [
        '[[1], [2], [1], [2]] index-of [1]',
        'index-of(["Albert", "Mojir", 160, [1, 2]], "Mojir")',
        'index-of([5, 10, 15, 20], 15)',
        'index-of([5, 10, 15, 20], 1)',
        'index-of(null, 1)',
      ],
    },
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
    docs: {
      category: 'Sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'any' },
        seq: { type: 'sequence' },
        values: { type: 'any', rest: true, description: 'At least one.' },
      },
      variants: [{ argumentNames: ['seq', 'values'] }],
      description: 'Returns copy of $seq with $values added to the end of it.',
      seeAlso: ['Sequence-Utils.unshift', 'pop', '++'],
      examples: [
        '[1, 2, 3] push 4',
        '"Albert" push "!"',
        'push([1, 2, 3], 4)',
        'push([1, 2, 3], 4, 5, 6)',
        `
let l = [1, 2, 3];
push(l, 4);
l`,
      ],
    },
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
    docs: {
      category: 'Sequence',
      returns: { type: ['sequence', 'null'] },
      args: { seq: { type: 'sequence' } },
      variants: [{ argumentNames: ['seq'] }],
      description: `If $seq is an array, returns a new array with all but the first element from $seq.
If $seq has less than two elements, an empty array is returned.
For string $seq returns all but the first characters in $seq.`,
      seeAlso: ['next', 'first', 'Sequence-Utils.shift'],
      examples: [
        'rest(["Albert", "Mojir", 160, [1, 2]])',
        'rest(["Albert"])',
        'rest([])',
        'rest("Albert")',
        'rest("A",)',
        'rest("")',
      ],
    },
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
    docs: {
      category: 'Sequence',
      returns: { type: ['sequence', 'null'] },
      args: { seq: { type: 'sequence' } },
      variants: [{ argumentNames: ['seq'] }],
      description: 'If $seq is an array, returns a new array with all but the first element from $seq. If $seq has less than two elements, `null` is returned. For string $seq returns all but the first characters in $seq. If length of string $seq is less than two, `null` is returned.',
      seeAlso: ['rest', 'first'],
      examples: [
        'next(["Albert", "Mojir", 160, [1, 2]])',
        'next(["Albert"])',
        'next([])',
        'next("Albert")',
        'next("A",)',
        'next("")',
      ],
    },
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
    docs: {
      category: 'Sequence',
      returns: { type: ['sequence', 'null'] },
      args: { seq: { type: ['sequence', 'null'] } },
      variants: [{ argumentNames: ['seq'] }],
      description: 'If $seq is an array, creates a new array with the elements from $seq in reversed order. If $seq is a string, returns new reversed string.',
      seeAlso: ['sort'],
      examples: [
        'reverse(["Albert", "Mojir", 160, [1, 2]])',
        'reverse([])',
        'reverse("Albert")',
        'reverse(null)',
      ],
    },
  },
  'second': {
    evaluate: ([seq], sourceCodeInfo): Any => {
      if (seq === null)
        return null

      assertSeq(seq, sourceCodeInfo)
      return toAny(seq[1])
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Sequence',
      returns: { type: 'any' },
      args: { seq: { type: ['sequence', 'null'] } },
      variants: [{ argumentNames: ['seq'] }],
      description: 'Returns the second element of $seq. If $seq has less than two elements or is `null`, `null` is returned.',
      seeAlso: ['first', 'last', 'nth'],
      examples: [
        'second(["Albert", "Mojir", 160, [1, 2]])',
        'second([1])',
        'second([])',
        'second(null)',
      ],
    },
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
    docs: {
      category: 'Sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'integer' },
        seq: { type: 'sequence', rest: true },
        start: { type: 'integer', description: 'Defaults to `0`.' },
        stop: { type: 'integer', description: 'Defaults lenght of sequence + 1.' },
      },
      variants: [
        { argumentNames: ['seq'] },
        { argumentNames: ['seq', 'start'] },
        { argumentNames: ['seq', 'start', 'stop'] },
      ],
      description: 'Returns a copy of a portion of $seq from index $start (inclusive) to $stop (exclusive).',
      seeAlso: ['Sequence-Utils.take', 'Sequence-Utils.drop', 'Sequence-Utils.splice', 'nth'],
      examples: [
        '[1, 2, 3, 4, 5] slice 2',
        'slice([1, 2, 3, 4, 5], 2, 4)',
        'slice([1, 2, 3, 4, 5], 2)',
      ],
    },
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
    docs: {
      category: 'Sequence',
      returns: { type: 'any' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'function' },
        seq: { type: ['sequence', 'null'] },
        fun: { type: 'function' },
      },
      variants: [{ argumentNames: ['seq', 'fun'] }],
      description: 'Returns the first element that passes the test implemented by $fun. I no element was found, `null` is returned.',
      seeAlso: ['Sequence-Utils.position', 'Collection-Utils.any?', 'find'],
      examples: [
        `
some(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
        `
some(
  [5, 10, 15, 20],
  -> $ > 10
)`,
        `
some(
  [1, 2, 3, 4],
  -> $ > 10
)`,
        `
some(
  [],
  -> $ > 10
)`,
        `
some(
  null,
  -> $ > 10
)`,
      ],
    },
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
    docs: {
      category: 'Sequence',
      returns: { type: 'any', rest: true },
      args: {
        a: { type: 'sequence' },
        b: { type: 'function' },
        seq: { type: 'sequence' },
        fun: { type: 'function' },
      },
      variants: [
        { argumentNames: ['seq'] },
        { argumentNames: ['seq', 'fun'] },
      ],
      description: 'Returns a new sequence with the elements from $seq sorted according to $fun. If no $fun is supplied, builtin `compare` will be used.',
      seeAlso: ['Sequence-Utils.sort-by', 'compare', 'reverse', 'Vector.sort-indices'],
      examples: [
        '[3, 1, 2] sort (a, b) -> b - a',
        'sort([3, 1, 2])',
        `
sort(
  [3, 1, 2],
  (a, b) -> cond case a < b then -1 case a > b then 1 case true then -1 end
)`,
        `
sort(
  [3, 1, 2],
  (a, b) -> cond case a > b then -1 case a < b then 1 case true then -1 end
)`,
      ],
    },
  },
}
