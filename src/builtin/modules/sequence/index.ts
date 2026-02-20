import type { Any, Arr, Obj, Seq } from '../../../interface'
import type { SourceCodeInfo } from '../../../tokenizer/token'
import { asArray, assertArray, assertCharArray } from '../../../typeGuards/array'
import { asAny, asSeq, assertAny, assertFunctionLike, assertSeq } from '../../../typeGuards/lits'
import { asNumber, assertNumber } from '../../../typeGuards/number'
import { assertString, assertStringOrNumber } from '../../../typeGuards/string'
import { collHasKey, compare, deepEqual, toNonNegativeInteger } from '../../../utils'
import { toFixedArity } from '../../../utils/arity'
import type { BuiltinNormalExpressions } from '../../interface'
import type { LitsModule } from '../interface'

const sequenceUtilsFunctions: BuiltinNormalExpressions = {
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
    docs: {
      category: 'sequence',
      returns: { type: ['number', 'null'] },
      args: {
        a: { type: 'sequence' },
        b: { type: 'function' },
        seq: { type: ['sequence', 'null'] },
        fun: { type: 'function' },
      },
      variants: [{ argumentNames: ['seq', 'fun'] }],
      description: 'Returns the index of the first elements that passes the test implemented by $fun. If no element was found, `null` is returned.',
      seeAlso: ['index-of', 'some', 'find'],
      examples: [
        `
let su = import("sequence");
su.position(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
        `
let su = import("sequence");
su.position(
  [5, 10, 15, 20],
  -> $ > 10
)`,
        `
let su = import("sequence");
su.position(
  [5, 10, 15, 20],
  -> $ > 100
)`,
        `
let su = import("sequence");
su.position(
  null,
  -> $ > 100
)`,
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: ['number', 'null'] },
      args: {
        a: { type: 'sequence' },
        b: { type: 'any' },
        seq: { type: ['sequence', 'null'] },
        x: { type: 'any' },
      },
      variants: [{ argumentNames: ['seq', 'x'] }],
      description: 'Returns the last index of $x in $seq. If element is not present in $seq `null` is returned.',
      seeAlso: ['index-of'],
      examples: [
        'let su = import("sequence"); su.last-index-of([[1], [2], [1], [2]], [1])',
        'let su = import("sequence"); su.last-index-of(["Albert", "Mojir", 160, [1, 2]], "Mojir")',
        'let su = import("sequence"); su.last-index-of([5, 10, 15, 20, 15], 15)',
        'let su = import("sequence"); su.last-index-of([5, 10, 15, 20], 1)',
        'let su = import("sequence"); su.last-index-of(null, 1)',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: ['sequence', 'null'] },
      args: { seq: { type: 'sequence' } },
      variants: [{ argumentNames: ['seq'] }],
      description: 'Returns a copy of $seq with first element removed. If $seq is empty `null` is returned.',
      seeAlso: ['sequence.unshift', 'pop', 'rest'],
      examples: [
        'let su = import("sequence"); su.shift([1, 2, 3])',
        'let su = import("sequence"); su.shift([])',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        seq: { type: 'sequence', rest: true },
        start: { type: 'integer' },
        deleteCount: { type: 'integer' },
        items: { type: 'any', rest: true },
      },
      variants: [
        { argumentNames: ['seq', 'start', 'deleteCount'] },
        { argumentNames: ['seq', 'start', 'deleteCount', 'items'] },
      ],
      description: 'Returns a a spliced array. Removes $deleteCount elements from $seq starting at $start and replaces them with $items. If $start is negative, it is counting from the end of the array.',
      seeAlso: ['slice', 'sequence.remove-at'],
      examples: [
        'let su = import("sequence"); su.splice([1, 2, 3, 4, 5], 2, 2, "x")',
        'let su = import("sequence"); su.splice([1, 2, 3, 4, 5], -2, 1, "x")',
        'let su = import("sequence"); su.splice("Albert", 2, 2, "fo")',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'any', rest: true },
      args: {
        a: { type: 'sequence' },
        b: { type: 'function' },
        seq: { type: 'sequence' },
        keyfn: { type: 'function' },
        comparer: { type: 'function' },
      },
      variants: [
        { argumentNames: ['seq', 'keyfn'] },
        { argumentNames: ['seq', 'keyfn', 'comparer'] },
      ],
      description: 'Returns a sorted sequence of the items in $seq, where the sort order is determined by comparing `(keyfn item)`. If no $comparer is supplied, uses builtin `compare`.',
      seeAlso: ['sort', 'compare'],
      examples: [
        'let su = import("sequence"); su.sort-by(["Albert", "Mojir", "Nina"], count)',
        'let su = import("sequence"); su.sort-by(["Albert", "Mojir", "Nina"], count)',
        'let su = import("sequence"); su.sort-by("Albert", lower-case, -> $2 compare $1)',
      ],
    },
  },
  'take': {
    evaluate: ([input, n], sourceCodeInfo): Seq => {
      assertNumber(n, sourceCodeInfo)
      assertSeq(input, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)
      return input.slice(0, num)
    },
    arity: toFixedArity(2),
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'integer' },
        n: { type: 'integer' },
        seq: { type: 'sequence' },
      },
      variants: [{ argumentNames: ['seq', 'n'] }],
      description: 'Constructs a new array/string with the $n first elements from $seq.',
      seeAlso: ['sequence.take-last', 'sequence.take-while', 'sequence.drop', 'slice', 'sequence.split-at'],
      examples: [
        'let su = import("sequence"); su.take([1, 2, 3, 4, 5], 3)',
        'let su = import("sequence"); su.take([1, 2, 3, 4, 5], 3)',
        'let su = import("sequence"); su.take([1, 2, 3, 4, 5], 0)',
        'let su = import("sequence"); su.take("Albert", 2)',
        'let su = import("sequence"); su.take("Albert", 50)',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'integer' },
        n: { type: 'integer' },
        seq: { type: 'sequence' },
      },
      variants: [{ argumentNames: ['n', 'seq'] }],
      description: 'Constructs a new array with the $n last elements from $seq.',
      seeAlso: ['sequence.take', 'sequence.drop-last'],
      examples: [
        'let su = import("sequence"); su.take-last([1, 2, 3, 4, 5], 3)',
        'let su = import("sequence"); su.take-last([1, 2, 3, 4, 5], 3)',
        'let su = import("sequence"); su.take-last([1, 2, 3, 4, 5], 0)',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'function' },
        seq: { type: 'sequence' },
        fun: { type: 'function' },
      },
      variants: [{ argumentNames: ['seq', 'fun'] }],
      description: 'Returns the members of $seq in order, stopping before the first one for which `predicate` returns a falsy value.',
      seeAlso: ['sequence.take', 'sequence.drop-while', 'sequence.split-with'],
      examples: [
        `
let su = import("sequence");
su.take-while(
  [1, 2, 3, 2, 1],
  -> $ < 3
)`,
        `
let su = import("sequence");
su.take-while(
  [1, 2, 3, 2, 1],
  -> $ > 3
)`,
      ],
    },
  },
  'drop': {
    evaluate: ([input, n], sourceCodeInfo): Seq => {
      assertNumber(n, sourceCodeInfo)
      const num = Math.max(Math.ceil(n), 0)
      assertSeq(input, sourceCodeInfo)
      return input.slice(num)
    },
    arity: toFixedArity(2),
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'integer' },
        seq: { type: 'sequence' },
        n: { type: 'integer' },
      },
      variants: [{ argumentNames: ['seq', 'n'] }],
      description: 'Constructs a new array/string with the $n first elements dropped from $seq.',
      seeAlso: ['sequence.drop-last', 'sequence.drop-while', 'sequence.take', 'slice', 'sequence.split-at'],
      examples: [
        'let su = import("sequence"); su.drop([1, 2, 3, 4, 5], 3)',
        'let su = import("sequence"); su.drop([1, 2, 3, 4, 5], 0)',
        'let su = import("sequence"); su.drop("Albert", 2)',
        'let su = import("sequence"); su.drop("Albert", 50)',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'integer' },
        seq: { type: 'sequence' },
        n: { type: 'integer' },
      },
      variants: [{ argumentNames: ['seq', 'n'] }],
      description: 'Constructs a new array with the $n last elements dropped from $seq.',
      seeAlso: ['sequence.drop', 'sequence.take-last'],
      examples: [
        'let su = import("sequence"); su.drop-last([1, 2, 3, 4, 5], 3)',
        'let su = import("sequence"); su.drop-last([1, 2, 3, 4, 5], 3)',
        'let su = import("sequence"); su.drop-last([1, 2, 3, 4, 5], 0)',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'function' },
        seq: { type: 'sequence' },
        fun: { type: 'function' },
      },
      variants: [{ argumentNames: ['seq', 'fun'] }],
      description: 'Returns the members of $seq in order, skipping the fist elements for witch the `predicate` returns a truethy value.',
      seeAlso: ['sequence.drop', 'sequence.take-while', 'sequence.split-with'],
      examples: [
        `
let su = import("sequence");
su.drop-while(
  [1, 2, 3, 2, 1],
  -> $ < 3
)`,
        `
let su = import("sequence");
su.drop-while(
  [1, 2, 3, 2, 1],
  -> $ > 3
)`,
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'any' },
        seq: { type: 'sequence' },
        values: { type: 'any', rest: true },
      },
      variants: [{ argumentNames: ['seq', 'values'] }],
      description: 'Returns copy of $seq with $values added to the beginning.',
      seeAlso: ['push', 'sequence.shift', '++'],
      examples: [
        'let su = import("sequence"); su.unshift([1, 2, 3], 4)',
        'let su = import("sequence"); su.unshift([1, 2, 3], 4)',
        'let su = import("sequence"); su.unshift([1, 2, 3], 4, 5, 6)',
        `
let su = import("sequence");
let l = [1, 2, 3];
su.unshift(l, 4);
l`,
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: { seq: { type: 'sequence' } },
      variants: [{ argumentNames: ['seq'] }],
      description: 'Returns a copy of $seq with no duplicates.',
      seeAlso: ['sequence.frequencies'],
      examples: [
        'let su = import("sequence"); su.distinct([[1], [2], [3], [1], [3], [5]])',
        'let su = import("sequence"); su.distinct([1, 2, 3, 1, 3, 5])',
        'let su = import("sequence"); su.distinct("Albert Mojir")',
        'let su = import("sequence"); su.distinct([])',
        'let su = import("sequence"); su.distinct("")',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'function' },
        seq: { type: 'sequence' },
        fun: { type: 'function' },
      },
      variants: [{ argumentNames: ['seq', 'fun'] }],
      description: 'Returns a new sequence of items in $seq for witch `pred(item)` returns a falsy value.',
      seeAlso: ['filter', 'sequence.remove-at'],
      examples: [
        'let su = import("sequence"); su.remove([1, 2, 3, 1, 3, 5], odd?)',
        'let su = import("sequence"); su.remove([1, 2, 3, 1, 3, 5], even?)',
        'let su = import("sequence"); su.remove("Albert Mojir", -> "aoueiyAOUEIY" contains? $)',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'integer' },
        seq: { type: 'sequence' },
        n: { type: 'number' },
      },
      variants: [{ argumentNames: ['seq', 'n'] }],
      description: 'Returns a new sequence of all items in $seq except item at position $n. If $n is negative, it is counting from the end of the sequence.',
      seeAlso: ['sequence.remove', 'sequence.splice'],
      examples: [
        'let su = import("sequence"); su.remove-at([1, 2, 3, 1, 3, 5], 2)',
        'let su = import("sequence"); su.remove-at("Albert", -2)',
        'let su = import("sequence"); su.remove-at([1, 2, 3, 1, 3, 5], 0)',
        'let su = import("sequence"); su.remove-at([1, 2, 3, 1, 3, 5], -1)',
        'let su = import("sequence"); su.remove-at("Albert Mojir", 6)',
      ],
    },
  },
  'split-at': {
    evaluate: ([seq, pos], sourceCodeInfo): Seq => {
      assertNumber(pos, sourceCodeInfo, { integer: true })
      assertSeq(seq, sourceCodeInfo)

      const at = pos < 0 ? seq.length + pos : pos
      return [seq.slice(0, at), seq.slice(at)]
    },
    arity: toFixedArity(2),
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'integer' },
        seq: { type: 'sequence' },
        n: { type: 'number' },
      },
      variants: [{ argumentNames: ['seq', 'n'] }],
      description: 'Returns a pair of sequence `[take(pos input), drop(pos input)]`.',
      seeAlso: ['sequence.split-with', 'sequence.take', 'sequence.drop'],
      examples: [
        'let su = import("sequence"); su.split-at([1, 2, 3, 4, 5], 2)',
        'let su = import("sequence"); su.split-at("Albert", -2)',
        'let su = import("sequence"); su.split-at([1, 2, 3, 4, 5], -2)',
        'let su = import("sequence"); su.split-at("Albert", 2)',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'function' },
        seq: { type: 'sequence' },
        fun: { type: 'function' },
      },
      variants: [{ argumentNames: ['seq', 'fun'] }],
      description: 'Returns a pair of sequences `[take-while(input, fun), drop-while(input, fun)]`.',
      seeAlso: ['sequence.split-at', 'sequence.take-while', 'sequence.drop-while'],
      examples: [
        'let su = import("sequence"); su.split-with([1, 2, 3, 4, 5], odd?)',
        'let su = import("sequence"); su.split-with([1, 2, 3, 4, 5], -> $ > 3)',
        'let su = import("sequence"); su.split-with("Albert", -> $ <= "o")',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'object' },
      args: { seq: { type: 'sequence' } },
      variants: [{ argumentNames: ['seq'] }],
      description: 'Returns an object from distinct items in $seq to the number of times they appear. Note that all items in $seq must be valid object keys i.e. strings.',
      seeAlso: ['sequence.group-by', 'sequence.distinct', 'vector.count-values'],
      examples: [
        'let su = import("sequence"); su.frequencies(["Albert", "Mojir", "Nina", "Mojir"])',
        'let su = import("sequence"); su.frequencies("Pneumonoultramicroscopicsilicovolcanoconiosis")',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'object' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'function' },
        seq: { type: 'sequence' },
        fun: { type: 'function' },
      },
      variants: [{ argumentNames: ['seq', 'fun'] }],
      description: 'Returns an object of the elements of $seq keyed by the result of $fun on each element. The value at each key will be an array of the corresponding elements.',
      seeAlso: ['sequence.frequencies', 'sequence.partition-by'],
      examples: [
        'let su = import("sequence"); su.group-by([{ name: "Albert" }, { name: "Albert" }, { name: "Mojir" }], "name")',
        'let su = import("sequence"); su.group-by([{name: "Albert"}, {name: "Albert"}, {name: "Mojir"}], "name")',
        'let su = import("sequence"); su.group-by("Albert Mojir", -> "aoueiAOUEI" contains? $ ? "vowel" : "other")',
      ],
    },
  },

  'partition': {
    evaluate: (params, sourceCodeInfo): Seq => {
      const seq = asSeq(params[0], sourceCodeInfo)
      const n = toNonNegativeInteger(asNumber(params[1], sourceCodeInfo))
      const step = params.length >= 3 ? toNonNegativeInteger(asNumber(params[2], sourceCodeInfo)) : n
      const pad = params.length === 4
        ? params[3] === null ? [] : asArray(params[3], sourceCodeInfo)
        : undefined

      return partitionHelper(n, step, seq, pad, sourceCodeInfo)
    },
    arity: { min: 2, max: 4 },
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'number' },
        seq: { type: 'sequence' },
        n: { type: 'number' },
        step: { type: 'number' },
        pad: { type: 'array' },
      },
      variants: [
        { argumentNames: ['seq', 'n'] },
        { argumentNames: ['seq', 'n', 'step'] },
        { argumentNames: ['seq', 'n', 'step', 'pad'] },
      ],
      description: 'Returns an array of sequences of $n items each, at offsets $step apart. If $step is not supplied, defaults to $n. If a $pad array is supplied, use its elements as necessary to complete last partition upto $n items. In case there are not enough padding elements, return a partition with less than $n items.',
      seeAlso: ['sequence.partition-all', 'sequence.partition-by'],
      examples: [
        'let su = import("sequence"); su.partition(range(20), 4)',
        'let su = import("sequence"); su.partition(range(20), 4)',
        'let su = import("sequence"); su.partition(range(22), 4)',
        'let su = import("sequence"); su.partition(range(20), 4, 6)',
        'let su = import("sequence"); su.partition(range(20), 4, 3)',
        'let su = import("sequence"); su.partition(range(20), 3, 6, ["a"])',
        'let su = import("sequence"); su.partition(range(20), 4, 6, ["a"])',
        'let su = import("sequence"); su.partition(range(20), 4, 6, ["a", "b", "c", "d"])',
        'let su = import("sequence"); su.partition(["a", "b", "c", "d", "e", "f"], 3, 1)',
        'let su = import("sequence"); su.partition([1, 2, 3, 4], 10)',
        'let su = import("sequence"); su.partition([1, 2, 3, 4], 10, 10)',
        'let su = import("sequence"); su.partition([1, 2, 3, 4], 10, 10, [])',
        'let su = import("sequence"); su.partition([1, 2, 3, 4], 10, 10, null)',
        'let su = import("sequence"); su.partition("superfragilistic", 5)',
        'let su = import("sequence"); su.partition("superfragilistic", 5, 5, null)',
        'let su = import("sequence"); let foo = [5, 6, 7, 8]; su.partition(foo, 2, 1, foo)',
      ],
    },
  },

  'partition-all': {
    evaluate: (params, sourceCodeInfo): Seq => {
      const seq = asSeq(params[0], sourceCodeInfo)
      const n = toNonNegativeInteger(asNumber(params[1], sourceCodeInfo))
      const step = params.length === 3 ? toNonNegativeInteger(asNumber(params[2], sourceCodeInfo)) : n

      return partitionHelper(n, step, seq, [], sourceCodeInfo)
    },
    arity: { min: 2, max: 3 },
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'number' },
        seq: { type: 'sequence' },
        n: { type: 'number' },
        step: { type: 'number' },
      },
      variants: [
        { argumentNames: ['seq', 'n'] },
        { argumentNames: ['seq', 'n', 'step'] },
      ],
      description: 'Returns an array of sequences like partition, but may include partitions with fewer than n items at the end.',
      seeAlso: ['sequence.partition', 'sequence.partition-by'],
      examples: [
        'let su = import("sequence"); su.partition-all([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 4)',
        'let su = import("sequence"); su.partition-all([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 4)',
        'let su = import("sequence"); su.partition([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 4)',
        'let su = import("sequence"); su.partition-all([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 2, 4)',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'function' },
        seq: { type: 'sequence' },
        fun: { type: 'function' },
      },
      variants: [{ argumentNames: ['seq', 'fun'] }],
      description: 'Applies $fun to each value in $seq, splitting it each time $fun returns a new value. Returns an array of sequences.',
      seeAlso: ['sequence.partition', 'sequence.partition-all', 'sequence.group-by'],
      examples: [
        'let su = import("sequence"); su.partition-by([1, 2, 3, 4, 5], odd?)',
        'let su = import("sequence"); su.partition-by([1, 2, 3, 4, 5], -> $ == 3)',
        'let su = import("sequence"); su.partition-by([1, 1, 1, 2, 2, 3, 3], odd?)',
        'let su = import("sequence"); su.partition-by("Leeeeeerrroyyy", identity)',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'sequence' },
        seq: { type: 'sequence' },
        suffix: { type: 'sequence' },
      },
      variants: [{ argumentNames: ['seq', 'suffix'] }],
      description: 'Returns `true` if $seq ends with $suffix, otherwise `false`.',
      seeAlso: ['sequence.starts-with?'],
      examples: [
        'let su = import("sequence"); su.ends-with?([[1], [2], [3], [4], [5]], [5])',
        'let su = import("sequence"); su.ends-with?([[1], [2], [3], [4], [5]], 5)',
        'let su = import("sequence"); su.ends-with?([1, 2, 3, 4, 5], 5)',
        'let su = import("sequence"); su.ends-with?([1, 2, 3, 4, 5], [5])',
        'let su = import("sequence"); su.ends-with?("Albert", "rt")',
        'let su = import("sequence"); su.ends-with?("Albert", "RT")',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'sequence' },
        seq: { type: 'sequence' },
        prefix: { type: 'sequence' },
      },
      variants: [{ argumentNames: ['seq', 'prefix'] }],
      description: 'Returns `true` if $seq starts with $prefix, otherwise `false`.',
      seeAlso: ['sequence.ends-with?'],
      examples: [
        'let su = import("sequence"); su.starts-with?([[1], [2], [3], [4], [5]], [1])',
        'let su = import("sequence"); su.starts-with?([1, 2, 3, 4, 5], 1)',
        'let su = import("sequence"); su.starts-with?([1, 2, 3, 4, 5], [1])',
        'let su = import("sequence"); su.starts-with?("Albert", "Al")',
        'let su = import("sequence"); su.starts-with?("Albert", "al")',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'sequence' },
        seqs: { type: 'sequence', rest: true },
      },
      variants: [{ argumentNames: ['seqs'] }],
      description: 'Returns a sequence of the first item from each of the $seqs, then the second item from each of the $seqs, until all items from the shortest seq are exhausted.',
      seeAlso: ['sequence.interpose', 'zipmap'],
      examples: [
        'let su = import("sequence"); su.interleave([1, 2, 3], [4, 5, 6])',
        'let su = import("sequence"); su.interleave("Albert", ".,.,.,")',
        'let su = import("sequence"); su.interleave([1, 2, 3], [4, 5, 6])',
        'let su = import("sequence"); su.interleave([1, 2, 3], [4, 5, 6], [7, 8, 9])',
        'let su = import("sequence"); su.interleave([1, 2, 3], [4, 5, 6], [7, 8])',
        'let su = import("sequence"); su.interleave([1, 2, 3], [4, 5, 6], [7])',
        'let su = import("sequence"); su.interleave([1, 2, 3], [4, 5, 6], [])',
        'let su = import("sequence"); su.interleave([1, 2, 3], [])',
        'let su = import("sequence"); su.interleave([])',
      ],
    },
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
    docs: {
      category: 'sequence',
      returns: { type: 'sequence' },
      args: {
        a: { type: 'sequence' },
        b: { type: 'any' },
        seq: { type: 'sequence' },
        separator: { type: 'any' },
      },
      variants: [{ argumentNames: ['seq', 'separator'] }],
      description: 'Returns a sequence of the elements of $seq separated by $separator. If $seq is a string, the separator must be a string.',
      seeAlso: ['sequence.interleave', 'join'],
      examples: [
        'let su = import("sequence"); su.interpose("Albert", "-")',
        'let su = import("sequence"); su.interpose([1, 2, 3, 4, 5], "a")',
        'let su = import("sequence"); su.interpose(["Albert", "Mojir", "Nina"], ", ")',
        'let su = import("sequence"); su.interpose("Albert", ".")',
      ],
    },
  },
}

function partitionHelper(n: number, step: number, seq: Seq, pad: Arr | undefined, sourceCodeInfo?: SourceCodeInfo) {
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

export const sequenceUtilsModule: LitsModule = {
  name: 'sequence',
  functions: sequenceUtilsFunctions,
}
