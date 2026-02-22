import type { ContextStack } from '../../evaluator/ContextStack'
import type { ExecuteFunction } from '../../evaluator/interface'
import type { Any, Arr, Coll, Obj, Seq } from '../../interface'
import type { SourceCodeInfo } from '../../tokenizer/token'
import { collHasKey, deepEqual, toAny } from '../../utils'
import { asAny, asFunctionLike, assertAny, assertColl, assertFunctionLike, assertObj, assertSeq, isObj, isSeq } from '../../typeGuards/lits'
import type { BuiltinNormalExpressions } from '../interface'
import { assertArray } from '../../typeGuards/array'
import { assertNumber, isNumber } from '../../typeGuards/number'
import { assertString, assertStringOrNumber, isString, isStringOrNumber } from '../../typeGuards/string'
import type { FunctionLike } from '../../parser/types'
import { LitsError } from '../../errors'
import { toFixedArity } from '../../utils/arity'
import type { MaybePromise } from '../../utils/maybePromise'
import { chain, mapSequential, reduceSequential } from '../../utils/maybePromise'

function mapObjects({
  colls,
  contextStack,
  executeFunction,
  fn,
  sourceCodeInfo,
}: {
  colls: unknown[]
  fn: FunctionLike
  sourceCodeInfo: SourceCodeInfo | undefined
  contextStack: ContextStack
  executeFunction: ExecuteFunction
}): MaybePromise<Obj> {
  assertObj(colls[0], sourceCodeInfo)
  const keys = Object.keys(colls[0])
  const params: Record<string, unknown[]> = {}
  colls.forEach((obj) => {
    assertObj(obj, sourceCodeInfo)
    const objKeys = Object.keys(obj)
    if (objKeys.length !== keys.length) {
      throw new LitsError(`All objects must have the same keys. Expected: ${keys.join(', ')}. Found: ${objKeys.join(', ')}`, sourceCodeInfo)
    }
    if (!objKeys.every(key => keys.includes(key))) {
      throw new LitsError(`All objects must have the same keys. Expected: ${keys.join(', ')}. Found: ${objKeys.join(', ')}`, sourceCodeInfo)
    }
    Object.entries(obj).forEach(([key, value]) => {
      if (!params[key])
        params[key] = []
      params[key].push(value)
    })
  })

  const initialObj: Obj = {}
  return reduceSequential(keys, (result: Obj, key) => {
    return chain(
      executeFunction(fn, params[key]!, contextStack, sourceCodeInfo),
      (value) => {
        result[key] = value
        return result
      },
    )
  }, initialObj)
}

function get(coll: Coll, key: string | number): Any | undefined {
  if (isObj(coll)) {
    if (typeof key === 'string' && collHasKey(coll, key))
      return toAny(coll[key])
  }
  else {
    if (isNumber(key, { nonNegative: true, integer: true }) && key >= 0 && key < coll.length)
      return toAny(coll[key])
  }
  return undefined
}

function assoc(coll: Coll, key: string | number, value: Any, sourceCodeInfo?: SourceCodeInfo) {
  assertColl(coll, sourceCodeInfo)
  assertStringOrNumber(key, sourceCodeInfo)
  if (Array.isArray(coll) || typeof coll === 'string') {
    assertNumber(key, sourceCodeInfo, { integer: true })
    assertNumber(key, sourceCodeInfo, { gte: 0 })
    assertNumber(key, sourceCodeInfo, { lte: coll.length })
    if (typeof coll === 'string') {
      assertString(value, sourceCodeInfo, { char: true })
      return `${coll.slice(0, key)}${value}${coll.slice(key + 1)}`
    }
    const copy = [...coll]
    copy[key] = value
    return copy
  }
  assertString(key, sourceCodeInfo)
  const copy = { ...coll }
  copy[key] = value
  return copy
}

export const collectionNormalExpression: BuiltinNormalExpressions = {
  'filter': {
    evaluate: ([coll, fn]: Arr, sourceCodeInfo, contextStack, { executeFunction }): MaybePromise<Coll> => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      if (Array.isArray(coll)) {
        return reduceSequential(coll, (result: Arr, elem) => {
          return chain(executeFunction(fn, [elem], contextStack, sourceCodeInfo), (keep) => {
            if (keep)
              result.push(elem)
            return result
          })
        }, [] as Arr)
      }
      if (isString(coll)) {
        const chars = coll.split('')
        return chain(
          reduceSequential(chars, (result: string[], elem) => {
            return chain(executeFunction(fn, [elem], contextStack, sourceCodeInfo), (keep) => {
              if (keep)
                result.push(elem)
              return result
            })
          }, [] as string[]),
          filtered => filtered.join(''),
        )
      }
      const entries = Object.entries(coll)
      const initialObj: Obj = {}
      return reduceSequential(entries, (result: Obj, [key, value]) => {
        return chain(executeFunction(fn, [value], contextStack, sourceCodeInfo), (keep) => {
          if (keep)
            result[key] = value
          return result
        })
      }, initialObj)
    },
    arity: toFixedArity(2),
    docs: {
      category: 'collection',
      returns: { type: 'collection' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
        coll: { type: 'collection' },
        fun: { type: 'function' },
      },
      variants: [{ argumentNames: ['coll', 'fun'] }],
      description: 'Creates a new collection with all elements that pass the test implemented by $fun.',
      seeAlso: ['collection.filteri', 'map', 'sequence.remove'],
      examples: [
        `
filter(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
        `
filter(
  [5, 10, 15, 20],
  -> $ > 10
)`,
        `
filter(
  { a: 1, b: 2 },
  odd?
)`,
      ],
    },
  },
  'map': {
    evaluate: (params, sourceCodeInfo, contextStack, { executeFunction }) => {
      const fn = asFunctionLike(params.at(-1), sourceCodeInfo)

      if (isObj(params[0])) {
        return mapObjects({
          colls: params.slice(0, -1),
          fn,
          sourceCodeInfo,
          contextStack,
          executeFunction,
        })
      }

      const seqs = params.slice(0, -1) as Seq[]
      assertSeq(seqs[0], sourceCodeInfo)

      const isStr = typeof seqs[0] === 'string'
      let len = seqs[0].length
      seqs.slice(1).forEach((seq) => {
        if (isStr) {
          assertString(seq, sourceCodeInfo)
        }
        else {
          assertArray(seq, sourceCodeInfo)
        }
        len = Math.min(len, seq.length)
      })

      const paramArray: unknown[][] = []
      for (let i = 0; i < len; i++) {
        paramArray.push(seqs.map(seq => seq[i]))
      }

      const mapped = mapSequential(paramArray, p => executeFunction(fn, p, contextStack, sourceCodeInfo))

      if (!isStr) {
        return mapped
      }
      return chain(mapped, (resolvedMapped) => {
        resolvedMapped.forEach(char => assertString(char, sourceCodeInfo))
        return resolvedMapped.join('')
      })
    },
    arity: { min: 2 },
    docs: {
      category: 'collection',
      returns: { type: 'collection' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
        colls: { type: 'collection', rest: true, description: 'At least one.' },
        fun: { type: 'function' },
      },
      variants: [{ argumentNames: ['colls', 'fun'] }],
      description: 'Creates a new collection populated with the results of calling $fun on every element in $colls.',
      seeAlso: ['collection.mapi', 'filter', 'reduce', 'mapcat', 'grid.map', 'grid.mapi'],
      examples: [
        '[1, 2, 3] map -',
        '[1, 2, 3] map -> -($)',
        'map(["Albert", "Mojir", 42], str)',
        'map([1, 2, 3], inc)',
        'map([1, 2, 3], [1, 10, 100], *)',
        'map({ a: 1, b: 2 }, inc)',
        'map({ a: 1, b: 2 }, { a: 10, b: 20 }, +)',
      ],
    },
  },
  'reduce': {
    evaluate: ([coll, fn, initial], sourceCodeInfo, contextStack, { executeFunction }): MaybePromise<Any> => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      assertAny(initial, sourceCodeInfo)

      if (typeof coll === 'string') {
        assertString(initial, sourceCodeInfo)
        if (coll.length === 0)
          return initial

        return reduceSequential(coll.split(''), (result: Any, elem) => {
          return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
        }, initial)
      }
      else if (Array.isArray(coll)) {
        if (coll.length === 0)
          return initial

        return reduceSequential(coll, (result: Any, elem) => {
          return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
        }, initial)
      }
      else {
        if (Object.keys(coll).length === 0)
          return initial

        return reduceSequential(Object.entries(coll), (result: Any, [, elem]) => {
          return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
        }, initial)
      }
    },
    arity: toFixedArity(3),
    docs: {
      category: 'collection',
      returns: { type: 'any' },
      args: {
        fun: { type: 'function' },
        coll: { type: 'collection' },
        initial: { type: 'any' },
      },
      variants: [{ argumentNames: ['coll', 'fun', 'initial'] }],
      description: 'Runs $fun function on each element of the $coll, passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $coll is a single value.',
      seeAlso: ['collection.reduce-right', 'collection.reducei', 'collection.reductions', 'map', 'grid.reduce', 'grid.reducei'],
      examples: [
        'reduce([1, 2, 3], +, 0)',
        'reduce([], +, 0)',
        'reduce({ a: 1, b: 2 }, +, 0)',
        `
reduce(
  [1, 2, 3, 4, 5, 6, 7, 8, 9],
  (result, value) -> result + (even?(value) ? value : 0),
  0)`,
      ],
    },
  },
  'get': {
    evaluate: (params, sourceCodeInfo) => {
      const [coll, key] = params
      const defaultValue = toAny(params[2])
      assertStringOrNumber(key, sourceCodeInfo)
      if (coll === null)
        return defaultValue

      assertColl(coll, sourceCodeInfo)
      const result = get(coll, key)
      return result === undefined ? defaultValue : result
    },
    arity: { min: 2, max: 3 },
    docs: {
      category: 'collection',
      returns: { type: 'any' },
      args: {
        'a': { type: 'collection' },
        'b': { type: ['string', 'integer'] },
        'not-found': { type: 'any', description: 'Default value to return if $b is not found.' },
      },
      variants: [
        { argumentNames: ['a', 'b'] },
        { argumentNames: ['a', 'b', 'not-found'] },
      ],
      description: 'Returns value in $a mapped at $b.',
      seeAlso: ['collection.get-in', 'contains?', 'find', 'nth'],
      examples: [
        '[1, 2, 3] get 1',
        '{ a: 1 } get "a"',
        '"Albert" get "3"',
        `
get(
  [1, 2, 3],
  1, // Optional comma after last argument
)`,
        `
get(
  [],
  1
)`,
        `
get(
  [],
  1,
  "default"
)`,
        `
get(
  { a: 1 },
  "a"
)`,
        `
get(
  { a: 1 },
  "b"
)`,
        `
get(
  { a: 1 },
  "b",
  "default"
)`,
        `
get(
  null,
  "a"
)`,
        `
get(
  null,
  "b",
  "default"
)`,
      ],
    },
  },
  'count': {
    evaluate: ([coll], sourceCodeInfo): number => {
      if (coll === null)
        return 0

      if (typeof coll === 'string')
        return coll.length

      assertColl(coll, sourceCodeInfo)
      if (Array.isArray(coll))
        return coll.length

      return Object.keys(coll).length
    },
    arity: toFixedArity(1),
    docs: {
      category: 'collection',
      returns: { type: 'number' },
      args: {
        coll: { type: ['collection', 'null'] },
      },
      variants: [{ argumentNames: ['coll'] }],
      description: 'Returns number of elements in $coll.',
      seeAlso: ['empty?'],
      examples: [
        'count([1, 2, 3])',
        'count([])',
        'count({ a: 1 })',
        'count("")',
        'count("Albert")',
        'count(null)',
      ],
    },
  },
  'contains?': {
    evaluate: ([coll, key], sourceCodeInfo): boolean => {
      if (coll === null)
        return false

      assertColl(coll, sourceCodeInfo)
      if (isString(coll)) {
        assertString(key, sourceCodeInfo)
        return coll.includes(key)
      }
      if (isSeq(coll)) {
        assertAny(key, sourceCodeInfo)
        return !!coll.find(elem => deepEqual(asAny(elem), key, sourceCodeInfo))
      }
      assertString(key, sourceCodeInfo)
      return key in coll
    },
    arity: toFixedArity(2),
    docs: {
      category: 'collection',
      returns: { type: 'boolean' },
      args: {
        a: { type: ['collection', 'null'] },
        b: { type: ['string', 'integer'] },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns `true` if $a contains $b, otherwise returns `false`. For strings, it checks if substring is included.',
      seeAlso: ['get', 'find', 'index-of'],
      examples: [
        '[1, 2, 3] contains? 1',
        'null contains? 1',
        '{ a: 1, b: 2 } contains? "a"',
        `
contains?(
  [],
  1
)`,
        `
contains?(
  [1],
  1
)`,
        `
contains?(
  [1, 2, 3],
  1
)`,
        `
contains?(
  {},
  "a"
)`,
        `
contains?(
  { a: 1, b: 2 },
  "a"
)`,
      ],
    },
  },
  'assoc': {
    evaluate: ([coll, key, value], sourceCodeInfo): Coll => {
      assertColl(coll, sourceCodeInfo)
      assertStringOrNumber(key, sourceCodeInfo)
      assertAny(value, sourceCodeInfo)
      return assoc(coll, key, value, sourceCodeInfo)
    },
    arity: toFixedArity(3),
    docs: {
      category: 'collection',
      returns: { type: 'collection' },
      args: {
        coll: { type: 'collection' },
        key: { type: ['string', 'number'] },
        value: { type: 'any' },
        kvs: { type: 'any', description: 'Key-value pairs to associate.', rest: true },
      },
      variants: [
        { argumentNames: ['coll', 'key', 'value'] },
        { argumentNames: ['coll', 'key', 'value', 'kvs'] },
      ],
      description: `
Add or replace the value of element $key to $value in $coll. Repeated for all key-value pairs in $kvs.
If $coll is an 'array', $key must be \`number\` satisfying \`0 <=\` $key \`<= length\`.`,
      seeAlso: ['collection.assoc-in', 'dissoc', 'merge', 'collection.update'],
      examples: [
        `
assoc(
  [1, 2, 3],
  1,
  "Two"
)`,
        `
assoc(
  [1, 2, 3],
  3,
  "Four"
)`,
        `
assoc(
  { a: 1, b: 2 },
  "a",
  "One")`,
        `
assoc(
  { a: 1, b: 2 },
  "c",
  "Three")`,
        `
assoc(
  "Albert",
  6,
  "a")`,
      ],
    },
  },
  '++': {
    evaluate: (params, sourceCodeInfo): Any => {
      if (!isNumber(params[0])) {
        assertColl(params[0], sourceCodeInfo)
      }
      if (Array.isArray(params[0])) {
        return params.reduce((result: Arr, arr) => {
          assertArray(arr, sourceCodeInfo)
          return result.concat(arr)
        }, [])
      }
      else if (isStringOrNumber(params[0])) {
        return params.reduce((result: string, s) => {
          assertStringOrNumber(s, sourceCodeInfo)
          return `${result}${s}`
        }, '')
      }
      else {
        return params.reduce((result: Obj, obj) => {
          assertObj(obj, sourceCodeInfo)
          return Object.assign(result, obj)
        }, {})
      }
    },
    arity: { min: 1 },
    docs: {
      category: 'collection',
      returns: { type: 'collection' },
      args: {
        a: { type: 'collection' },
        b: { type: 'collection' },
        colls: { type: 'collection', rest: true },
      },
      variants: [
        { argumentNames: ['a'] },
        { argumentNames: ['a', 'colls'] },
      ],
      description: 'Concatenates collections into one collection.',
      seeAlso: ['mapcat', 'str', 'join', 'push', 'sequence.unshift'],
      examples: [
        '"Albert" ++ " " ++ "Mojir"',
        '"Albert" ++ "Mojir"',

        '++("Albert", "-", "Mojir")',
        '++("Albert")',

        '++("A", "l", "b", "e", "r", "t")',
        '++([1, 2], [3, 4])',
        '++([], [3, 4])',
        '++([1, 2], [])',
        '++([1, 2], [3, 4], [5, 6])',
        '++([])',
        '++({ a: 1, b: 2 }, { b: 1, c: 2 })',
        '++({}, { a: 1 })',
      ],
    },
  },
}
