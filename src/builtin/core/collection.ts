import type { ContextStack } from '../../evaluator/ContextStack'
import type { ExecuteFunction } from '../../evaluator/interface'
import type { Any, Arr, Coll, Obj, Seq } from '../../interface'
import type { SourceCodeInfo } from '../../tokenizer/token'
import { cloneColl, collHasKey, deepEqual, toAny, toNonNegativeInteger } from '../../utils'
import { asAny, asColl, asFunctionLike, assertAny, assertColl, assertFunctionLike, assertObj, assertSeq, isColl, isObj, isSeq } from '../../typeGuards/lits'
import type { BuiltinNormalExpressions } from '../interface'
import { assertArray } from '../../typeGuards/array'
import { assertNumber, isNumber } from '../../typeGuards/number'
import { asString, asStringOrNumber, assertString, assertStringOrNumber, isString, isStringOrNumber } from '../../typeGuards/string'
import type { FunctionLike } from '../../parser/types'
import { LitsError } from '../../errors'
import { toFixedArity } from '../../utils/arity'

interface CollMeta {
  coll: Coll
  parent: Obj | Arr
}

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
}): Obj {
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

  return keys.reduce((result: Obj, key) => {
    result[key] = executeFunction(fn, params[key]!, contextStack, sourceCodeInfo)
    return result
  }, {})
}

function cloneAndGetMeta(
  originalColl: Coll,
  keys: Arr,
  sourceCodeInfo?: SourceCodeInfo,
): { coll: Coll, innerCollMeta: CollMeta } {
  const coll = cloneColl(originalColl)

  const butLastKeys = keys.slice(0, keys.length - 1)

  const innerCollMeta = butLastKeys.reduce(
    (result: CollMeta, key) => {
      const resultColl = result.coll

      let newResultColl: Coll
      if (Array.isArray(resultColl)) {
        assertNumber(key, sourceCodeInfo)
        newResultColl = asColl(resultColl[key], sourceCodeInfo)
      }
      else {
        assertObj(resultColl, sourceCodeInfo)
        assertString(key, sourceCodeInfo)
        if (!collHasKey(result.coll, key))
          resultColl[key] = {}

        newResultColl = asColl(resultColl[key], sourceCodeInfo)
      }

      return { coll: newResultColl, parent: resultColl }
    },
    { coll, parent: {} },
  )
  return { coll, innerCollMeta }
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

function update(
  coll: Coll,
  key: string | number,
  fn: FunctionLike,
  params: Arr,
  contextStack: ContextStack,
  executeFunction: ExecuteFunction,
  sourceCodeInfo?: SourceCodeInfo,
): Coll {
  if (isObj(coll)) {
    assertString(key, sourceCodeInfo)
    const result = { ...coll }
    result[key] = executeFunction(fn, [result[key], ...params], contextStack, sourceCodeInfo)
    return result
  }
  else {
    assertNumber(key, sourceCodeInfo)
    const intKey = toNonNegativeInteger(key)
    assertNumber(intKey, sourceCodeInfo, { lte: coll.length })
    if (Array.isArray(coll)) {
      const result = coll.map((elem, index) => {
        if (intKey === index)
          return executeFunction(fn, [elem, ...params], contextStack, sourceCodeInfo)

        return elem
      })
      if (intKey === coll.length)
        result[intKey] = executeFunction(fn, [undefined, ...params], contextStack, sourceCodeInfo)

      return result
    }
    else {
      const result = coll.split('').map((elem, index) => {
        if (intKey === index) {
          return asString(executeFunction(fn, [elem, ...params], contextStack, sourceCodeInfo), sourceCodeInfo, {
            char: true,
          })
        }
        return elem
      })
      if (intKey === coll.length) {
        result[intKey] = asString(
          executeFunction(fn, [undefined, ...params], contextStack, sourceCodeInfo),
          sourceCodeInfo,
          {
            char: true,
          },
        )
      }
      return result.join('')
    }
  }
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
    evaluate: ([coll, fn]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Coll => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      if (Array.isArray(coll)) {
        const result = coll.filter(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
        return result
      }
      if (isString(coll)) {
        return coll
          .split('')
          .filter(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
          .join('')
      }
      return Object.entries(coll)
        .filter(([, value]) => executeFunction(fn, [value], contextStack, sourceCodeInfo))
        .reduce((result: Obj, [key, value]) => {
          result[key] = value
          return result
        }, {})
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Collection',
      returns: { type: 'collection' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
        coll: { type: 'collection' },
        fun: { type: 'function' },
      },
      variants: [{ argumentNames: ['coll', 'fun'] }],
      description: 'Creates a new collection with all elements that pass the test implemented by $fun.',
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
  'filteri': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): Coll => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      if (Array.isArray(coll)) {
        const result = coll.filter((elem, index) => executeFunction(fn, [elem, index], contextStack, sourceCodeInfo))
        return result
      }
      if (isString(coll)) {
        return coll
          .split('')
          .filter((elem, index) => executeFunction(fn, [elem, index], contextStack, sourceCodeInfo))
          .join('')
      }
      return Object.entries(coll)
        .filter(([key, value]) => executeFunction(fn, [value, key], contextStack, sourceCodeInfo))
        .reduce((result: Obj, [key, value]) => {
          result[key] = value
          return result
        }, {})
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Collection',
      returns: { type: 'collection' },
      args: {
        a: { type: 'collection' },
        b: {
          type: 'function',
          description: 'The function to call for each element in the collection. The function should take two arguments: the element itself and the index.',
        },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Creates a new collection with all elements that pass the test implemented by $b. The function is called for each element in the collection, and it should take two arguments: the element itself and the index.',
      examples: [
        'filteri([1, 2, 3], (x, i) -> i % 2 == 0)',
        'filteri([1, 2, 3], (x, i) -> x % 2 == 0)',
        'filteri([1, 2, 3], (x, i) -> x + i > 3)',
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

      const mapped = paramArray.map(p => executeFunction(fn, p, contextStack, sourceCodeInfo))

      if (!isStr) {
        return mapped
      }
      mapped.forEach(char => assertString(char, sourceCodeInfo))
      return mapped.join('')
    },
    arity: { min: 2 },
    docs: {
      category: 'Collection',
      returns: { type: 'collection' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
        colls: { type: 'collection', rest: true, description: 'At least one.' },
        fun: { type: 'function' },
      },
      variants: [{ argumentNames: ['colls', 'fun'] }],
      description: 'Creates a new collection populated with the results of calling $fun on every element in $colls.',
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
  'mapi': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }) => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)

      if (Array.isArray(coll)) {
        return coll.map((elem, index) => executeFunction(fn, [elem, index], contextStack, sourceCodeInfo))
      }
      if (isString(coll)) {
        return coll
          .split('')
          .map((elem, index) => executeFunction(fn, [elem, index], contextStack, sourceCodeInfo))
          .join('')
      }
      return Object.entries(coll)
        .reduce((acc: Obj, [key, value]) => {
          acc[key] = executeFunction(fn, [value, key], contextStack, sourceCodeInfo)
          return acc
        }, {})
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Collection',
      returns: { type: 'collection' },
      args: {
        a: { type: 'collection' },
        b: {
          type: 'function',
          description: 'The function to call for each element in the collection. The function should take two arguments: the element itself and the index.',
        },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Creates a new collection populated with the results of calling $b on every element in $a. The function is called for each element in the collection, and it should take two arguments: the element itself and the index.',
      examples: [
        'mapi([1, 2, 3], (x, i) -> x + i)',
        'mapi([1, 2, 3], (x, i) -> x * i)',
        'mapi([1, 2, 3], (x, i) -> x - i)',
        'mapi([1, 2, 3], (x, i) -> x / i)',
        'mapi([1, 2, 3], (x, i) -> x % inc(i))',
      ],
    },
  },
  'reduce': {
    evaluate: ([coll, fn, initial], sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      assertAny(initial, sourceCodeInfo)

      if (typeof coll === 'string') {
        assertString(initial, sourceCodeInfo)
        if (coll.length === 0)
          return initial

        return coll.split('').reduce((result: Any, elem) => {
          return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
        }, initial)
      }
      else if (Array.isArray(coll)) {
        if (coll.length === 0)
          return initial

        return coll.reduce((result: Any, elem) => {
          return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
        }, initial)
      }
      else {
        if (Object.keys(coll).length === 0)
          return initial

        return Object.entries(coll).reduce((result: Any, [, elem]) => {
          return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
        }, initial)
      }
    },
    arity: toFixedArity(3),
    docs: {
      category: 'Collection',
      returns: { type: 'any' },
      args: {
        fun: { type: 'function' },
        coll: { type: 'collection' },
        initial: { type: 'any' },
      },
      variants: [{ argumentNames: ['coll', 'fun', 'initial'] }],
      description: 'Runs $fun function on each element of the $coll, passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $coll is a single value.',
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
  'reducei': {
    evaluate: ([coll, fn, initial], sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      assertAny(initial, sourceCodeInfo)

      if (typeof coll === 'string') {
        assertString(initial, sourceCodeInfo)
        if (coll.length === 0)
          return initial

        return coll.split('').reduce((result: Any, elem, index) => {
          return executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo)
        }, initial)
      }
      else if (Array.isArray(coll)) {
        if (coll.length === 0)
          return initial

        return coll.reduce((result: Any, elem, index) => {
          return executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo)
        }, initial)
      }
      else {
        if (Object.keys(coll).length === 0)
          return initial

        return Object.entries(coll).reduce((result: Any, [key, elem]) => {
          return executeFunction(fn, [result, elem, key], contextStack, sourceCodeInfo)
        }, initial)
      }
    },
    arity: toFixedArity(3),
    docs: {
      category: 'Collection',
      returns: { type: 'any' },
      args: {
        coll: { type: 'collection' },
        fun: {
          type: 'function',
          description: 'The function to call for each element in the collection. The function should take three arguments: the accumulator, the element itself, and the index.',
        },
        initial: {
          type: 'any',
          description: 'The initial value to use as the accumulator.',
        },
      },
      variants: [{ argumentNames: ['coll', 'fun', 'initial'] }],
      description: 'Runs $fun function on each element of the $coll, passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $coll is a single value. The function is called for each element in the collection, and it should take three arguments: the accumulator, the element itself, and the index.',
      examples: [
        'reducei([1, 2, 3], (acc, x, i) -> acc + x + i, 0)',
        'reducei("Albert", (acc, x, i) -> acc ++ x ++ i, "")',
        'reducei({ a: 1, b: 2 }, -> $1 ++ $3, "")',
      ],
    },
  },
  'reduce-right': {
    evaluate: ([coll, fn, initial], sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      assertAny(initial, sourceCodeInfo)

      if (typeof coll === 'string') {
        if (coll.length === 0)
          return initial

        return coll.split('').reduceRight((result: Any, elem) => {
          return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
        }, initial)
      }
      else if (Array.isArray(coll)) {
        if (coll.length === 0)
          return initial

        return coll.reduceRight((result: Any, elem) => {
          return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
        }, initial)
      }
      else {
        if (Object.keys(coll).length === 0)
          return initial

        return Object.entries(coll).reduceRight((result: Any, [, elem]) => {
          return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
        }, initial)
      }
    },
    arity: toFixedArity(3),
    docs: {
      category: 'Collection',
      returns: { type: 'any' },
      args: {
        fun: { type: 'function' },
        coll: { type: 'collection' },
        initial: { type: 'any' },
      },
      variants: [{ argumentNames: ['coll', 'fun', 'initial'] }],
      description: 'Runs $fun function on each element of the $coll (starting from the last item), passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $coll is a single value.',
      examples: [
        'reduce-right(["A", "B", "C"], str, "")',
        'reduce-right({ a: 1, b: 2 }, +, 0)',
      ],
    },
  },
  'reducei-right': {
    evaluate: ([coll, fn, initial], sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      assertAny(initial, sourceCodeInfo)

      if (typeof coll === 'string') {
        if (coll.length === 0)
          return initial

        return coll.split('').reduceRight((result: Any, elem, index) => {
          return executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo)
        }, initial)
      }
      else if (Array.isArray(coll)) {
        if (coll.length === 0)
          return initial

        return coll.reduceRight((result: Any, elem, index) => {
          return executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo)
        }, initial)
      }
      else {
        if (Object.keys(coll).length === 0)
          return initial

        return Object.entries(coll).reduceRight((result: Any, [key, elem]) => {
          return executeFunction(fn, [result, elem, key], contextStack, sourceCodeInfo)
        }, initial)
      }
    },
    arity: toFixedArity(3),
    docs: {
      category: 'Collection',
      returns: { type: 'any' },
      args: {
        coll: { type: 'collection' },
        fun: {
          type: 'function',
          description: 'The function to call for each element in the collection. The function should take three arguments: the accumulator, the element itself, and the index.',
        },
        initial: {
          type: 'any',
          description: 'The initial value to use as the accumulator.',
        },
      },
      variants: [{ argumentNames: ['coll', 'fun', 'initial'] }],
      description: 'Runs $fun function on each element of the $coll (starting from the last item), passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $coll is a single value. The function is called for each element in the collection, and it should take three arguments: the accumulator, the element itself, and the index.',
      examples: [
        'reducei-right([1, 2, 3], (acc, x, i) -> acc + x + i, 0)',
        'reducei-right("Albert", (acc, x, i) -> acc ++ x ++ i, "")',
        'reducei-right({ a: 1, b: 2 }, -> $1 ++ $3, "")',
      ],
    },
  },
  'reductions': {
    evaluate: ([coll, fn, initial], sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      assertAny(initial, sourceCodeInfo)

      assertAny(initial, sourceCodeInfo)
      if (typeof coll === 'string') {
        assertString(initial, sourceCodeInfo)
        if (coll.length === 0)
          return [initial]

        const resultArray: Any[] = [initial]
        coll.split('').reduce((result: Any, elem) => {
          const newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
          resultArray.push(newVal)
          return newVal
        }, initial)
        return resultArray
      }
      else if (Array.isArray(coll)) {
        if (coll.length === 0)
          return [initial]

        const resultArray: Any[] = [initial]
        coll.reduce((result: Any, elem) => {
          const newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
          resultArray.push(newVal)
          return newVal
        }, initial)
        return resultArray
      }
      else {
        if (Object.keys(coll).length === 0)
          return [initial]

        const resultArray: Any[] = [initial]
        Object.entries(coll).reduce((result: Any, [, elem]) => {
          const newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo)
          resultArray.push(newVal)
          return newVal
        }, initial)
        return resultArray
      }
    },
    arity: toFixedArity(3),
    docs: {
      category: 'Collection',
      returns: { type: 'any', array: true },
      args: {
        fun: { type: 'function' },
        coll: { type: 'collection' },
        initial: { type: 'any' },
      },
      variants: [{ argumentNames: ['coll', 'fun', 'initial'] }],
      description: 'Returns an array of the intermediate values of the reduction (see `reduce`) of $coll by $fun.',
      examples: [
        'reductions([1, 2, 3], +, 0)',
        'reductions([1, 2, 3], +, 10)',
        'reductions([], +, 0)',
        'reductions({ a: 1, b: 2 }, +, 0)',
        `
reductions(
  [1, 2, 3, 4, 5, 6, 7, 8, 9],
  (result, value) -> result + (even?(value) ? value : 0),
  0
)`,
      ],
    },
  },
  'reductionsi': {
    evaluate: ([coll, fn, initial], sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      assertAny(initial, sourceCodeInfo)

      assertAny(initial, sourceCodeInfo)
      if (typeof coll === 'string') {
        assertString(initial, sourceCodeInfo)
        if (coll.length === 0)
          return [initial]

        const resultArray: Any[] = [initial]
        coll.split('').reduce((result: Any, elem, index) => {
          const newVal = executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo)
          resultArray.push(newVal)
          return newVal
        }, initial)
        return resultArray
      }
      else if (Array.isArray(coll)) {
        if (coll.length === 0)
          return [initial]

        const resultArray: Any[] = [initial]
        coll.reduce((result: Any, elem, index) => {
          const newVal = executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo)
          resultArray.push(newVal)
          return newVal
        }, initial)
        return resultArray
      }
      else {
        if (Object.keys(coll).length === 0)
          return [initial]

        const resultArray: Any[] = [initial]
        Object.entries(coll).reduce((result: Any, [key, elem]) => {
          const newVal = executeFunction(fn, [result, elem, key], contextStack, sourceCodeInfo)
          resultArray.push(newVal)
          return newVal
        }, initial)
        return resultArray
      }
    },
    arity: toFixedArity(3),
    docs: {
      category: 'Collection',
      returns: { type: 'any', array: true },
      args: {
        coll: { type: 'collection' },
        fun: {
          type: 'function',
          description: 'The function to call for each element in the collection. The function should take three arguments: the accumulator, the element itself, and the index.',
        },
        initial: {
          type: 'any',
          description: 'The initial value to use as the accumulator.',
        },
      },
      variants: [{ argumentNames: ['coll', 'fun', 'initial'] }],
      description: 'Returns an array of the intermediate values of the reduction (see `reduce`) of $coll by $fun. The function is called for each element in the collection, and it should take three arguments: the accumulator, the element itself, and the index.',
      examples: [
        'reductionsi([1, 2, 3], (acc, x, i) -> acc + x + i, 0)',
        'reductionsi("Albert", (acc, x, i) -> acc ++ x ++ i, "")',
        'reductionsi({ a: 1, b: 2 }, -> $1 ++ $3, "")',
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
      category: 'Collection',
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
  'get-in': {
    evaluate: (params, sourceCodeInfo): Any => {
      let coll = toAny(params[0])
      const keys = params[1] ?? [] // null behaves as empty array
      const defaultValue = toAny(params[2])
      assertArray(keys, sourceCodeInfo)
      for (const key of keys) {
        assertStringOrNumber(key, sourceCodeInfo)
        if (isColl(coll)) {
          const nextValue = get(coll, key)
          if (nextValue !== undefined)
            coll = nextValue
          else
            return defaultValue
        }
        else {
          return defaultValue
        }
      }
      return coll
    },
    arity: { min: 2, max: 3 },
    docs: {
      category: 'Collection',
      returns: { type: 'any' },
      args: {
        'a': { type: 'collection' },
        'b': { type: 'array' },
        'not-found': { type: 'any' },
      },
      variants: [
        { argumentNames: ['a', 'b'] },
        { argumentNames: ['a', 'b', 'not-found'] },
      ],
      description: 'Returns the value in a nested collection, where $b is an array of keys. Returns $not-found if the key is not present. If $not-found is not set, `null` is returned.',
      examples: [
        `
get-in(
  [[1, 2, 3], [4, { a: "Kalle" }, 6]],
  [1, 1, "a", 0]
)`,
        `
get-in(
  [[1, 2, 3], [4, { a: "Kalle" }, 6]],
  [1, 1, "b", 0]
)`,
        `
get-in(
  [[1, 2, 3], [4, { a: "Kalle" }, 6]],
  [1, 1, "b", 0],
  "Lisa"
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
      category: 'Collection',
      returns: { type: 'number' },
      args: {
        coll: { type: ['collection', 'null'] },
      },
      variants: [{ argumentNames: ['coll'] }],
      description: 'Returns number of elements in $coll.',
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
      category: 'Collection',
      returns: { type: 'boolean' },
      args: {
        a: { type: ['collection', 'null'] },
        b: { type: ['string', 'integer'] },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns `true` if $a contains $b, otherwise returns `false`. For strings, it checks if substring is included.',
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
      category: 'Collection',
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
  'assoc-in': {
    evaluate: ([originalColl, keys, value], sourceCodeInfo): Coll => {
      assertColl(originalColl, sourceCodeInfo)
      assertArray(keys, sourceCodeInfo)
      assertAny(value, sourceCodeInfo)

      if (keys.length === 1) {
        assertStringOrNumber(keys[0], sourceCodeInfo)
        return assoc(originalColl, keys[0], value, sourceCodeInfo)
      }

      const { coll, innerCollMeta } = cloneAndGetMeta(originalColl, keys, sourceCodeInfo)

      const lastKey = asStringOrNumber(keys[keys.length - 1], sourceCodeInfo)
      const parentKey = asStringOrNumber(keys[keys.length - 2], sourceCodeInfo)

      if (Array.isArray(innerCollMeta.parent)) {
        assertNumber(parentKey, sourceCodeInfo)
        innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, sourceCodeInfo)
      }
      else {
        assertString(parentKey, sourceCodeInfo)
        innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, sourceCodeInfo)
      }

      return coll
    },
    arity: toFixedArity(3),
    docs: {
      category: 'Collection',
      returns: { type: 'collection' },
      args: {
        coll: { type: 'collection' },
        ks: { type: ['number', 'string'], array: true },
        value: { type: 'any' },
      },
      variants: [{ argumentNames: ['coll', 'ks', 'value'] }],
      description: `
Associates a value in the nested collection $coll, where $ks is an array of keys and $value is the new value.

If any levels do not exist, objects will be created - and the corresponding keys must be of type string.`,
      examples: [
        `
assoc-in(
  {},
  ["a", "b", "c"],
  "Albert"
)`,
        `
assoc-in(
  [1, 2, [1, 2, 3]],
  [2, 1],
  "Albert"
)`,
        `
assoc-in(
  [1, 2, { name: "albert" }],
  [2, "name", 0],
  "A"
)`,
      ],
    },
  },
  'update': {
    evaluate: ([coll, key, fn, ...params], sourceCodeInfo, contextStack, { executeFunction }): Coll => {
      assertColl(coll, sourceCodeInfo)
      assertStringOrNumber(key, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      return update(coll, key, fn, params, contextStack, executeFunction, sourceCodeInfo)
    },
    arity: { min: 3 },
    docs: {
      category: 'Collection',
      returns: { type: 'collection' },
      args: {
        'coll': { type: 'collection' },
        'key': { type: ['string', 'number'] },
        'fun': { type: 'function' },
        'fun-args': { type: 'any', rest: true },
      },
      variants: [
        { argumentNames: ['coll', 'value', 'fun'] },
        { argumentNames: ['coll', 'value', 'fun', 'fun-args'] },
      ],
      description: `
Updates a value in the $coll collection, where $key is a key. $fun is a function
that will take the old value and any supplied $fun-args and
return the new value.
If the key does not exist, \`null\` is passed as the old value.`,
      examples: [
        `
let x = { a: 1, b: 2 };
update(x, "a", inc)`,
        `
let x = { a: 1, b: 2 };
update(
  x,
  "c",
  val -> null?(val) ? 0 : inc(val)
)`,
      ],
    },
  },
  'update-in': {
    evaluate: ([originalColl, keys, fn, ...params], sourceCodeInfo, contextStack, { executeFunction }): Coll => {
      assertColl(originalColl, sourceCodeInfo)
      assertArray(keys, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)

      if (keys.length === 1) {
        assertStringOrNumber(keys[0], sourceCodeInfo)
        return update(originalColl, keys[0], fn, params, contextStack, executeFunction, sourceCodeInfo)
      }

      const { coll, innerCollMeta } = cloneAndGetMeta(originalColl, keys, sourceCodeInfo)

      const lastKey = asStringOrNumber(keys[keys.length - 1], sourceCodeInfo)
      const parentKey = asStringOrNumber(keys[keys.length - 2], sourceCodeInfo)

      if (Array.isArray(innerCollMeta.parent)) {
        assertNumber(parentKey, sourceCodeInfo)
        innerCollMeta.parent[parentKey] = update(
          innerCollMeta.coll,
          lastKey,
          fn,
          params,
          contextStack,
          executeFunction,
          sourceCodeInfo,
        )
      }
      else {
        assertString(parentKey, sourceCodeInfo)
        innerCollMeta.parent[parentKey] = update(
          innerCollMeta.coll,
          lastKey,
          fn,
          params,
          contextStack,
          executeFunction,
          sourceCodeInfo,
        )
      }

      return coll
    },
    arity: { min: 3 },
    docs: {
      category: 'Collection',
      returns: { type: 'collection' },
      args: {
        'coll': { type: 'collection' },
        'ks': { type: 'array' },
        'fun': { type: 'function' },
        'fun-args': { type: 'any', rest: true },
      },
      variants: [
        { argumentNames: ['coll', 'ks', 'fun'] },
        { argumentNames: ['coll', 'ks', 'fun', 'fun-args'] },
      ],
      description: `Updates a value in the $coll collection, where $ks is an array of
keys and $fun is a function that will take the old value and
any supplied $fun-args and return the new value. If any levels do not exist,
objects will be created - and the corresponding keys must be of type string.`,
      examples: [
        `
update-in(
  { a: [1, 2, 3] },
  ["a", 1],
  -> null?($) ? 0 : inc($)
)`,
        `
update-in(
  { a: { foo: "bar"} },
  ["a", "foo"],
  -> null?($) ? "?" : "!"
)`,
        `
update-in(
  { a: { foo: "bar"} },
  ["a", "baz"],
  -> null?($) ? "?" : "!"
)`,
        `
update-in(
  { a: [1, 2, 3] },
  ["a", 1],
  *,
  10,
  10,
  10,
)`,
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
      category: 'Collection',
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
  'not-empty': {
    evaluate: ([coll], sourceCodeInfo): Coll | null => {
      if (coll === null)
        return null

      assertColl(coll, sourceCodeInfo)
      if (typeof coll === 'string')
        return coll.length > 0 ? coll : null

      if (Array.isArray(coll))
        return coll.length > 0 ? coll : null

      return Object.keys(coll).length > 0 ? coll : null
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Collection',
      returns: { type: 'boolean' },
      args: {
        coll: { type: ['collection', 'null'] },
      },
      variants: [{ argumentNames: ['coll'] }],
      description: 'Returns `null` if $coll is empty or `null`, otherwise $coll.',
      examples: [
        'not-empty([])',
        'not-empty([1, 2, 3])',
        'not-empty({})',
        'not-empty({ a: 2 })',
        'not-empty("")',
        'not-empty("Albert")',
        'not-empty(null)',
      ],
    },
  },
  'every?': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)

      if (Array.isArray(coll))
        return coll.every(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      if (typeof coll === 'string')
        return coll.split('').every(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      return Object.entries(coll).every(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Collection',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns `true` if all entries in $a pass the test implemented by $b, otherwise returns `false`.',
      examples: [
        '[1, 2, 3] every? number?',
        '[1, 2, 3] every? even?',
        `
every?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?,
)`,
        `
every?(
  [50, 100, 150, 200],
  -> $ > 10,
)`,
        `
every?(
  [],
  number?
)`,
        `
every?(
  "",
  number?
)`,
        `
every?(
  {},
  number?
)`,
        `
every?(
  { a: 2, b: 4},
  -> even?(second($))
)`,
        `
every?(
  { a: 2, b: 3 },
  -> even?(second($))
)`,
      ],
    },
  },
  'any?': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertFunctionLike(fn, sourceCodeInfo)
      assertColl(coll, sourceCodeInfo)

      if (Array.isArray(coll))
        return coll.some(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      if (typeof coll === 'string')
        return coll.split('').some(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      return Object.entries(coll).some(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Collection',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns `true` if any element in $a pass the test implemented by $b, otherwise returns `false`.',
      examples: [
        `
any?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
        `
any?(
  [50, 100, 150, 200],
  x -> x > 10
)`,
        `
any?(
  [],
  number?
)`,
        `
any?(
  "",
  number?
)`,
        `
any?(
  {},
  number?
)`,
        `
any?(
  { a: 2, b: 3 },
  -> even?(second($))
)`,
        `
any?(
  { a: 1, b: 3 },
  -> even?(second($))
)`,
      ],
    },
  },
  'not-any?': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertFunctionLike(fn, sourceCodeInfo)
      assertColl(coll, sourceCodeInfo)

      if (Array.isArray(coll))
        return !coll.some(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      if (typeof coll === 'string')
        return !coll.split('').some(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      return !Object.entries(coll).some(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Collection',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns `false` if any element in $a pass the test implemented by $b, otherwise returns `true`.',
      examples: [
        `
not-any?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
        `
not-any?(
  [50, 100, 150, 200],
  x -> x > 10
)`,
        `
not-any?(
  [],
  number?
)`,
        `
not-any?(
  "",
  number?
)`,
        `
not-any?(
  {},
  number?
)`,
        `
not-any?(
  { a: 2, b: 3 },
  -> even?(second($))
)`,
        `
not-any?(
  { a: 1, b: 3 },
  -> even?(second($))
)`,
      ],
    },
  },
  'not-every?': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertFunctionLike(fn, sourceCodeInfo)
      assertColl(coll, sourceCodeInfo)

      if (Array.isArray(coll))
        return !coll.every(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      if (typeof coll === 'string')
        return !coll.split('').every(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      return !Object.entries(coll).every(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Collection',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns `true` if at least one element in $a does not pass the test implemented by $b, otherwise returns `false`.',
      examples: [
        `
not-every?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
        `
not-every?(
  [50, 100, 150, 200],
  x -> x > 10
)`,
        `
not-every?(
  [],
  number?
)`,
        `
not-every?(
  "",
  number?
)`,
        `
not-every?(
  {},
  number?
)`,
        `
not-every?(
  { a: 2, b: 4 },
  -> even?(second($))
)`,
        `
not-every?(
  { a: 2, b: 3 },
  -> even?(second($))
)`,
      ],
    },
  },
}
