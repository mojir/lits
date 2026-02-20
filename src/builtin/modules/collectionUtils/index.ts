import type { ContextStack } from '../../../evaluator/ContextStack'
import type { ExecuteFunction } from '../../../evaluator/interface'
import type { Any, Arr, Coll, Obj } from '../../../interface'
import type { SourceCodeInfo } from '../../../tokenizer/token'
import { cloneColl, collHasKey, toAny, toNonNegativeInteger } from '../../../utils'
import { asColl, assertAny, assertColl, assertFunctionLike, assertObj, isColl, isObj } from '../../../typeGuards/lits'
import type { BuiltinNormalExpressions } from '../../interface'
import { assertArray } from '../../../typeGuards/array'
import { assertNumber, isNumber } from '../../../typeGuards/number'
import { asString, asStringOrNumber, assertString, assertStringOrNumber } from '../../../typeGuards/string'
import type { FunctionLike } from '../../../parser/types'
import { toFixedArity } from '../../../utils/arity'
import type { LitsModule } from '../interface'

// --- Private helper: get value from collection by key ---
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

// --- Private helper: assoc value into collection ---
function assoc(coll: Coll, key: string | number, value: Any, sourceCodeInfo?: SourceCodeInfo) {
  assertColl(coll, sourceCodeInfo)
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

// --- Private helper: update value in collection ---
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

interface CollMeta {
  coll: Coll
  parent: Obj | Arr
}

// --- Private helper: clone and get meta for nested operations ---
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
          (resultColl)[key] = {}

        newResultColl = asColl(resultColl[key], sourceCodeInfo)
      }

      return { coll: newResultColl, parent: resultColl }
    },
    { coll, parent: {} },
  )
  return { coll, innerCollMeta }
}

const collectionUtilsFunctions: BuiltinNormalExpressions = {
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
      category: 'Collection-Utils',
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
      seeAlso: ['get', 'Collection-Utils.assoc-in', 'Collection-Utils.update-in'],
      examples: [
        `
let cu = import("Collection-Utils");
cu.get-in(
  [[1, 2, 3], [4, { a: "Kalle" }, 6]],
  [1, 1, "a", 0]
)`,
        `
let cu = import("Collection-Utils");
cu.get-in(
  [[1, 2, 3], [4, { a: "Kalle" }, 6]],
  [1, 1, "b", 0]
)`,
        `
let cu = import("Collection-Utils");
cu.get-in(
  [[1, 2, 3], [4, { a: "Kalle" }, 6]],
  [1, 1, "b", 0],
  "Lisa"
)`,
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
      category: 'Collection-Utils',
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
      seeAlso: ['assoc', 'Collection-Utils.get-in', 'Collection-Utils.update-in'],
      examples: [
        `
let cu = import("Collection-Utils");
cu.assoc-in(
  {},
  ["a", "b", "c"],
  "Albert"
)`,
        `
let cu = import("Collection-Utils");
cu.assoc-in(
  [1, 2, [1, 2, 3]],
  [2, 1],
  "Albert"
)`,
        `
let cu = import("Collection-Utils");
cu.assoc-in(
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
      category: 'Collection-Utils',
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
      seeAlso: ['Collection-Utils.update-in', 'assoc'],
      examples: [
        `
let cu = import("Collection-Utils");
let x = { a: 1, b: 2 };
cu.update(x, "a", inc)`,
        `
let cu = import("Collection-Utils");
let x = { a: 1, b: 2 };
cu.update(
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
      category: 'Collection-Utils',
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
      seeAlso: ['Collection-Utils.update', 'Collection-Utils.assoc-in', 'Collection-Utils.get-in'],
      examples: [
        `
let cu = import("Collection-Utils");
cu.update-in(
  { a: [1, 2, 3] },
  ["a", 1],
  -> null?($) ? 0 : inc($)
)`,
        `
let cu = import("Collection-Utils");
cu.update-in(
  { a: { foo: "bar"} },
  ["a", "foo"],
  -> null?($) ? "?" : "!"
)`,
        `
let cu = import("Collection-Utils");
cu.update-in(
  { a: { foo: "bar"} },
  ["a", "baz"],
  -> null?($) ? "?" : "!"
)`,
        `
let cu = import("Collection-Utils");
cu.update-in(
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
  'filteri': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): Coll => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      if (Array.isArray(coll)) {
        const result = coll.filter((elem, index) => executeFunction(fn, [elem, index], contextStack, sourceCodeInfo))
        return result
      }
      if (typeof coll === 'string') {
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
      category: 'Collection-Utils',
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
      seeAlso: ['filter', 'Collection-Utils.mapi'],
      examples: [
        'let cu = import("Collection-Utils"); cu.filteri([1, 2, 3], (x, i) -> i % 2 == 0)',
        'let cu = import("Collection-Utils"); cu.filteri([1, 2, 3], (x, i) -> x % 2 == 0)',
        'let cu = import("Collection-Utils"); cu.filteri([1, 2, 3], (x, i) -> x + i > 3)',
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
      if (typeof coll === 'string') {
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
      category: 'Collection-Utils',
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
      seeAlso: ['map', 'Collection-Utils.filteri'],
      examples: [
        'let cu = import("Collection-Utils"); cu.mapi([1, 2, 3], (x, i) -> x + i)',
        'let cu = import("Collection-Utils"); cu.mapi([1, 2, 3], (x, i) -> x * i)',
        'let cu = import("Collection-Utils"); cu.mapi([1, 2, 3], (x, i) -> x - i)',
        'let cu = import("Collection-Utils"); cu.mapi([1, 2, 3], (x, i) -> x / i)',
        'let cu = import("Collection-Utils"); cu.mapi([1, 2, 3], (x, i) -> x % inc(i))',
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
      category: 'Collection-Utils',
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
      seeAlso: ['reduce', 'Collection-Utils.reducei-right', 'Collection-Utils.reductionsi'],
      examples: [
        'let cu = import("Collection-Utils"); cu.reducei([1, 2, 3], (acc, x, i) -> acc + x + i, 0)',
        'let cu = import("Collection-Utils"); cu.reducei("Albert", (acc, x, i) -> acc ++ x ++ i, "")',
        'let cu = import("Collection-Utils"); cu.reducei({ a: 1, b: 2 }, -> $1 ++ $3, "")',
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
      category: 'Collection-Utils',
      returns: { type: 'any' },
      args: {
        fun: { type: 'function' },
        coll: { type: 'collection' },
        initial: { type: 'any' },
      },
      variants: [{ argumentNames: ['coll', 'fun', 'initial'] }],
      description: 'Runs $fun function on each element of the $coll (starting from the last item), passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $coll is a single value.',
      seeAlso: ['reduce', 'Collection-Utils.reducei-right'],
      examples: [
        'let cu = import("Collection-Utils"); cu.reduce-right(["A", "B", "C"], str, "")',
        'let cu = import("Collection-Utils"); cu.reduce-right({ a: 1, b: 2 }, +, 0)',
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
      category: 'Collection-Utils',
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
      seeAlso: ['Collection-Utils.reducei', 'Collection-Utils.reduce-right'],
      examples: [
        'let cu = import("Collection-Utils"); cu.reducei-right([1, 2, 3], (acc, x, i) -> acc + x + i, 0)',
        'let cu = import("Collection-Utils"); cu.reducei-right("Albert", (acc, x, i) -> acc ++ x ++ i, "")',
        'let cu = import("Collection-Utils"); cu.reducei-right({ a: 1, b: 2 }, -> $1 ++ $3, "")',
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
      category: 'Collection-Utils',
      returns: { type: 'any', array: true },
      args: {
        fun: { type: 'function' },
        coll: { type: 'collection' },
        initial: { type: 'any' },
      },
      variants: [{ argumentNames: ['coll', 'fun', 'initial'] }],
      description: 'Returns an array of the intermediate values of the reduction (see `reduce`) of $coll by $fun.',
      seeAlso: ['reduce', 'Collection-Utils.reductionsi'],
      examples: [
        'let cu = import("Collection-Utils"); cu.reductions([1, 2, 3], +, 0)',
        'let cu = import("Collection-Utils"); cu.reductions([1, 2, 3], +, 10)',
        'let cu = import("Collection-Utils"); cu.reductions([], +, 0)',
        'let cu = import("Collection-Utils"); cu.reductions({ a: 1, b: 2 }, +, 0)',
        `
let cu = import("Collection-Utils");
cu.reductions(
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
      category: 'Collection-Utils',
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
      seeAlso: ['Collection-Utils.reductions', 'Collection-Utils.reducei'],
      examples: [
        'let cu = import("Collection-Utils"); cu.reductionsi([1, 2, 3], (acc, x, i) -> acc + x + i, 0)',
        'let cu = import("Collection-Utils"); cu.reductionsi("Albert", (acc, x, i) -> acc ++ x ++ i, "")',
        'let cu = import("Collection-Utils"); cu.reductionsi({ a: 1, b: 2 }, -> $1 ++ $3, "")',
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
      category: 'Collection-Utils',
      returns: { type: 'boolean' },
      args: {
        coll: { type: ['collection', 'null'] },
      },
      variants: [{ argumentNames: ['coll'] }],
      description: 'Returns `null` if $coll is empty or `null`, otherwise $coll.',
      seeAlso: ['empty?', 'not-empty?'],
      examples: [
        'let cu = import("Collection-Utils"); cu.not-empty([])',
        'let cu = import("Collection-Utils"); cu.not-empty([1, 2, 3])',
        'let cu = import("Collection-Utils"); cu.not-empty({})',
        'let cu = import("Collection-Utils"); cu.not-empty({ a: 2 })',
        'let cu = import("Collection-Utils"); cu.not-empty("")',
        'let cu = import("Collection-Utils"); cu.not-empty("Albert")',
        'let cu = import("Collection-Utils"); cu.not-empty(null)',
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
      category: 'Collection-Utils',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns `true` if all entries in $a pass the test implemented by $b, otherwise returns `false`.',
      seeAlso: ['Collection-Utils.any?', 'Collection-Utils.not-every?', 'Collection-Utils.not-any?', 'every-pred', 'Grid.every?'],
      examples: [
        'let cu = import("Collection-Utils"); cu.every?([1, 2, 3], number?)',
        'let cu = import("Collection-Utils"); cu.every?([1, 2, 3], even?)',
        `
let cu = import("Collection-Utils");
cu.every?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?,
)`,
        `
let cu = import("Collection-Utils");
cu.every?(
  [50, 100, 150, 200],
  -> $ > 10,
)`,
        'let cu = import("Collection-Utils"); cu.every?([], number?)',
        'let cu = import("Collection-Utils"); cu.every?("", number?)',
        'let cu = import("Collection-Utils"); cu.every?({}, number?)',
        `
let cu = import("Collection-Utils");
cu.every?(
  { a: 2, b: 4},
  -> even?(second($))
)`,
        `
let cu = import("Collection-Utils");
cu.every?(
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
      category: 'Collection-Utils',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns `true` if any element in $a pass the test implemented by $b, otherwise returns `false`.',
      seeAlso: ['Collection-Utils.every?', 'Collection-Utils.not-any?', 'Collection-Utils.not-every?', 'some-pred', 'some', 'Grid.some?'],
      examples: [
        `
let cu = import("Collection-Utils");
cu.any?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
        `
let cu = import("Collection-Utils");
cu.any?(
  [50, 100, 150, 200],
  x -> x > 10
)`,
        'let cu = import("Collection-Utils"); cu.any?([], number?)',
        'let cu = import("Collection-Utils"); cu.any?("", number?)',
        'let cu = import("Collection-Utils"); cu.any?({}, number?)',
        `
let cu = import("Collection-Utils");
cu.any?(
  { a: 2, b: 3 },
  -> even?(second($))
)`,
        `
let cu = import("Collection-Utils");
cu.any?(
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
      category: 'Collection-Utils',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns `false` if any element in $a pass the test implemented by $b, otherwise returns `true`.',
      seeAlso: ['Collection-Utils.any?', 'Collection-Utils.every?', 'Collection-Utils.not-every?'],
      examples: [
        `
let cu = import("Collection-Utils");
cu.not-any?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
        `
let cu = import("Collection-Utils");
cu.not-any?(
  [50, 100, 150, 200],
  x -> x > 10
)`,
        'let cu = import("Collection-Utils"); cu.not-any?([], number?)',
        'let cu = import("Collection-Utils"); cu.not-any?("", number?)',
        'let cu = import("Collection-Utils"); cu.not-any?({}, number?)',
        `
let cu = import("Collection-Utils");
cu.not-any?(
  { a: 2, b: 3 },
  -> even?(second($))
)`,
        `
let cu = import("Collection-Utils");
cu.not-any?(
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
      category: 'Collection-Utils',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns `true` if at least one element in $a does not pass the test implemented by $b, otherwise returns `false`.',
      seeAlso: ['Collection-Utils.every?', 'Collection-Utils.any?', 'Collection-Utils.not-any?'],
      examples: [
        `
let cu = import("Collection-Utils");
cu.not-every?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
        `
let cu = import("Collection-Utils");
cu.not-every?(
  [50, 100, 150, 200],
  x -> x > 10
)`,
        'let cu = import("Collection-Utils"); cu.not-every?([], number?)',
        'let cu = import("Collection-Utils"); cu.not-every?("", number?)',
        'let cu = import("Collection-Utils"); cu.not-every?({}, number?)',
        `
let cu = import("Collection-Utils");
cu.not-every?(
  { a: 2, b: 4 },
  -> even?(second($))
)`,
        `
let cu = import("Collection-Utils");
cu.not-every?(
  { a: 2, b: 3 },
  -> even?(second($))
)`,
      ],
    },
  },
}

export const collectionUtilsModule: LitsModule = {
  name: 'Collection-Utils',
  functions: collectionUtilsFunctions,
}
