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
import type { MaybePromise } from '../../../utils/maybePromise'
import { chain, everySequential, filterSequential, mapSequential, reduceSequential, someSequential } from '../../../utils/maybePromise'
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
): MaybePromise<Coll> {
  if (isObj(coll)) {
    assertString(key, sourceCodeInfo)
    const result = { ...coll }
    return chain(executeFunction(fn, [result[key], ...params], contextStack, sourceCodeInfo), (val) => {
      result[key] = val
      return result
    })
  }
  else {
    assertNumber(key, sourceCodeInfo)
    const intKey = toNonNegativeInteger(key)
    assertNumber(intKey, sourceCodeInfo, { lte: coll.length })
    if (Array.isArray(coll)) {
      return chain(
        mapSequential(Array.from({ length: coll.length + (intKey === coll.length ? 1 : 0) }), (_, index) => {
          if (intKey === index)
            return executeFunction(fn, [coll[index], ...params], contextStack, sourceCodeInfo)
          return coll[index] as Any
        }),
        result => result,
      )
    }
    else {
      const chars = coll.split('')
      return chain(
        mapSequential(Array.from({ length: chars.length + (intKey === chars.length ? 1 : 0) }), (_, index) => {
          if (intKey === index) {
            return chain(
              executeFunction(fn, [chars[index], ...params], contextStack, sourceCodeInfo),
              val => asString(val, sourceCodeInfo, { char: true }),
            )
          }
          return chars[index] as string
        }),
        result => result.join(''),
      )
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
      category: 'collection',
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
      seeAlso: ['get', 'collection.assoc-in', 'collection.update-in'],
      examples: [
        `
let cu = import(collection);
cu.get-in(
  [[1, 2, 3], [4, { a: "Kalle" }, 6]],
  [1, 1, "a", 0]
)`,
        `
let cu = import(collection);
cu.get-in(
  [[1, 2, 3], [4, { a: "Kalle" }, 6]],
  [1, 1, "b", 0]
)`,
        `
let cu = import(collection);
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
      category: 'collection',
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
      seeAlso: ['assoc', 'collection.get-in', 'collection.update-in'],
      examples: [
        `
let cu = import(collection);
cu.assoc-in(
  {},
  ["a", "b", "c"],
  "Albert"
)`,
        `
let cu = import(collection);
cu.assoc-in(
  [1, 2, [1, 2, 3]],
  [2, 1],
  "Albert"
)`,
        `
let cu = import(collection);
cu.assoc-in(
  [1, 2, { name: "albert" }],
  [2, "name", 0],
  "A"
)`,
      ],
    },
  },
  'update': {
    evaluate: ([coll, key, fn, ...params], sourceCodeInfo, contextStack, { executeFunction }): MaybePromise<Coll> => {
      assertColl(coll, sourceCodeInfo)
      assertStringOrNumber(key, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      return update(coll, key, fn, params, contextStack, executeFunction, sourceCodeInfo)
    },
    arity: { min: 3 },
    docs: {
      category: 'collection',
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
      seeAlso: ['collection.update-in', 'assoc'],
      examples: [
        `
let cu = import(collection);
let x = { a: 1, b: 2 };
cu.update(x, "a", inc)`,
        `
let cu = import(collection);
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
    evaluate: ([originalColl, keys, fn, ...params], sourceCodeInfo, contextStack, { executeFunction }): MaybePromise<Coll> => {
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
        return chain(
          update(innerCollMeta.coll, lastKey, fn, params, contextStack, executeFunction, sourceCodeInfo),
          (updated) => {
            ;(innerCollMeta.parent as Any[])[parentKey] = updated
            return coll
          },
        )
      }
      else {
        assertString(parentKey, sourceCodeInfo)
        return chain(
          update(innerCollMeta.coll, lastKey, fn, params, contextStack, executeFunction, sourceCodeInfo),
          (updated) => {
            ;(innerCollMeta.parent as Record<string, unknown>)[parentKey] = updated
            return coll
          },
        )
      }
    },
    arity: { min: 3 },
    docs: {
      category: 'collection',
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
      seeAlso: ['collection.update', 'collection.assoc-in', 'collection.get-in'],
      examples: [
        `
let cu = import(collection);
cu.update-in(
  { a: [1, 2, 3] },
  ["a", 1],
  -> null?($) ? 0 : inc($)
)`,
        `
let cu = import(collection);
cu.update-in(
  { a: { foo: "bar"} },
  ["a", "foo"],
  -> null?($) ? "?" : "!"
)`,
        `
let cu = import(collection);
cu.update-in(
  { a: { foo: "bar"} },
  ["a", "baz"],
  -> null?($) ? "?" : "!"
)`,
        `
let cu = import(collection);
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
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): MaybePromise<Coll> => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      if (Array.isArray(coll)) {
        return filterSequential(coll, (elem, index) => executeFunction(fn, [elem, index], contextStack, sourceCodeInfo))
      }
      if (typeof coll === 'string') {
        return chain(
          filterSequential(coll.split(''), (elem, index) => executeFunction(fn, [elem, index], contextStack, sourceCodeInfo)),
          filtered => filtered.join(''),
        )
      }
      return chain(
        filterSequential(Object.entries(coll), ([key, value]) => executeFunction(fn, [value, key], contextStack, sourceCodeInfo)),
        filtered => filtered.reduce((result: Obj, [key, value]) => {
          result[key] = value
          return result
        }, {}),
      )
    },
    arity: toFixedArity(2),
    docs: {
      category: 'collection',
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
      seeAlso: ['filter', 'collection.mapi'],
      examples: [
        'let cu = import(collection); cu.filteri([1, 2, 3], (x, i) -> i % 2 == 0)',
        'let cu = import(collection); cu.filteri([1, 2, 3], (x, i) -> x % 2 == 0)',
        'let cu = import(collection); cu.filteri([1, 2, 3], (x, i) -> x + i > 3)',
      ],
    },
  },
  'mapi': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): MaybePromise<Coll> => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)

      if (Array.isArray(coll)) {
        return mapSequential(coll, (elem, index) => executeFunction(fn, [elem, index], contextStack, sourceCodeInfo))
      }
      if (typeof coll === 'string') {
        return chain(
          mapSequential(coll.split(''), (elem, index) => executeFunction(fn, [elem, index], contextStack, sourceCodeInfo)),
          mapped => mapped.join(''),
        )
      }
      const entries = Object.entries(coll)
      return reduceSequential(entries, (acc: Obj, [key, value]) => {
        return chain(executeFunction(fn, [value, key], contextStack, sourceCodeInfo), (result) => {
          acc[key] = result
          return acc
        })
      }, {})
    },
    arity: toFixedArity(2),
    docs: {
      category: 'collection',
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
      seeAlso: ['map', 'collection.filteri'],
      examples: [
        'let cu = import(collection); cu.mapi([1, 2, 3], (x, i) -> x + i)',
        'let cu = import(collection); cu.mapi([1, 2, 3], (x, i) -> x * i)',
        'let cu = import(collection); cu.mapi([1, 2, 3], (x, i) -> x - i)',
        'let cu = import(collection); cu.mapi([1, 2, 3], (x, i) -> x / i)',
        'let cu = import(collection); cu.mapi([1, 2, 3], (x, i) -> x % inc(i))',
      ],
    },
  },
  'reducei': {
    evaluate: ([coll, fn, initial], sourceCodeInfo, contextStack, { executeFunction }): MaybePromise<Any> => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      assertAny(initial, sourceCodeInfo)

      if (typeof coll === 'string') {
        assertString(initial, sourceCodeInfo)
        if (coll.length === 0)
          return initial

        return reduceSequential(
          coll.split('').map((elem, index) => ({ elem, index })),
          (result, { elem, index }) => executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo),
          initial as Any,
        )
      }
      else if (Array.isArray(coll)) {
        if (coll.length === 0)
          return initial

        return reduceSequential(
          coll.map((elem, index) => ({ elem, index })),
          (result, { elem, index }) => executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo),
          initial,
        )
      }
      else {
        if (Object.keys(coll).length === 0)
          return initial

        return reduceSequential(
          Object.entries(coll),
          (result, [key, elem]) => executeFunction(fn, [result, elem, key], contextStack, sourceCodeInfo),
          initial,
        )
      }
    },
    arity: toFixedArity(3),
    docs: {
      category: 'collection',
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
      seeAlso: ['reduce', 'collection.reducei-right', 'collection.reductionsi'],
      examples: [
        'let cu = import(collection); cu.reducei([1, 2, 3], (acc, x, i) -> acc + x + i, 0)',
        'let cu = import(collection); cu.reducei("Albert", (acc, x, i) -> acc ++ x ++ i, "")',
        'let cu = import(collection); cu.reducei({ a: 1, b: 2 }, -> $1 ++ $3, "")',
      ],
    },
  },
  'reduce-right': {
    evaluate: ([coll, fn, initial], sourceCodeInfo, contextStack, { executeFunction }): MaybePromise<Any> => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      assertAny(initial, sourceCodeInfo)

      if (typeof coll === 'string') {
        if (coll.length === 0)
          return initial

        return reduceSequential(
          Array.from(coll.split('')).reverse(),
          (result, elem) => executeFunction(fn, [result, elem], contextStack, sourceCodeInfo),
          initial,
        )
      }
      else if (Array.isArray(coll)) {
        if (coll.length === 0)
          return initial

        return reduceSequential(
          Array.from(coll).reverse(),
          (result, elem) => executeFunction(fn, [result, elem], contextStack, sourceCodeInfo),
          initial,
        )
      }
      else {
        if (Object.keys(coll).length === 0)
          return initial

        return reduceSequential(
          Object.entries(coll).reverse(),
          (result, [, elem]) => executeFunction(fn, [result, elem], contextStack, sourceCodeInfo),
          initial,
        )
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
      description: 'Runs $fun function on each element of the $coll (starting from the last item), passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $coll is a single value.',
      seeAlso: ['reduce', 'collection.reducei-right'],
      examples: [
        'let cu = import(collection); cu.reduce-right(["A", "B", "C"], str, "")',
        'let cu = import(collection); cu.reduce-right({ a: 1, b: 2 }, +, 0)',
      ],
    },
  },
  'reducei-right': {
    evaluate: ([coll, fn, initial], sourceCodeInfo, contextStack, { executeFunction }): MaybePromise<Any> => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      assertAny(initial, sourceCodeInfo)

      if (typeof coll === 'string') {
        if (coll.length === 0)
          return initial

        return reduceSequential(
          Array.from(coll.split('')).reverse().map((elem, _, arr) => ({ elem, index: arr.length - 1 - _ })),
          (result, { elem, index }) => executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo),
          initial,
        )
      }
      else if (Array.isArray(coll)) {
        if (coll.length === 0)
          return initial

        return reduceSequential(
          Array.from(coll).reverse().map((elem, _, arr) => ({ elem, index: arr.length - 1 - _ })),
          (result, { elem, index }) => executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo),
          initial,
        )
      }
      else {
        if (Object.keys(coll).length === 0)
          return initial

        return reduceSequential(
          Object.entries(coll).reverse(),
          (result, [key, elem]) => executeFunction(fn, [result, elem, key], contextStack, sourceCodeInfo),
          initial,
        )
      }
    },
    arity: toFixedArity(3),
    docs: {
      category: 'collection',
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
      seeAlso: ['collection.reducei', 'collection.reduce-right'],
      examples: [
        'let cu = import(collection); cu.reducei-right([1, 2, 3], (acc, x, i) -> acc + x + i, 0)',
        'let cu = import(collection); cu.reducei-right("Albert", (acc, x, i) -> acc ++ x ++ i, "")',
        'let cu = import(collection); cu.reducei-right({ a: 1, b: 2 }, -> $1 ++ $3, "")',
      ],
    },
  },
  'reductions': {
    evaluate: ([coll, fn, initial], sourceCodeInfo, contextStack, { executeFunction }): MaybePromise<Any> => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      assertAny(initial, sourceCodeInfo)

      assertAny(initial, sourceCodeInfo)

      const items: Any[] = typeof coll === 'string'
        ? (assertString(initial, sourceCodeInfo), coll.length === 0 ? [] : coll.split(''))
        : Array.isArray(coll)
          ? (coll.length === 0 ? [] : Array.from(coll) as Any[])
          : (Object.keys(coll).length === 0 ? [] : Object.entries(coll).map(([, v]) => v as Any))

      if (items.length === 0)
        return [initial]

      const resultArray: Any[] = [initial]
      return chain(
        reduceSequential(
          items,
          (result, elem) => {
            return chain(executeFunction(fn, [result, elem], contextStack, sourceCodeInfo), (newVal) => {
              resultArray.push(newVal)
              return newVal
            })
          },
          initial,
        ),
        () => resultArray,
      )
    },
    arity: toFixedArity(3),
    docs: {
      category: 'collection',
      returns: { type: 'any', array: true },
      args: {
        fun: { type: 'function' },
        coll: { type: 'collection' },
        initial: { type: 'any' },
      },
      variants: [{ argumentNames: ['coll', 'fun', 'initial'] }],
      description: 'Returns an array of the intermediate values of the reduction (see `reduce`) of $coll by $fun.',
      seeAlso: ['reduce', 'collection.reductionsi'],
      examples: [
        'let cu = import(collection); cu.reductions([1, 2, 3], +, 0)',
        'let cu = import(collection); cu.reductions([1, 2, 3], +, 10)',
        'let cu = import(collection); cu.reductions([], +, 0)',
        'let cu = import(collection); cu.reductions({ a: 1, b: 2 }, +, 0)',
        `
let cu = import(collection);
cu.reductions(
  [1, 2, 3, 4, 5, 6, 7, 8, 9],
  (result, value) -> result + (even?(value) ? value : 0),
  0
)`,
      ],
    },
  },
  'reductionsi': {
    evaluate: ([coll, fn, initial], sourceCodeInfo, contextStack, { executeFunction }): MaybePromise<Any> => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      assertAny(initial, sourceCodeInfo)

      assertAny(initial, sourceCodeInfo)

      type IndexedItem = { elem: Any, key: string | number }
      const toIndexedItem = (elem: Any, key: string | number): IndexedItem => ({ elem, key })
      const items: IndexedItem[] = typeof coll === 'string'
        ? (assertString(initial, sourceCodeInfo), coll.length === 0 ? [] : coll.split('').map((elem, index) => toIndexedItem(elem, index)))
        : Array.isArray(coll)
          ? (coll.length === 0 ? [] : coll.map((elem, index) => toIndexedItem(elem as Any, index)))
          : (Object.keys(coll).length === 0 ? [] : Object.entries(coll).map(([key, v]) => toIndexedItem(v as Any, key)))

      if (items.length === 0)
        return [initial]

      const resultArray: Any[] = [initial]
      return chain(
        reduceSequential(
          items,
          (result, { elem, key }) => {
            return chain(executeFunction(fn, [result, elem, key], contextStack, sourceCodeInfo), (newVal) => {
              resultArray.push(newVal)
              return newVal
            })
          },
          initial,
        ),
        () => resultArray,
      )
    },
    arity: toFixedArity(3),
    docs: {
      category: 'collection',
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
      seeAlso: ['collection.reductions', 'collection.reducei'],
      examples: [
        'let cu = import(collection); cu.reductionsi([1, 2, 3], (acc, x, i) -> acc + x + i, 0)',
        'let cu = import(collection); cu.reductionsi("Albert", (acc, x, i) -> acc ++ x ++ i, "")',
        'let cu = import(collection); cu.reductionsi({ a: 1, b: 2 }, -> $1 ++ $3, "")',
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
      category: 'collection',
      returns: { type: 'boolean' },
      args: {
        coll: { type: ['collection', 'null'] },
      },
      variants: [{ argumentNames: ['coll'] }],
      description: 'Returns `null` if $coll is empty or `null`, otherwise $coll.',
      seeAlso: ['empty?', 'not-empty?'],
      examples: [
        'let cu = import(collection); cu.not-empty([])',
        'let cu = import(collection); cu.not-empty([1, 2, 3])',
        'let cu = import(collection); cu.not-empty({})',
        'let cu = import(collection); cu.not-empty({ a: 2 })',
        'let cu = import(collection); cu.not-empty("")',
        'let cu = import(collection); cu.not-empty("Albert")',
        'let cu = import(collection); cu.not-empty(null)',
      ],
    },
  },
  'every?': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): MaybePromise<boolean> => {
      assertColl(coll, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)

      const arr = Array.isArray(coll)
        ? coll
        : typeof coll === 'string'
          ? coll.split('')
          : Object.entries(coll)

      return everySequential(arr, elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
    },
    arity: toFixedArity(2),
    docs: {
      category: 'collection',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns `true` if all entries in $a pass the test implemented by $b, otherwise returns `false`.',
      seeAlso: ['collection.any?', 'collection.not-every?', 'collection.not-any?', 'functional.every-pred', 'grid.every?'],
      examples: [
        'let cu = import(collection); cu.every?([1, 2, 3], number?)',
        'let cu = import(collection); cu.every?([1, 2, 3], even?)',
        `
let cu = import(collection);
cu.every?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?,
)`,
        `
let cu = import(collection);
cu.every?(
  [50, 100, 150, 200],
  -> $ > 10,
)`,
        'let cu = import(collection); cu.every?([], number?)',
        'let cu = import(collection); cu.every?("", number?)',
        'let cu = import(collection); cu.every?({}, number?)',
        `
let cu = import(collection);
cu.every?(
  { a: 2, b: 4},
  -> even?(second($))
)`,
        `
let cu = import(collection);
cu.every?(
  { a: 2, b: 3 },
  -> even?(second($))
)`,
      ],
    },
  },
  'any?': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): MaybePromise<boolean> => {
      assertFunctionLike(fn, sourceCodeInfo)
      assertColl(coll, sourceCodeInfo)

      const arr = Array.isArray(coll)
        ? coll
        : typeof coll === 'string'
          ? coll.split('')
          : Object.entries(coll)

      return someSequential(arr, elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
    },
    arity: toFixedArity(2),
    docs: {
      category: 'collection',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns `true` if any element in $a pass the test implemented by $b, otherwise returns `false`.',
      seeAlso: ['collection.every?', 'collection.not-any?', 'collection.not-every?', 'functional.some-pred', 'some', 'grid.some?'],
      examples: [
        `
let cu = import(collection);
cu.any?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
        `
let cu = import(collection);
cu.any?(
  [50, 100, 150, 200],
  x -> x > 10
)`,
        'let cu = import(collection); cu.any?([], number?)',
        'let cu = import(collection); cu.any?("", number?)',
        'let cu = import(collection); cu.any?({}, number?)',
        `
let cu = import(collection);
cu.any?(
  { a: 2, b: 3 },
  -> even?(second($))
)`,
        `
let cu = import(collection);
cu.any?(
  { a: 1, b: 3 },
  -> even?(second($))
)`,
      ],
    },
  },
  'not-any?': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): MaybePromise<boolean> => {
      assertFunctionLike(fn, sourceCodeInfo)
      assertColl(coll, sourceCodeInfo)

      const arr = Array.isArray(coll)
        ? coll
        : typeof coll === 'string'
          ? coll.split('')
          : Object.entries(coll)

      return chain(
        someSequential(arr, elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo)),
        result => !result,
      )
    },
    arity: toFixedArity(2),
    docs: {
      category: 'collection',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns `false` if any element in $a pass the test implemented by $b, otherwise returns `true`.',
      seeAlso: ['collection.any?', 'collection.every?', 'collection.not-every?'],
      examples: [
        `
let cu = import(collection);
cu.not-any?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
        `
let cu = import(collection);
cu.not-any?(
  [50, 100, 150, 200],
  x -> x > 10
)`,
        'let cu = import(collection); cu.not-any?([], number?)',
        'let cu = import(collection); cu.not-any?("", number?)',
        'let cu = import(collection); cu.not-any?({}, number?)',
        `
let cu = import(collection);
cu.not-any?(
  { a: 2, b: 3 },
  -> even?(second($))
)`,
        `
let cu = import(collection);
cu.not-any?(
  { a: 1, b: 3 },
  -> even?(second($))
)`,
      ],
    },
  },
  'not-every?': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): MaybePromise<boolean> => {
      assertFunctionLike(fn, sourceCodeInfo)
      assertColl(coll, sourceCodeInfo)

      const arr = Array.isArray(coll)
        ? coll
        : typeof coll === 'string'
          ? coll.split('')
          : Object.entries(coll)

      return chain(
        everySequential(arr, elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo)),
        result => !result,
      )
    },
    arity: toFixedArity(2),
    docs: {
      category: 'collection',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns `true` if at least one element in $a does not pass the test implemented by $b, otherwise returns `false`.',
      seeAlso: ['collection.every?', 'collection.any?', 'collection.not-any?'],
      examples: [
        `
let cu = import(collection);
cu.not-every?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
        `
let cu = import(collection);
cu.not-every?(
  [50, 100, 150, 200],
  x -> x > 10
)`,
        'let cu = import(collection); cu.not-every?([], number?)',
        'let cu = import(collection); cu.not-every?("", number?)',
        'let cu = import(collection); cu.not-every?({}, number?)',
        `
let cu = import(collection);
cu.not-every?(
  { a: 2, b: 4 },
  -> even?(second($))
)`,
        `
let cu = import(collection);
cu.not-every?(
  { a: 2, b: 3 },
  -> even?(second($))
)`,
      ],
    },
  },
}

export const collectionUtilsModule: LitsModule = {
  name: 'collection',
  functions: collectionUtilsFunctions,
}
