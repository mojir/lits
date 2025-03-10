import type { LitsFunction } from '../../..'
import type { ContextStack } from '../../../evaluator/ContextStack'
import type { ExecuteFunction } from '../../../evaluator/interface'
import type { Any, Arr, Coll, Obj } from '../../../interface'
import type { SourceCodeInfo } from '../../../tokenizer/interface'
import { cloneColl, collHasKey, toAny, toNonNegativeInteger } from '../../../utils'

import { assertLitsFunction } from '../../../typeGuards/litsFunction'

import type { BuiltinNormalExpressions } from '../../interface'
import { assertArray } from '../../../typeGuards/array'
import { asColl, assertAny, assertColl, assertObj, assertSeq, isColl, isObj, isSeq } from '../../../typeGuards/lits'
import { assertNumber, isNumber } from '../../../typeGuards/number'
import { asString, asStringOrNumber, assertString, assertStringOrNumber, isString } from '../../../typeGuards/string'

interface CollMeta {
  coll: Coll
  parent: Obj | Arr
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
  fn: LitsFunction,
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
    paramCount: { min: 2, max: 3 },
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
    paramCount: { min: 2, max: 3 },
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
    paramCount: 1,
  },
  'contains?': {
    evaluate: ([coll, key], sourceCodeInfo): boolean => {
      assertStringOrNumber(key, sourceCodeInfo)
      if (coll === null)
        return false

      assertColl(coll, sourceCodeInfo)
      if (isSeq(coll)) {
        if (!isNumber(key, { integer: true }))
          return false

        assertNumber(key, sourceCodeInfo, { integer: true })
        return key >= 0 && key < coll.length
      }
      return !!Object.getOwnPropertyDescriptor(coll, key)
    },
    paramCount: 2,
  },
  'has?': {
    evaluate: ([coll, value], sourceCodeInfo): boolean => {
      if (coll === null)
        return false

      assertColl(coll, sourceCodeInfo)
      if (Array.isArray(coll))
        return coll.includes(value)

      if (typeof coll === 'string') {
        if (typeof value === 'string') {
          return coll.includes(value)
        }
        else if (typeof value === 'number') {
          return coll.includes(`${value}`)
        }
        return false
      }

      return Object.values(coll).includes(value)
    },
    paramCount: 2,
  },
  'has-some?': {
    evaluate: ([coll, seq], sourceCodeInfo): boolean => {
      if (coll === null || seq === null)
        return false

      assertColl(coll, sourceCodeInfo)
      assertSeq(seq, sourceCodeInfo)
      if (Array.isArray(coll)) {
        for (const value of seq) {
          if (coll.includes(value))
            return true
        }
        return false
      }
      if (typeof coll === 'string') {
        for (const value of seq) {
          if (isString(value, { char: true }) ? coll.split('').includes(value) : false)
            return true
        }
        return false
      }
      for (const value of seq) {
        if (Object.values(coll).includes(value))
          return true
      }
      return false
    },
    paramCount: 2,
  },
  'has-every?': {
    evaluate: ([coll, seq], sourceCodeInfo): boolean => {
      if (coll === null)
        return false

      assertColl(coll, sourceCodeInfo)
      if (seq === null)
        return true

      assertSeq(seq, sourceCodeInfo)
      if (Array.isArray(coll)) {
        for (const value of seq) {
          if (!coll.includes(value))
            return false
        }
        return true
      }
      if (typeof coll === 'string') {
        for (const value of seq) {
          if (!isString(value, { char: true }) || !coll.split('').includes(value))
            return false
        }
        return true
      }
      for (const value of seq) {
        if (!Object.values(coll).includes(value))
          return false
      }
      return true
    },
    paramCount: 2,
  },
  'assoc': {
    evaluate: ([coll, key, value], sourceCodeInfo): Coll => {
      assertColl(coll, sourceCodeInfo)
      assertStringOrNumber(key, sourceCodeInfo)
      assertAny(value, sourceCodeInfo)
      return assoc(coll, key, value, sourceCodeInfo)
    },
    paramCount: 3,
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
    paramCount: 3,
  },
  'update': {
    evaluate: ([coll, key, fn, ...params], sourceCodeInfo, contextStack, { executeFunction }): Coll => {
      assertColl(coll, sourceCodeInfo)
      assertStringOrNumber(key, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)
      return update(coll, key, fn, params, contextStack, executeFunction, sourceCodeInfo)
    },
    paramCount: { min: 3 },
  },
  'update-in': {
    evaluate: ([originalColl, keys, fn, ...params], sourceCodeInfo, contextStack, { executeFunction }): Coll => {
      assertColl(originalColl, sourceCodeInfo)
      assertArray(keys, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)

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
    paramCount: { min: 3 },
  },
  'concat': {
    evaluate: (params, sourceCodeInfo): Any => {
      assertColl(params[0], sourceCodeInfo)
      if (Array.isArray(params[0])) {
        return params.reduce((result: Arr, arr) => {
          assertArray(arr, sourceCodeInfo)
          return result.concat(arr)
        }, [])
      }
      else if (isString(params[0])) {
        return params.reduce((result: string, s) => {
          assertString(s, sourceCodeInfo)
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
    paramCount: { min: 1 },
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
    paramCount: 1,
  },
  'every?': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertColl(coll, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)

      if (Array.isArray(coll))
        return coll.every(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      if (typeof coll === 'string')
        return coll.split('').every(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      return Object.entries(coll).every(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
    },
    paramCount: 2,
  },
  'any?': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertLitsFunction(fn, sourceCodeInfo)
      assertColl(coll, sourceCodeInfo)

      if (Array.isArray(coll))
        return coll.some(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      if (typeof coll === 'string')
        return coll.split('').some(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      return Object.entries(coll).some(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
    },
    paramCount: 2,
  },
  'not-any?': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertLitsFunction(fn, sourceCodeInfo)
      assertColl(coll, sourceCodeInfo)

      if (Array.isArray(coll))
        return !coll.some(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      if (typeof coll === 'string')
        return !coll.split('').some(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      return !Object.entries(coll).some(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
    },
    paramCount: 2,
  },
  'not-every?': {
    evaluate: ([coll, fn], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertLitsFunction(fn, sourceCodeInfo)
      assertColl(coll, sourceCodeInfo)

      if (Array.isArray(coll))
        return !coll.every(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      if (typeof coll === 'string')
        return !coll.split('').every(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))

      return !Object.entries(coll).every(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo))
    },
    paramCount: 2,
  },
}
