import { LitsFunction } from '../../..'
import { ContextStack, ExecuteFunction } from '../../../evaluator/interface'
import { Any, Arr, Coll, Obj } from '../../../interface'
import { DebugInfo } from '../../../tokenizer/interface'
import { toNonNegativeInteger, toAny, collHasKey, cloneColl } from '../../../utils'
import {
  any,
  collection,
  litsFunction,
  number,
  object,
  sequence,
  stringOrNumber,
  array,
  string,
  assertNumberOfParams,
} from '../../../utils/assertion'
import { BuiltinNormalExpressions } from '../../interface'

type CollMeta = {
  coll: Coll
  parent: Obj | Arr
}

function cloneAndGetMeta(
  originalColl: Coll,
  keys: Arr,
  debugInfo?: DebugInfo,
): { coll: Coll; innerCollMeta: CollMeta } {
  const coll = cloneColl(originalColl)

  const butLastKeys = keys.slice(0, keys.length - 1)

  const innerCollMeta = butLastKeys.reduce(
    (result: CollMeta, key) => {
      const resultColl = result.coll

      let newResultColl: Coll
      if (array.is(resultColl)) {
        number.assert(key, debugInfo)
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        newResultColl = collection.as(resultColl[key], debugInfo)
      } else {
        object.assert(resultColl, debugInfo)
        string.assert(key, debugInfo)
        if (!collHasKey(result.coll, key)) {
          resultColl[key] = {}
        }
        newResultColl = collection.as(resultColl[key], debugInfo)
      }

      return { coll: newResultColl, parent: resultColl }
    },
    { coll, parent: {} },
  )
  return { coll, innerCollMeta }
}

function get(coll: Coll, key: string | number): Any | undefined {
  if (object.is(coll)) {
    if (string.is(key) && collHasKey(coll, key)) {
      return toAny(coll[key])
    }
  } else {
    if (number.is(key, { nonNegative: true, integer: true }) && key >= 0 && key < coll.length) {
      return toAny(coll[key])
    }
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
  debugInfo?: DebugInfo,
): Coll {
  if (object.is(coll)) {
    string.assert(key, debugInfo)
    const result = { ...coll }
    result[key] = executeFunction(fn, [result[key], ...params], contextStack, debugInfo)
    return result
  } else {
    number.assert(key, debugInfo)
    const intKey = toNonNegativeInteger(key)
    number.assert(intKey, debugInfo, { lte: coll.length })
    if (Array.isArray(coll)) {
      const result = coll.map((elem, index) => {
        if (intKey === index) {
          return executeFunction(fn, [elem, ...params], contextStack, debugInfo)
        }
        return elem
      })
      if (intKey === coll.length) {
        result[intKey] = executeFunction(fn, [undefined, ...params], contextStack, debugInfo)
      }
      return result
    } else {
      const result = coll.split(``).map((elem, index) => {
        if (intKey === index) {
          return string.as(executeFunction(fn, [elem, ...params], contextStack, debugInfo), debugInfo, {
            char: true,
          })
        }
        return elem
      })
      if (intKey === coll.length) {
        result[intKey] = string.as(executeFunction(fn, [undefined, ...params], contextStack, debugInfo), debugInfo, {
          char: true,
        })
      }
      return result.join(``)
    }
  }
}

function assoc(coll: Coll, key: string | number, value: Any, debugInfo?: DebugInfo) {
  collection.assert(coll, debugInfo)
  stringOrNumber.assert(key, debugInfo)
  if (Array.isArray(coll) || typeof coll === `string`) {
    number.assert(key, debugInfo, { integer: true })
    number.assert(key, debugInfo, { gte: 0 })
    number.assert(key, debugInfo, { lte: coll.length })
    if (typeof coll === `string`) {
      string.assert(value, debugInfo, { char: true })
      return `${coll.slice(0, key)}${value}${coll.slice(key + 1)}`
    }
    const copy = [...coll]
    copy[key] = value
    return copy
  }
  string.assert(key, debugInfo)
  const copy = { ...coll }
  copy[key] = value
  return copy
}

export const collectionNormalExpression: BuiltinNormalExpressions = {
  get: {
    evaluate: (params, debugInfo) => {
      const [coll, key] = params
      const defaultValue = toAny(params[2])
      stringOrNumber.assert(key, debugInfo)
      if (coll === null) {
        return defaultValue
      }
      collection.assert(coll, debugInfo)
      const result = get(coll, key)
      return result === undefined ? defaultValue : result
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'get-in': {
    evaluate: (params, debugInfo): Any => {
      let coll = toAny(params[0])
      const keys = params[1] ?? [] // nil behaves as empty array
      const defaultValue = toAny(params[2])
      array.assert(keys, debugInfo)
      for (const key of keys) {
        stringOrNumber.assert(key, debugInfo)
        if (collection.is(coll)) {
          const nextValue = get(coll, key)
          if (nextValue !== undefined) {
            coll = nextValue
          } else {
            return defaultValue
          }
        } else {
          return defaultValue
        }
      }
      return coll
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  count: {
    evaluate: ([coll], debugInfo): number => {
      if (typeof coll === `string`) {
        return coll.length
      }
      collection.assert(coll, debugInfo)
      if (Array.isArray(coll)) {
        return coll.length
      }
      return Object.keys(coll).length
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'contains?': {
    evaluate: ([coll, key], debugInfo): boolean => {
      collection.assert(coll, debugInfo)
      stringOrNumber.assert(key, debugInfo)
      if (sequence.is(coll)) {
        if (!number.is(key, { integer: true })) {
          return false
        }
        number.assert(key, debugInfo, { integer: true })
        return key >= 0 && key < coll.length
      }
      return !!Object.getOwnPropertyDescriptor(coll, key)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'has?': {
    evaluate: ([coll, value], debugInfo): boolean => {
      collection.assert(coll, debugInfo)
      if (array.is(coll)) {
        return coll.includes(value)
      }
      if (string.is(coll)) {
        return string.is(value) ? coll.split(``).includes(value) : false
      }
      return Object.values(coll).includes(value)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'has-some?': {
    evaluate: ([coll, seq], debugInfo): boolean => {
      collection.assert(coll, debugInfo)
      sequence.assert(seq, debugInfo)
      if (array.is(coll)) {
        for (const value of seq) {
          if (coll.includes(value)) {
            return true
          }
        }
        return false
      }
      if (string.is(coll)) {
        for (const value of seq) {
          if (string.is(value, { char: true }) ? coll.split(``).includes(value) : false) {
            return true
          }
        }
        return false
      }
      for (const value of seq) {
        if (Object.values(coll).includes(value)) {
          return true
        }
      }
      return false
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'has-every?': {
    evaluate: ([coll, seq], debugInfo): boolean => {
      collection.assert(coll, debugInfo)
      sequence.assert(seq, debugInfo)
      if (array.is(coll)) {
        for (const value of seq) {
          if (!coll.includes(value)) {
            return false
          }
        }
        return true
      }
      if (string.is(coll)) {
        for (const value of seq) {
          if (!string.is(value, { char: true }) || !coll.split(``).includes(value)) {
            return false
          }
        }
        return true
      }
      for (const value of seq) {
        if (!Object.values(coll).includes(value)) {
          return false
        }
      }
      return true
    },
    validate: node => assertNumberOfParams(2, node),
  },
  assoc: {
    evaluate: ([coll, key, value], debugInfo): Coll => {
      collection.assert(coll, debugInfo)
      stringOrNumber.assert(key, debugInfo)
      any.assert(value, debugInfo)
      return assoc(coll, key, value, debugInfo)
    },
    validate: node => assertNumberOfParams(3, node),
  },
  'assoc-in': {
    evaluate: ([originalColl, keys, value], debugInfo): Coll => {
      collection.assert(originalColl, debugInfo)
      array.assert(keys, debugInfo)
      any.assert(value, debugInfo)

      if (keys.length === 1) {
        stringOrNumber.assert(keys[0], debugInfo)
        return assoc(originalColl, keys[0], value, debugInfo)
      }

      const { coll, innerCollMeta } = cloneAndGetMeta(originalColl, keys, debugInfo)

      const lastKey = stringOrNumber.as(keys[keys.length - 1], debugInfo)
      const parentKey = stringOrNumber.as(keys[keys.length - 2], debugInfo)

      if (array.is(innerCollMeta.parent)) {
        number.assert(parentKey, debugInfo)
        innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, debugInfo)
      } else {
        string.assert(parentKey, debugInfo)
        innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, debugInfo)
      }

      return coll
    },
    validate: node => assertNumberOfParams(3, node),
  },
  update: {
    evaluate: ([coll, key, fn, ...params], debugInfo, contextStack, { executeFunction }): Coll => {
      collection.assert(coll, debugInfo)
      stringOrNumber.assert(key, debugInfo)
      litsFunction.assert(fn, debugInfo)
      return update(coll, key, fn, params, contextStack, executeFunction, debugInfo)
    },
    validate: node => assertNumberOfParams({ min: 3 }, node),
  },
  'update-in': {
    evaluate: ([originalColl, keys, fn, ...params], debugInfo, contextStack, { executeFunction }): Coll => {
      collection.assert(originalColl, debugInfo)
      array.assert(keys, debugInfo)
      litsFunction.assert(fn, debugInfo)

      if (keys.length === 1) {
        stringOrNumber.assert(keys[0], debugInfo)
        return update(originalColl, keys[0], fn, params, contextStack, executeFunction, debugInfo)
      }

      const { coll, innerCollMeta } = cloneAndGetMeta(originalColl, keys, debugInfo)

      const lastKey = stringOrNumber.as(keys[keys.length - 1], debugInfo)
      const parentKey = stringOrNumber.as(keys[keys.length - 2], debugInfo)

      if (array.is(innerCollMeta.parent)) {
        number.assert(parentKey, debugInfo)
        innerCollMeta.parent[parentKey] = update(
          innerCollMeta.coll,
          lastKey,
          fn,
          params,
          contextStack,
          executeFunction,
          debugInfo,
        )
      } else {
        string.assert(parentKey, debugInfo)
        innerCollMeta.parent[parentKey] = update(
          innerCollMeta.coll,
          lastKey,
          fn,
          params,
          contextStack,
          executeFunction,
          debugInfo,
        )
      }

      return coll
    },
    validate: node => assertNumberOfParams({ min: 3 }, node),
  },
  concat: {
    evaluate: (params, debugInfo): Any => {
      collection.assert(params[0], debugInfo)
      if (array.is(params[0])) {
        return params.reduce((result: Arr, arr) => {
          array.assert(arr, debugInfo)
          return result.concat(arr)
        }, [])
      } else if (string.is(params[0])) {
        return params.reduce((result: string, s) => {
          string.assert(s, debugInfo)
          return `${result}${s}`
        }, ``)
      } else {
        return params.reduce((result: Obj, obj) => {
          object.assert(obj, debugInfo)
          return Object.assign(result, obj)
        }, {})
      }
    },
    validate: node => assertNumberOfParams({ min: 1 }, node),
  },
  'not-empty': {
    evaluate: ([coll], debugInfo): Coll | null => {
      collection.assert(coll, debugInfo)
      if (string.is(coll)) {
        return coll.length > 0 ? coll : null
      }
      if (Array.isArray(coll)) {
        return coll.length > 0 ? coll : null
      }
      return Object.keys(coll).length > 0 ? coll : null
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'every?': {
    evaluate: ([fn, coll], debugInfo, contextStack, { executeFunction }): boolean => {
      litsFunction.assert(fn, debugInfo)
      collection.assert(coll, debugInfo)

      if (Array.isArray(coll)) {
        return coll.every(elem => executeFunction(fn, [elem], contextStack, debugInfo))
      }
      if (string.is(coll)) {
        return coll.split(``).every(elem => executeFunction(fn, [elem], contextStack, debugInfo))
      }
      return Object.entries(coll).every(elem => executeFunction(fn, [elem], contextStack, debugInfo))
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'any?': {
    evaluate: ([fn, coll], debugInfo, contextStack, { executeFunction }): boolean => {
      litsFunction.assert(fn, debugInfo)
      collection.assert(coll, debugInfo)

      if (Array.isArray(coll)) {
        return coll.some(elem => executeFunction(fn, [elem], contextStack, debugInfo))
      }
      if (string.is(coll)) {
        return coll.split(``).some(elem => executeFunction(fn, [elem], contextStack, debugInfo))
      }
      return Object.entries(coll).some(elem => executeFunction(fn, [elem], contextStack, debugInfo))
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'not-any?': {
    evaluate: ([fn, coll], debugInfo, contextStack, { executeFunction }): boolean => {
      litsFunction.assert(fn, debugInfo)
      collection.assert(coll, debugInfo)

      if (Array.isArray(coll)) {
        return !coll.some(elem => executeFunction(fn, [elem], contextStack, debugInfo))
      }
      if (string.is(coll)) {
        return !coll.split(``).some(elem => executeFunction(fn, [elem], contextStack, debugInfo))
      }
      return !Object.entries(coll).some(elem => executeFunction(fn, [elem], contextStack, debugInfo))
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'not-every?': {
    evaluate: ([fn, coll], debugInfo, contextStack, { executeFunction }): boolean => {
      litsFunction.assert(fn, debugInfo)
      collection.assert(coll, debugInfo)

      if (Array.isArray(coll)) {
        return !coll.every(elem => executeFunction(fn, [elem], contextStack, debugInfo))
      }
      if (string.is(coll)) {
        return !coll.split(``).every(elem => executeFunction(fn, [elem], contextStack, debugInfo))
      }
      return !Object.entries(coll).every(elem => executeFunction(fn, [elem], contextStack, debugInfo))
    },
    validate: node => assertNumberOfParams(2, node),
  },
}
