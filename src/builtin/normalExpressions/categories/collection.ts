import { LitsFunction } from '../../..'
import { ContextStack, ExecuteFunction } from '../../../evaluator/interface'
import { Any, Arr, Coll, Obj } from '../../../interface'
import { SourceCodeInfo } from '../../../tokenizer/interface'
import { assertLength, toNonNegativeInteger, toAny, collHasKey, cloneColl } from '../../../utils'
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
} from '../../../utils/assertion'
import { BuiltinNormalExpressions } from '../../interface'

type CollMeta = {
  coll: Coll
  parent: Obj | Arr
}

function cloneAndGetMeta(
  originalColl: Coll,
  keys: Arr,
  sourceCodeInfo: SourceCodeInfo,
): { coll: Coll; innerCollMeta: CollMeta } {
  const coll = cloneColl(originalColl)

  const butLastKeys = keys.slice(0, keys.length - 1)

  const innerCollMeta = butLastKeys.reduce(
    (result: CollMeta, key) => {
      const resultColl = result.coll

      let newResultColl: Coll
      if (array.is(resultColl)) {
        number.assert(key, sourceCodeInfo)
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        newResultColl = collection.as(resultColl[key], sourceCodeInfo)
      } else {
        object.assert(resultColl, sourceCodeInfo)
        string.assert(key, sourceCodeInfo)
        if (!collHasKey(result.coll, key)) {
          resultColl[key] = {}
        }
        newResultColl = collection.as(resultColl[key], sourceCodeInfo)
      }

      return { coll: newResultColl, parent: resultColl }
    },
    { coll, parent: {} },
  )
  return { coll, innerCollMeta }
}

function get(coll: Coll, key: string | number, sourceCodeInfo: SourceCodeInfo): Any | undefined {
  if (array.is(coll)) {
    number.assert(key, sourceCodeInfo, { integer: true })
    if (key < coll.length) {
      return toAny(coll[key])
    }
  } else if (object.is(coll)) {
    string.assert(key, sourceCodeInfo)
    if (collHasKey(coll, key)) {
      return toAny(coll[key])
    }
  } else {
    number.assert(key, sourceCodeInfo, { integer: true })
    if (key < coll.length) {
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
  sourceCodeInfo: SourceCodeInfo,
  contextStack: ContextStack,
  executeFunction: ExecuteFunction,
): Coll {
  if (object.is(coll)) {
    string.assert(key, sourceCodeInfo)
    const result = { ...coll }
    result[key] = executeFunction(fn, [result[key], ...params], sourceCodeInfo, contextStack)
    return result
  } else {
    number.assert(key, sourceCodeInfo)
    const intKey = toNonNegativeInteger(key)
    number.assert(intKey, sourceCodeInfo, { lte: coll.length })
    if (Array.isArray(coll)) {
      const result = coll.map((elem, index) => {
        if (intKey === index) {
          return executeFunction(fn, [elem, ...params], sourceCodeInfo, contextStack)
        }
        return elem
      })
      if (intKey === coll.length) {
        result[intKey] = executeFunction(fn, [undefined, ...params], sourceCodeInfo, contextStack)
      }
      return result
    } else {
      const result = coll.split(``).map((elem, index) => {
        if (intKey === index) {
          return string.as(executeFunction(fn, [elem, ...params], sourceCodeInfo, contextStack), sourceCodeInfo, {
            char: true,
          })
        }
        return elem
      })
      if (intKey === coll.length) {
        result[intKey] = string.as(
          executeFunction(fn, [undefined, ...params], sourceCodeInfo, contextStack),
          sourceCodeInfo,
          {
            char: true,
          },
        )
      }
      return result.join(``)
    }
  }
}

function assoc(coll: Coll, key: string | number, value: Any, sourceCodeInfo: SourceCodeInfo) {
  collection.assert(coll, sourceCodeInfo)
  stringOrNumber.assert(key, sourceCodeInfo)
  if (Array.isArray(coll) || typeof coll === `string`) {
    number.assert(key, sourceCodeInfo, { integer: true })
    number.assert(key, sourceCodeInfo, { gte: 0 })
    number.assert(key, sourceCodeInfo, { lte: coll.length })
    if (typeof coll === `string`) {
      string.assert(value, sourceCodeInfo, { char: true })
      return `${coll.slice(0, key)}${value}${coll.slice(key + 1)}`
    }
    const copy = [...coll]
    copy[key] = value
    return copy
  }
  string.assert(key, sourceCodeInfo)
  const copy = { ...coll }
  copy[key] = value
  return copy
}

export const collectionNormalExpression: BuiltinNormalExpressions = {
  get: {
    evaluate: (params, sourceCodeInfo) => {
      const [coll, key] = params
      const defaultValue = toAny(params[2])
      collection.assert(coll, sourceCodeInfo)
      stringOrNumber.assert(key, sourceCodeInfo)
      const result = get(coll, key, sourceCodeInfo)
      return result === undefined ? defaultValue : result
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  'get-in': {
    evaluate: (params, sourceCodeInfo): Any => {
      let coll = params[0]
      const keys = params[1]
      const defaultValue = toAny(params[2])
      collection.assert(coll, sourceCodeInfo)
      array.assert(keys, sourceCodeInfo)
      for (const key of keys) {
        stringOrNumber.assert(key, sourceCodeInfo)
        if (collection.is(coll)) {
          coll = get(coll, key, sourceCodeInfo)
        } else {
          return defaultValue
        }
      }
      return any.is(coll) ? coll : defaultValue
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  count: {
    evaluate: ([coll], sourceCodeInfo): number => {
      if (typeof coll === `string`) {
        return coll.length
      }
      collection.assert(coll, sourceCodeInfo)
      if (Array.isArray(coll)) {
        return coll.length
      }
      return Object.keys(coll).length
    },
    validate: node => assertLength(1, node),
  },
  'contains?': {
    evaluate: ([coll, key], sourceCodeInfo): boolean => {
      collection.assert(coll, sourceCodeInfo)
      stringOrNumber.assert(key, sourceCodeInfo)
      if (sequence.is(coll)) {
        if (!number.is(key, { integer: true })) {
          return false
        }
        number.assert(key, sourceCodeInfo, { integer: true })
        return key >= 0 && key < coll.length
      }
      return !!Object.getOwnPropertyDescriptor(coll, key)
    },
    validate: node => assertLength(2, node),
  },
  'has?': {
    evaluate: ([coll, value], sourceCodeInfo): boolean => {
      collection.assert(coll, sourceCodeInfo)
      if (array.is(coll)) {
        return coll.includes(value)
      }
      if (string.is(coll)) {
        return string.is(value) ? coll.split(``).includes(value) : false
      }
      return Object.values(coll).includes(value)
    },
    validate: node => assertLength(2, node),
  },
  'has-some?': {
    evaluate: ([coll, seq], sourceCodeInfo): boolean => {
      collection.assert(coll, sourceCodeInfo)
      sequence.assert(seq, sourceCodeInfo)
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
    validate: node => assertLength(2, node),
  },
  'has-every?': {
    evaluate: ([coll, seq], sourceCodeInfo): boolean => {
      collection.assert(coll, sourceCodeInfo)
      sequence.assert(seq, sourceCodeInfo)
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
    validate: node => assertLength(2, node),
  },
  assoc: {
    evaluate: ([coll, key, value], sourceCodeInfo): Coll => {
      collection.assert(coll, sourceCodeInfo)
      stringOrNumber.assert(key, sourceCodeInfo)
      any.assert(value, sourceCodeInfo)
      return assoc(coll, key, value, sourceCodeInfo)
    },
    validate: node => assertLength(3, node),
  },
  'assoc-in': {
    evaluate: ([originalColl, keys, value], sourceCodeInfo): Coll => {
      collection.assert(originalColl, sourceCodeInfo)
      array.assert(keys, sourceCodeInfo)
      any.assert(value, sourceCodeInfo)

      if (keys.length === 1) {
        stringOrNumber.assert(keys[0], sourceCodeInfo)
        return assoc(originalColl, keys[0], value, sourceCodeInfo)
      }

      const { coll, innerCollMeta } = cloneAndGetMeta(originalColl, keys, sourceCodeInfo)

      const lastKey = stringOrNumber.as(keys[keys.length - 1], sourceCodeInfo)
      const parentKey = stringOrNumber.as(keys[keys.length - 2], sourceCodeInfo)

      if (array.is(innerCollMeta.parent)) {
        number.assert(parentKey, sourceCodeInfo)
        innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, sourceCodeInfo)
      } else {
        string.assert(parentKey, sourceCodeInfo)
        innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, sourceCodeInfo)
      }

      return coll
    },
    validate: node => assertLength(3, node),
  },
  update: {
    evaluate: ([coll, key, fn, ...params], sourceCodeInfo, contextStack, { executeFunction }): Coll => {
      collection.assert(coll, sourceCodeInfo)
      stringOrNumber.assert(key, sourceCodeInfo)
      litsFunction.assert(fn, sourceCodeInfo)
      return update(coll, key, fn, params, sourceCodeInfo, contextStack, executeFunction)
    },
    validate: node => assertLength({ min: 3 }, node),
  },
  'update-in': {
    evaluate: ([originalColl, keys, fn, ...params], sourceCodeInfo, contextStack, { executeFunction }): Coll => {
      collection.assert(originalColl, sourceCodeInfo)
      array.assert(keys, sourceCodeInfo)
      litsFunction.assert(fn, sourceCodeInfo)

      if (keys.length === 1) {
        stringOrNumber.assert(keys[0], sourceCodeInfo)
        return update(originalColl, keys[0], fn, params, sourceCodeInfo, contextStack, executeFunction)
      }

      const { coll, innerCollMeta } = cloneAndGetMeta(originalColl, keys, sourceCodeInfo)

      const lastKey = stringOrNumber.as(keys[keys.length - 1], sourceCodeInfo)
      const parentKey = stringOrNumber.as(keys[keys.length - 2], sourceCodeInfo)

      if (array.is(innerCollMeta.parent)) {
        number.assert(parentKey, sourceCodeInfo)
        innerCollMeta.parent[parentKey] = update(
          innerCollMeta.coll,
          lastKey,
          fn,
          params,
          sourceCodeInfo,
          contextStack,
          executeFunction,
        )
      } else {
        string.assert(parentKey, sourceCodeInfo)
        innerCollMeta.parent[parentKey] = update(
          innerCollMeta.coll,
          lastKey,
          fn,
          params,
          sourceCodeInfo,
          contextStack,
          executeFunction,
        )
      }

      return coll
    },
    validate: node => assertLength({ min: 3 }, node),
  },
  concat: {
    evaluate: (params, sourceCodeInfo): Any => {
      collection.assert(params[0], sourceCodeInfo)
      if (array.is(params[0])) {
        return params.reduce((result: Arr, arr) => {
          array.assert(arr, sourceCodeInfo)
          return result.concat(arr)
        }, [])
      } else if (string.is(params[0])) {
        return params.reduce((result: string, s) => {
          string.assert(s, sourceCodeInfo)
          return `${result}${s}`
        }, ``)
      } else {
        return params.reduce((result: Obj, obj) => {
          object.assert(obj, sourceCodeInfo)
          return Object.assign(result, obj)
        }, {})
      }
    },
    validate: node => assertLength({ min: 1 }, node),
  },
  'empty?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      collection.assert(first, sourceCodeInfo)
      if (string.is(first)) {
        return first.length === 0
      }
      if (Array.isArray(first)) {
        return first.length === 0
      }
      return Object.keys(first).length === 0
    },
    validate: node => assertLength(1, node),
  },
  'every?': {
    evaluate: ([fn, coll], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      litsFunction.assert(fn, sourceCodeInfo)
      collection.assert(coll, sourceCodeInfo)

      if (Array.isArray(coll)) {
        return coll.every(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
      }
      if (string.is(coll)) {
        return coll.split(``).every(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
      }
      return Object.entries(coll).every(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
    },
    validate: node => assertLength(2, node),
  },
  'any?': {
    evaluate: ([fn, coll], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      litsFunction.assert(fn, sourceCodeInfo)
      collection.assert(coll, sourceCodeInfo)

      if (Array.isArray(coll)) {
        return coll.some(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
      }
      if (string.is(coll)) {
        return coll.split(``).some(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
      }
      return Object.entries(coll).some(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
    },
    validate: node => assertLength(2, node),
  },
  'not-any?': {
    evaluate: ([fn, coll], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      litsFunction.assert(fn, sourceCodeInfo)
      collection.assert(coll, sourceCodeInfo)

      if (Array.isArray(coll)) {
        return !coll.some(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
      }
      if (string.is(coll)) {
        return !coll.split(``).some(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
      }
      return !Object.entries(coll).some(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
    },
    validate: node => assertLength(2, node),
  },
  'not-every?': {
    evaluate: ([fn, coll], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      litsFunction.assert(fn, sourceCodeInfo)
      collection.assert(coll, sourceCodeInfo)

      if (Array.isArray(coll)) {
        return !coll.every(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
      }
      if (string.is(coll)) {
        return !coll.split(``).every(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
      }
      return !Object.entries(coll).every(elem => executeFunction(fn, [elem], sourceCodeInfo, contextStack))
    },
    validate: node => assertLength(2, node),
  },
}
