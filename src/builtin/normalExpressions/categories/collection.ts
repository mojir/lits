import { LitsFunction } from '../../..'
import { Type } from '../../../types/Type'
import { ContextStack } from '../../../ContextStack'
import { ExecuteFunction } from '../../../evaluator/interface'
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
      if (params.every(Type.isNotType)) {
        const [coll, key] = params
        const defaultValue = toAny(params[2])
        stringOrNumber.assert(key, debugInfo)
        if (coll === null) {
          return defaultValue
        }
        collection.assert(coll, debugInfo)
        const result = get(coll, key)
        return result === undefined ? defaultValue : result
      } else {
        const collType = Type.of(params[0])
        const keyType = Type.of(params[1])
        const defaultValueType = Type.isType(params[2])
          ? params[2]
          : params[2] === undefined
          ? Type.nil
          : Type.of(params[2])

        collType.assertIs(Type.or(Type.array, Type.string, Type.object).nilable(), debugInfo)
        keyType.assertIs(Type.or(Type.string, Type.float, Type.nil), debugInfo)

        if (collType.is(Type.nil)) {
          return defaultValueType
        }

        if (collType.is(Type.string)) {
          return Type.or(Type.string.nilable(), defaultValueType)
        }

        return Type.unknown
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2, max: 3 }, arity, `get`, debugInfo),
  },
  'get-in': {
    evaluate: (params, debugInfo): Any => {
      if (params.every(Type.isNotType)) {
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
      } else {
        const collType = Type.of(params[0])
        const keysType = Type.of(params[1])
        collType.assertIs(Type.or(Type.array, Type.string, Type.object).nilable(), debugInfo)
        keysType.assertIs(Type.array.nilable(), debugInfo)

        if (keysType.is(Type.nil)) {
          return collType
        }

        return Type.unknown
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2, max: 3 }, arity, `___`, debugInfo),
  },
  count: {
    evaluate: ([coll], debugInfo): number | Type => {
      if (Type.isNotType(coll)) {
        if (typeof coll === `string`) {
          return coll.length
        }
        collection.assert(coll, debugInfo)
        if (Array.isArray(coll)) {
          return coll.length
        }
        return Object.keys(coll).length
      } else {
        const collType = Type.of(coll)
        collType.assertIs(Type.or(Type.array, Type.string, Type.object), debugInfo)
        return collType.is(Type.or(Type.emptyArray, Type.emptyString, Type.emptyObject))
          ? Type.zero
          : collType.is(Type.or(Type.nonEmptyArray, Type.nonEmptyString, Type.nonEmptyObject))
          ? Type.positiveInteger
          : Type.nonNegativeInteger
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `count`, debugInfo),
  },
  'contains?': {
    evaluate: (params, debugInfo): boolean | Type => {
      if (params.every(Type.isNotType)) {
        const [coll, key] = params
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
      } else {
        const collType = Type.of(params[0])
        return collType.is(Type.or(Type.emptyArray, Type.emptyString, Type.emptyObject)) ? Type.false : Type.boolean
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `contains?`, debugInfo),
  },
  'has?': {
    evaluate: (params, debugInfo): boolean | Type => {
      if (params.every(Type.isNotType)) {
        const [coll, value] = params
        collection.assert(coll, debugInfo)

        if ((array.is(coll) && coll.some(Type.isType)) || (object.is(coll) && Object.values(coll).some(Type.isType))) {
          return Type.boolean
        }

        if (array.is(coll)) {
          return coll.includes(value)
        }
        if (string.is(coll)) {
          return string.is(value) ? coll.split(``).includes(value) : false
        }
        return Object.values(coll).includes(value)
      } else {
        const collType = Type.of(params[0])
        return collType.is(Type.or(Type.emptyArray, Type.emptyString, Type.emptyObject)) ? Type.false : Type.boolean
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `has?`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `has-some?`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `has-every?`, debugInfo),
  },
  assoc: {
    evaluate: ([coll, key, value], debugInfo): Coll => {
      collection.assert(coll, debugInfo)
      stringOrNumber.assert(key, debugInfo)
      any.assert(value, debugInfo)
      return assoc(coll, key, value, debugInfo)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(3, arity, `assoc`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams(3, arity, `assoc-in`, debugInfo),
  },
  update: {
    evaluate: ([coll, key, fn, ...params], debugInfo, contextStack, { executeFunction }): Coll => {
      collection.assert(coll, debugInfo)
      stringOrNumber.assert(key, debugInfo)
      litsFunction.assert(fn, debugInfo)
      return update(coll, key, fn, params, contextStack, executeFunction, debugInfo)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 3 }, arity, `update`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 3 }, arity, `update-in`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1 }, arity, `concat`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `not-empty`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `every?`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `any?`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `not-any?`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `not-every?`, debugInfo),
  },
}
