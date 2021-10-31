import { LitsFunction } from '../../..'
import { ContextStack, ExecuteFunction } from '../../../evaluator/interface'
import { Any, Arr, Coll, Obj } from '../../../interface'
import { TokenMeta } from '../../../tokenizer/interface'
import {
  assertArr,
  assertChar,
  assertColl,
  assertInteger,
  assertLength,
  assertLitsFunction,
  assertMax,
  assertNumber,
  assertNumberGte,
  assertNumberLte,
  assertObj,
  assertString,
  assertStringOrNumber,
  isArr,
  isObj,
  isSeq,
  isString,
  toNonNegativeInteger,
  toAny,
  asChar,
  collHasKey,
  isColl,
  isAny,
  cloneColl,
  asColl,
  assertAny,
  asStringOrNumber,
  assertSeq,
  isChar,
} from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

type CollMeta = {
  coll: Coll
  parent: Obj | Arr
}

function cloneAndGetMeta(originalColl: Coll, keys: Arr, meta: TokenMeta): { coll: Coll; innerCollMeta: CollMeta } {
  const coll = cloneColl(originalColl)

  const butLastKeys = keys.slice(0, keys.length - 1)

  const innerCollMeta = butLastKeys.reduce(
    (result: CollMeta, key) => {
      const resultColl = result.coll

      let newResultColl: Coll
      if (isArr(resultColl)) {
        assertNumber(key, meta)
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        newResultColl = asColl(resultColl[key], meta)
      } else {
        assertObj(resultColl, meta)
        assertString(key, meta)
        if (!collHasKey(result.coll, key)) {
          resultColl[key] = {}
        }
        newResultColl = asColl(resultColl[key], meta)
      }

      return { coll: newResultColl, parent: resultColl }
    },
    { coll, parent: {} },
  )
  return { coll, innerCollMeta }
}

function get(coll: Coll, key: string | number, meta: TokenMeta): Any | undefined {
  if (isArr(coll)) {
    assertInteger(key, meta)
    if (key < coll.length) {
      return toAny(coll[key])
    }
  } else if (isObj(coll)) {
    assertString(key, meta)
    if (collHasKey(coll, key)) {
      return toAny(coll[key])
    }
  } else {
    assertInteger(key, meta)
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
  meta: TokenMeta,
  contextStack: ContextStack,
  executeFunction: ExecuteFunction,
): Coll {
  if (isObj(coll)) {
    assertString(key, meta)
    const result = { ...coll }
    result[key] = executeFunction(fn, [result[key], ...params], meta, contextStack)
    return result
  } else {
    assertNumber(key, meta)
    const intKey = toNonNegativeInteger(key)
    assertMax(intKey, coll.length, meta)
    if (Array.isArray(coll)) {
      const result = coll.map((elem, index) => {
        if (intKey === index) {
          return executeFunction(fn, [elem, ...params], meta, contextStack)
        }
        return elem
      })
      if (intKey === coll.length) {
        result[intKey] = executeFunction(fn, [undefined, ...params], meta, contextStack)
      }
      return result
    } else {
      const result = coll.split(``).map((elem, index) => {
        if (intKey === index) {
          return asChar(executeFunction(fn, [elem, ...params], meta, contextStack), meta)
        }
        return elem
      })
      if (intKey === coll.length) {
        result[intKey] = asChar(executeFunction(fn, [undefined, ...params], meta, contextStack), meta)
      }
      return result.join(``)
    }
  }
}

function assoc(coll: Coll, key: string | number, value: Any, meta: TokenMeta) {
  assertColl(coll, meta)
  assertStringOrNumber(key, meta)
  if (Array.isArray(coll) || typeof coll === `string`) {
    assertInteger(key, meta)
    assertNumberGte(key, 0, meta)
    assertNumberLte(key, coll.length, meta)
    if (typeof coll === `string`) {
      assertChar(value, meta)
      return `${coll.slice(0, key)}${value}${coll.slice(key + 1)}`
    }
    const copy = [...coll]
    copy[key] = value
    return copy
  }
  assertString(key, meta)
  const copy = { ...coll }
  copy[key] = value
  return copy
}

export const collectionNormalExpression: BuiltinNormalExpressions = {
  get: {
    evaluate: (params, meta) => {
      const [coll, key] = params
      const defaultValue = toAny(params[2])
      assertColl(coll, meta)
      assertStringOrNumber(key, meta)
      const result = get(coll, key, meta)
      return result === undefined ? defaultValue : result
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  'get-in': {
    evaluate: (params, meta): Any => {
      let coll = params[0]
      const keys = params[1]
      const defaultValue = toAny(params[2])
      assertColl(coll, meta)
      assertArr(keys, meta)
      for (const key of keys) {
        assertStringOrNumber(key, meta)
        if (isColl(coll)) {
          coll = get(coll, key, meta)
        } else {
          return defaultValue
        }
      }
      return isAny(coll) ? coll : defaultValue
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  count: {
    evaluate: ([coll], meta): number => {
      if (typeof coll === `string`) {
        return coll.length
      }
      assertColl(coll, meta)
      if (Array.isArray(coll)) {
        return coll.length
      }
      return Object.keys(coll).length
    },
    validate: node => assertLength(1, node),
  },
  'contains?': {
    evaluate: ([coll, key], meta): boolean => {
      assertColl(coll, meta)
      assertStringOrNumber(key, meta)
      if (isSeq(coll)) {
        if (!Number.isInteger(key)) {
          return false
        }
        assertInteger(key, meta)
        return key >= 0 && key < coll.length
      }
      return !!Object.getOwnPropertyDescriptor(coll, key)
    },
    validate: node => assertLength(2, node),
  },
  'has?': {
    evaluate: ([coll, value], meta): boolean => {
      assertColl(coll, meta)
      if (isArr(coll)) {
        return coll.includes(value)
      }
      if (isString(coll)) {
        return isString(value) ? coll.split(``).includes(value) : false
      }
      return Object.values(coll).includes(value)
    },
    validate: node => assertLength(2, node),
  },
  'has-some?': {
    evaluate: ([coll, seq], meta): boolean => {
      assertColl(coll, meta)
      assertSeq(seq, meta)
      if (isArr(coll)) {
        for (const value of seq) {
          if (coll.includes(value)) {
            return true
          }
        }
        return false
      }
      if (isString(coll)) {
        for (const value of seq) {
          if (isChar(value) ? coll.split(``).includes(value) : false) {
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
    evaluate: ([coll, seq], meta): boolean => {
      assertColl(coll, meta)
      assertSeq(seq, meta)
      if (isArr(coll)) {
        for (const value of seq) {
          if (!coll.includes(value)) {
            return false
          }
        }
        return true
      }
      if (isString(coll)) {
        for (const value of seq) {
          if (!isChar(value) || !coll.split(``).includes(value)) {
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
    evaluate: ([coll, key, value], meta): Coll => {
      assertColl(coll, meta)
      assertStringOrNumber(key, meta)
      assertAny(value, meta)
      return assoc(coll, key, value, meta)
    },
    validate: node => assertLength(3, node),
  },
  'assoc-in': {
    evaluate: ([originalColl, keys, value], meta): Coll => {
      assertColl(originalColl, meta)
      assertArr(keys, meta)
      assertAny(value, meta)

      if (keys.length === 1) {
        assertStringOrNumber(keys[0], meta)
        return assoc(originalColl, keys[0], value, meta)
      }

      const { coll, innerCollMeta } = cloneAndGetMeta(originalColl, keys, meta)

      const lastKey = asStringOrNumber(keys[keys.length - 1], meta)
      const parentKey = asStringOrNumber(keys[keys.length - 2], meta)

      if (isArr(innerCollMeta.parent)) {
        assertNumber(parentKey, meta)
        innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, meta)
      } else {
        assertString(parentKey, meta)
        innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, meta)
      }

      return coll
    },
    validate: node => assertLength(3, node),
  },
  update: {
    evaluate: ([coll, key, fn, ...params], meta, contextStack, { executeFunction }): Coll => {
      assertColl(coll, meta)
      assertStringOrNumber(key, meta)
      assertLitsFunction(fn, meta)
      return update(coll, key, fn, params, meta, contextStack, executeFunction)
    },
    validate: node => assertLength({ min: 3 }, node),
  },
  'update-in': {
    evaluate: ([originalColl, keys, fn, ...params], meta, contextStack, { executeFunction }): Coll => {
      assertColl(originalColl, meta)
      assertArr(keys, meta)
      assertLitsFunction(fn, meta)

      if (keys.length === 1) {
        assertStringOrNumber(keys[0], meta)
        return update(originalColl, keys[0], fn, params, meta, contextStack, executeFunction)
      }

      const { coll, innerCollMeta } = cloneAndGetMeta(originalColl, keys, meta)

      const lastKey = asStringOrNumber(keys[keys.length - 1], meta)
      const parentKey = asStringOrNumber(keys[keys.length - 2], meta)

      if (isArr(innerCollMeta.parent)) {
        assertNumber(parentKey, meta)
        innerCollMeta.parent[parentKey] = update(
          innerCollMeta.coll,
          lastKey,
          fn,
          params,
          meta,
          contextStack,
          executeFunction,
        )
      } else {
        assertString(parentKey, meta)
        innerCollMeta.parent[parentKey] = update(
          innerCollMeta.coll,
          lastKey,
          fn,
          params,
          meta,
          contextStack,
          executeFunction,
        )
      }

      return coll
    },
    validate: node => assertLength({ min: 3 }, node),
  },
  concat: {
    evaluate: (params, meta): Any => {
      assertColl(params[0], meta)
      if (isArr(params[0])) {
        return params.reduce((result: Arr, arr) => {
          assertArr(arr, meta)
          return result.concat(arr)
        }, [])
      } else if (isString(params[0])) {
        return params.reduce((result: string, s) => {
          assertString(s, meta)
          return `${result}${s}`
        }, ``)
      } else {
        return params.reduce((result: Obj, obj) => {
          assertObj(obj, meta)
          return Object.assign(result, obj)
        }, {})
      }
    },
    validate: node => assertLength({ min: 1 }, node),
  },
  'empty?': {
    evaluate: ([first], meta): boolean => {
      assertColl(first, meta)
      if (isString(first)) {
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
    evaluate: ([fn, coll], meta, contextStack, { executeFunction }): boolean => {
      assertLitsFunction(fn, meta)
      assertColl(coll, meta)

      if (Array.isArray(coll)) {
        return coll.every(elem => executeFunction(fn, [elem], meta, contextStack))
      }
      if (isString(coll)) {
        return coll.split(``).every(elem => executeFunction(fn, [elem], meta, contextStack))
      }
      return Object.entries(coll).every(elem => executeFunction(fn, [elem], meta, contextStack))
    },
    validate: node => assertLength(2, node),
  },
  'any?': {
    evaluate: ([fn, coll], meta, contextStack, { executeFunction }): boolean => {
      assertLitsFunction(fn, meta)
      assertColl(coll, meta)

      if (Array.isArray(coll)) {
        return coll.some(elem => executeFunction(fn, [elem], meta, contextStack))
      }
      if (isString(coll)) {
        return coll.split(``).some(elem => executeFunction(fn, [elem], meta, contextStack))
      }
      return Object.entries(coll).some(elem => executeFunction(fn, [elem], meta, contextStack))
    },
    validate: node => assertLength(2, node),
  },
  'not-any?': {
    evaluate: ([fn, coll], meta, contextStack, { executeFunction }): boolean => {
      assertLitsFunction(fn, meta)
      assertColl(coll, meta)

      if (Array.isArray(coll)) {
        return !coll.some(elem => executeFunction(fn, [elem], meta, contextStack))
      }
      if (isString(coll)) {
        return !coll.split(``).some(elem => executeFunction(fn, [elem], meta, contextStack))
      }
      return !Object.entries(coll).some(elem => executeFunction(fn, [elem], meta, contextStack))
    },
    validate: node => assertLength(2, node),
  },
  'not-every?': {
    evaluate: ([fn, coll], meta, contextStack, { executeFunction }): boolean => {
      assertLitsFunction(fn, meta)
      assertColl(coll, meta)

      if (Array.isArray(coll)) {
        return !coll.every(elem => executeFunction(fn, [elem], meta, contextStack))
      }
      if (isString(coll)) {
        return !coll.split(``).every(elem => executeFunction(fn, [elem], meta, contextStack))
      }
      return !Object.entries(coll).every(elem => executeFunction(fn, [elem], meta, contextStack))
    },
    validate: node => assertLength(2, node),
  },
}
