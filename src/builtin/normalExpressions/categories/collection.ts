import { LitsFunction } from '../../..'
import { ContextStack, ExecuteFunction } from '../../../evaluator/interface'
import { Any, Arr, Coll, Obj } from '../../../interface'
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

function cloneAndGetMeta(originalColl: Coll, keys: Arr): { coll: Coll; innerCollMeta: CollMeta } {
  const coll = cloneColl(originalColl)

  const butLastKeys = keys.slice(0, keys.length - 1)

  const innerCollMeta = butLastKeys.reduce(
    (result: CollMeta, key) => {
      const resultColl = result.coll

      let newResultColl: Coll
      if (isArr(resultColl)) {
        assertNumber(key)
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        newResultColl = asColl(resultColl[key])
      } else {
        assertObj(resultColl)
        assertString(key)
        if (!collHasKey(result.coll, key)) {
          resultColl[key] = {}
        }
        newResultColl = asColl(resultColl[key])
      }

      return { coll: newResultColl, parent: resultColl }
    },
    { coll, parent: {} },
  )
  return { coll, innerCollMeta }
}

function get(coll: Coll, key: string | number): Any | undefined {
  if (isArr(coll)) {
    assertInteger(key)
    if (key < coll.length) {
      return toAny(coll[key])
    }
  } else if (isObj(coll)) {
    assertString(key)
    if (collHasKey(coll, key)) {
      return toAny(coll[key])
    }
  } else {
    assertInteger(key)
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
  contextStack: ContextStack,
  executeFunction: ExecuteFunction,
): Coll {
  if (isObj(coll)) {
    assertString(key)
    const result = { ...coll }
    result[key] = executeFunction(fn, [result[key], ...params], contextStack)
    return result
  } else {
    assertNumber(key)
    const intKey = toNonNegativeInteger(key)
    assertMax(intKey, coll.length)
    if (Array.isArray(coll)) {
      const result = coll.map((elem, index) => {
        if (intKey === index) {
          return executeFunction(fn, [elem, ...params], contextStack)
        }
        return elem
      })
      if (intKey === coll.length) {
        result[intKey] = executeFunction(fn, [undefined, ...params], contextStack)
      }
      return result
    } else {
      const result = coll.split(``).map((elem, index) => {
        if (intKey === index) {
          return asChar(executeFunction(fn, [elem, ...params], contextStack))
        }
        return elem
      })
      if (intKey === coll.length) {
        result[intKey] = asChar(executeFunction(fn, [undefined, ...params], contextStack))
      }
      return result.join(``)
    }
  }
}

function assoc(coll: Coll, key: string | number, value: Any) {
  assertColl(coll)
  assertStringOrNumber(key)
  if (Array.isArray(coll) || typeof coll === `string`) {
    assertInteger(key)
    assertNumberGte(key, 0)
    assertNumberLte(key, coll.length)
    if (typeof coll === `string`) {
      assertChar(value)
      return `${coll.slice(0, key)}${value}${coll.slice(key + 1)}`
    }
    const copy = [...coll]
    copy[key] = value
    return copy
  }
  assertString(key)
  const copy = { ...coll }
  copy[key] = value
  return copy
}

export const collectionNormalExpression: BuiltinNormalExpressions = {
  get: {
    evaluate: (params: Arr): Any => {
      const [coll, key] = params
      const defaultValue = toAny(params[2])
      assertColl(coll)
      assertStringOrNumber(key)
      const result = get(coll, key)
      return result === undefined ? defaultValue : result
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  'get-in': {
    evaluate: (params: Arr): Any => {
      let coll = params[0]
      const keys = params[1]
      const defaultValue = toAny(params[2])
      assertColl(coll)
      assertArr(keys)
      for (const key of keys) {
        assertStringOrNumber(key)
        if (isColl(coll)) {
          coll = get(coll, key)
        } else {
          return defaultValue
        }
      }
      return isAny(coll) ? coll : defaultValue
    },
    validate: node => assertLength({ min: 2, max: 3 }, node),
  },
  count: {
    evaluate: ([coll]): number => {
      if (typeof coll === `string`) {
        return coll.length
      }
      assertColl(coll)
      if (Array.isArray(coll)) {
        return coll.length
      }
      return Object.keys(coll).length
    },
    validate: node => assertLength(1, node),
  },
  'contains?': {
    evaluate: ([coll, key]): boolean => {
      assertColl(coll)
      assertStringOrNumber(key)
      if (isSeq(coll)) {
        if (!Number.isInteger(key)) {
          return false
        }
        assertInteger(key)
        return key >= 0 && key < coll.length
      }
      return !!Object.getOwnPropertyDescriptor(coll, key)
    },
    validate: node => assertLength(2, node),
  },
  'has?': {
    evaluate: ([coll, value]): boolean => {
      assertColl(coll)
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
    evaluate: ([coll, seq]): boolean => {
      assertColl(coll)
      assertSeq(seq)
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
    evaluate: ([coll, seq]): boolean => {
      assertColl(coll)
      assertSeq(seq)
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
    evaluate: ([coll, key, value]): Coll => {
      assertColl(coll)
      assertStringOrNumber(key)
      assertAny(value)
      return assoc(coll, key, value)
    },
    validate: node => assertLength(3, node),
  },
  'assoc-in': {
    evaluate: ([originalColl, keys, value]): Coll => {
      assertColl(originalColl)
      assertArr(keys)
      assertAny(value)

      if (keys.length === 1) {
        assertStringOrNumber(keys[0])
        return assoc(originalColl, keys[0], value)
      }

      const { coll, innerCollMeta } = cloneAndGetMeta(originalColl, keys)

      const lastKey = asStringOrNumber(keys[keys.length - 1])
      const parentKey = asStringOrNumber(keys[keys.length - 2])

      if (isArr(innerCollMeta.parent)) {
        assertNumber(parentKey)
        innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value)
      } else {
        assertString(parentKey)
        innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value)
      }

      return coll
    },
    validate: node => assertLength(3, node),
  },
  update: {
    evaluate: ([coll, key, fn, ...params], contextStack, { executeFunction }): Coll => {
      assertColl(coll)
      assertStringOrNumber(key)
      assertLitsFunction(fn)
      return update(coll, key, fn, params, contextStack, executeFunction)
    },
    validate: node => assertLength({ min: 3 }, node),
  },
  'update-in': {
    evaluate: ([originalColl, keys, fn, ...params], contextStack, { executeFunction }): Coll => {
      assertColl(originalColl)
      assertArr(keys)
      assertLitsFunction(fn)

      if (keys.length === 1) {
        assertStringOrNumber(keys[0])
        return update(originalColl, keys[0], fn, params, contextStack, executeFunction)
      }

      const { coll, innerCollMeta } = cloneAndGetMeta(originalColl, keys)

      const lastKey = asStringOrNumber(keys[keys.length - 1])
      const parentKey = asStringOrNumber(keys[keys.length - 2])

      if (isArr(innerCollMeta.parent)) {
        assertNumber(parentKey)
        innerCollMeta.parent[parentKey] = update(innerCollMeta.coll, lastKey, fn, params, contextStack, executeFunction)
      } else {
        assertString(parentKey)
        innerCollMeta.parent[parentKey] = update(innerCollMeta.coll, lastKey, fn, params, contextStack, executeFunction)
      }

      return coll
    },
    validate: node => assertLength({ min: 3 }, node),
  },
  concat: {
    evaluate: (params: Arr): Any => {
      assertColl(params[0])
      if (isArr(params[0])) {
        return params.reduce((result: Arr, arr) => {
          assertArr(arr)
          return result.concat(arr)
        }, [])
      } else if (isString(params[0])) {
        return params.reduce((result: string, s) => {
          assertString(s)
          return `${result}${s}`
        }, ``)
      } else {
        return params.reduce((result: Obj, obj) => {
          assertObj(obj)
          return Object.assign(result, obj)
        }, {})
      }
    },
    validate: node => assertLength({ min: 1 }, node),
  },
  'empty?': {
    evaluate: ([first]): boolean => {
      assertColl(first)
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
    evaluate: ([fn, coll], contextStack, { executeFunction }): boolean => {
      assertLitsFunction(fn)
      assertColl(coll)

      if (Array.isArray(coll)) {
        return coll.every(elem => executeFunction(fn, [elem], contextStack))
      }
      if (isString(coll)) {
        return coll.split(``).every(elem => executeFunction(fn, [elem], contextStack))
      }
      return Object.entries(coll).every(elem => executeFunction(fn, [elem], contextStack))
    },
    validate: node => assertLength(2, node),
  },
  'any?': {
    evaluate: ([fn, coll], contextStack, { executeFunction }): boolean => {
      assertLitsFunction(fn)
      assertColl(coll)

      if (Array.isArray(coll)) {
        return coll.some(elem => executeFunction(fn, [elem], contextStack))
      }
      if (isString(coll)) {
        return coll.split(``).some(elem => executeFunction(fn, [elem], contextStack))
      }
      return Object.entries(coll).some(elem => executeFunction(fn, [elem], contextStack))
    },
    validate: node => assertLength(2, node),
  },
  'not-any?': {
    evaluate: ([fn, coll], contextStack, { executeFunction }): boolean => {
      assertLitsFunction(fn)
      assertColl(coll)

      if (Array.isArray(coll)) {
        return !coll.some(elem => executeFunction(fn, [elem], contextStack))
      }
      if (isString(coll)) {
        return !coll.split(``).some(elem => executeFunction(fn, [elem], contextStack))
      }
      return !Object.entries(coll).some(elem => executeFunction(fn, [elem], contextStack))
    },
    validate: node => assertLength(2, node),
  },
  'not-every?': {
    evaluate: ([fn, coll], contextStack, { executeFunction }): boolean => {
      assertLitsFunction(fn)
      assertColl(coll)

      if (Array.isArray(coll)) {
        return !coll.every(elem => executeFunction(fn, [elem], contextStack))
      }
      if (isString(coll)) {
        return !coll.split(``).every(elem => executeFunction(fn, [elem], contextStack))
      }
      return !Object.entries(coll).every(elem => executeFunction(fn, [elem], contextStack))
    },
    validate: node => assertLength(2, node),
  },
}
