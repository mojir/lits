import type { Any, Arr, Coll, Obj } from '../interface'
import type { NativeJsFunction, RegularExpression } from '../parser/interface'
import type { SourceCodeInfo } from '../tokenizer/interface'
import { asAny, isColl, isObj, isRegularExpression } from '../typeGuards/lits'
import { isNumber } from '../typeGuards/number'
import { asString } from '../typeGuards/string'
import { isUnknownRecord } from '../typeGuards'
import { FunctionType } from '../constants/constants'

export function collHasKey(coll: unknown, key: string | number): boolean {
  if (!isColl(coll))
    return false

  if (typeof coll === 'string' || Array.isArray(coll)) {
    if (!isNumber(key, { integer: true }))
      return false

    return key >= 0 && key < coll.length
  }
  return !!Object.getOwnPropertyDescriptor(coll, key)
}

type Type = 'null' | 'boolean' | 'number' | 'string' | 'object' | 'array' | 'regexp' | 'unknown'

const sortOrderByType: Record<Type, number> = {
  boolean: 0,
  number: 1,
  string: 2,
  array: 3,
  object: 4,
  regexp: 5,
  unknown: 6,
  null: 7,
}

function getType(value: unknown): Type {
  if (value === null)
    return 'null'
  else if (typeof value === 'boolean')
    return 'boolean'
  else if (typeof value === 'number')
    return 'number'
  else if (typeof value === 'string')
    return 'string'
  else if (Array.isArray(value))
    return 'array'
  else if (isObj(value))
    return 'object'
  else if (isRegularExpression(value))
    return 'regexp'
  else
    return 'unknown'
}

export function compare(a: unknown, b: unknown): number {
  const aType = getType(a)
  const bType = getType(b)
  if (aType !== bType)
    return Math.sign(sortOrderByType[aType] - sortOrderByType[bType])

  switch (aType) {
    case 'null':
      return 0
    case 'boolean':
      if (a === b)
        return 0

      return a === false ? -1 : 1
    case 'number':
      return Math.sign((a as number) - (b as number))
    case 'string': {
      const aString = a as string
      const bString = b as string
      return aString < bString ? -1 : aString > bString ? 1 : 0
    }
    case 'array': {
      const aArray = a as Arr
      const bArray = b as Arr
      if (aArray.length < bArray.length)
        return -1
      else if (aArray.length > bArray.length)
        return 1

      for (let i = 0; i < aArray.length; i += 1) {
        const innerComp = compare(aArray[i], bArray[i])
        if (innerComp !== 0)
          return innerComp
      }
      return 0
    }
    case 'object': {
      const aObj = a as Obj
      const bObj = b as Obj
      return Math.sign(Object.keys(aObj).length - Object.keys(bObj).length)
    }
    case 'regexp': {
      const aString = (a as RegularExpression).s
      const bString = (b as RegularExpression).s
      return aString < bString ? -1 : aString > bString ? 1 : 0
    }
    case 'unknown':
      return 0
  }
}

export function deepEqual(a: Any, b: Any, sourceCodeInfo?: SourceCodeInfo): boolean {
  if (a === b)
    return true

  if (typeof a === 'number' && typeof b === 'number')
    return Math.abs(a - b) < Number.EPSILON

  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length)
      return false

    for (let i = 0; i < a.length; i += 1) {
      if (!deepEqual(asAny(a[i], sourceCodeInfo), asAny(b[i], sourceCodeInfo), sourceCodeInfo))
        return false
    }
    return true
  }
  if (isRegularExpression(a) && isRegularExpression(b))
    return a.s === b.s && a.f === b.f

  if (isUnknownRecord(a) && isUnknownRecord(b)) {
    const aKeys = Object.keys(a)
    const bKeys = Object.keys(b)
    if (aKeys.length !== bKeys.length)
      return false

    for (let i = 0; i < aKeys.length; i += 1) {
      const key = asString(aKeys[i], sourceCodeInfo)
      if (!deepEqual(toAny(a[key]), toAny(b[key]), sourceCodeInfo))
        return false
    }
    return true
  }
  return false
}

export function toNonNegativeInteger(num: number): number {
  return Math.max(0, Math.ceil(num))
}

export function toAny(value: unknown): Any {
  return (value ?? null) as Any
}

function clone<T>(value: T): T {
  if (isObj(value)) {
    return Object.entries(value).reduce((result: Obj, entry) => {
      const [key, val] = entry
      result[key] = clone(val)
      return result
    }, {}) as T
  }
  if (Array.isArray(value))
    // eslint-disable-next-line ts/no-unsafe-return
    return value.map(item => clone(item)) as unknown as T

  return value
}

export function cloneColl<T extends Coll>(value: T): T {
  return clone(value)
}

export function createNativeJsFunction(fn: (...args: any[]) => unknown, name?: string): NativeJsFunction {
  return {
    __fn: true,
    f: {
      fn,
    },
    n: name,
    t: FunctionType.NativeJsFunction,
  }
}
