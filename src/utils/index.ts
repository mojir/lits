import type { Any, Coll, Obj } from '../interface'
import type { NativeJsFunction } from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { asAny, isColl, isObj, isRegularExpression } from '../typeGuards/lits'
import { isNumber } from '../typeGuards/number'
import { asString, assertStringOrNumber } from '../typeGuards/string'
import { isUnknownRecord } from '../typeGuards'
import { FunctionType } from '../constants/constants'
import { LitsError } from '../errors'
import { FUNCTION_SYMBOL } from './symbols'

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

export function compare<T extends string | number>(a: T, b: T, sourceCodeInfo: SourceCodeInfo | undefined): number {
  assertStringOrNumber(a, sourceCodeInfo)
  assertStringOrNumber(b, sourceCodeInfo)

  if (typeof a === 'string' && typeof b === 'string') {
    return a < b ? -1 : a > b ? 1 : 0
  }
  if (typeof a === 'number' && typeof b === 'number') {
    return Math.sign((a) - (b))
  }
  throw new LitsError(`Cannot compare values of different types: ${typeof a} and ${typeof b}`, sourceCodeInfo)
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
    [FUNCTION_SYMBOL]: true,
    f: {
      fn,
    },
    n: name,
    t: FunctionType.NativeJsFunction,
  }
}

export function arrayToPairs<T>(arr: T[]): T[][] {
  const pairs: T[][] = []
  for (let i = 0; i < arr.length; i += 2)
    pairs.push([arr[i]!, arr[i + 1]!])

  return pairs
}

export function joinSets<T>(...results: Set<T>[]): Set<T> {
  const result = new Set<T>()
  for (const symbols of results)
    symbols.forEach(symbol => result.add(symbol))

  return result
}

export function addToSet<T>(target: Set<T>, source: Set<T>): void {
  source.forEach(symbol => target.add(symbol))
}
