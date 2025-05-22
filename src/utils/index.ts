import type { Any, Coll, Obj } from '../interface'
import type { FunctionLike, NativeJsFunction, NormalExpressionNodeWithName } from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { asAny, isColl, isObj, isRegularExpression } from '../typeGuards/lits'
import { isNumber } from '../typeGuards/number'
import { asString, assertStringOrNumber } from '../typeGuards/string'
import { isUnknownRecord } from '../typeGuards'
import { LitsError } from '../errors'
import type { ParamCount } from '../builtin/interface'
import { getNodeTypeName } from '../constants/constants'
import { FUNCTION_SYMBOL } from './symbols'
import { valueToString } from './debug/debugTools'

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

export function deepEqual(a: unknown, b: unknown, sourceCodeInfo?: SourceCodeInfo): boolean {
  if (a === b)
    return true

  if (typeof a === 'number' && typeof b === 'number')
    return approxEqual(a, b)

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
      if (!deepEqual(a[key], b[key], sourceCodeInfo))
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
    nativeFn: {
      fn,
    },
    name,
    functionType: 'NativeJsFunction',
    paramCount: {},
  }
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

export const EPSILON = 1e-10

export function approxEqual(a: number, b: number, epsilon: number = EPSILON): boolean {
  if (a === b) {
    return true
  }

  const diff = Math.abs(a - b)

  if (a === 0 || b === 0 || diff < epsilon) {
    // Use absolute error for values near zero
    return diff < epsilon
  }
  const absA = Math.abs(a)
  const absB = Math.abs(b)

  // Use relative error for larger values
  return diff / (absA + absB) < epsilon
}

export function approxZero(value: number): boolean {
  return Math.abs(value) < EPSILON
}

export function mergeParamCounts(count: ParamCount, node: NormalExpressionNodeWithName): void {
  const length = node[1][1].length
  if (typeof count === 'number') {
    if (length !== count) {
      const name = getNodeTypeName(node[0])
      throw new LitsError(
        `Wrong number of arguments to "${name}", expected ${count}, got ${valueToString(length)}.`,
        node[2],
      )
    }
  }
  else {
    const { min, max, even, odd } = count
    if (even) {
      const name = getNodeTypeName(node[0])
      if (length % 2 !== 0) {
        throw new LitsError(
          `Wrong number of arguments to "${name}",, expected an even number, got ${valueToString(length)}.`,
          node[2],
        )
      }
    }

    if (odd) {
      if (length % 2 !== 1) {
        const name = getNodeTypeName(node[0])
        throw new LitsError(
          `Wrong number of arguments to "${name}",, expected an odd number, got ${valueToString(length)}.`,
          node[2],
        )
      }
    }

    if (typeof min === 'number' && length < min) {
      const name = getNodeTypeName(node[0])
      throw new LitsError(
        `Wrong number of arguments to "${name}", expected at least ${min}, got ${valueToString(length)}.`,
        node[2],
      )
    }

    if (typeof max === 'number' && length > max) {
      const name = getNodeTypeName(node[0])
      throw new LitsError(
        `Wrong number of arguments to "${name}", expected at most ${max}, got ${valueToString(length)}.`,
        node[2],
      )
    }
  }
}

export function assertNumberOfParams(count: ParamCount, node: NormalExpressionNodeWithName): void {
  const length = node[1][1].length
  if (typeof count === 'number') {
    if (length !== count) {
      const name = getNodeTypeName(node[0])
      throw new LitsError(
        `Wrong number of arguments to "${name}", expected ${count}, got ${valueToString(length)}.`,
        node[2],
      )
    }
  }
  else {
    const { min, max, even, odd } = count
    if (even) {
      const name = getNodeTypeName(node[0])
      if (length % 2 !== 0) {
        throw new LitsError(
          `Wrong number of arguments to "${name}",, expected an even number, got ${valueToString(length)}.`,
          node[2],
        )
      }
    }

    if (odd) {
      if (length % 2 !== 1) {
        const name = getNodeTypeName(node[0])
        throw new LitsError(
          `Wrong number of arguments to "${name}",, expected an odd number, got ${valueToString(length)}.`,
          node[2],
        )
      }
    }

    if (typeof min === 'number' && length < min) {
      const name = getNodeTypeName(node[0])
      throw new LitsError(
        `Wrong number of arguments to "${name}", expected at least ${min}, got ${valueToString(length)}.`,
        node[2],
      )
    }

    if (typeof max === 'number' && length > max) {
      const name = getNodeTypeName(node[0])
      throw new LitsError(
        `Wrong number of arguments to "${name}", expected at most ${max}, got ${valueToString(length)}.`,
        node[2],
      )
    }
  }
}

function paramCountAccepts(paramsCount: ParamCount, nbrOfParams: number): boolean {
  if (typeof paramsCount === 'number') {
    return paramsCount === nbrOfParams
  }
  const { min, max, even, odd } = paramsCount
  if (even && nbrOfParams % 2 !== 0) {
    return false
  }
  if (odd && nbrOfParams % 2 !== 1) {
    return false
  }
  if (typeof min === 'number' && nbrOfParams < min) {
    return false
  }
  if (typeof max === 'number' && nbrOfParams > max) {
    return false
  }
  return true
}

export function paramCountAcceptsMin(paramsCount: ParamCount, nbrOfParams: number): boolean {
  if (typeof paramsCount === 'number') {
    return nbrOfParams >= paramsCount
  }
  const { min } = paramsCount
  if (typeof min === 'number' && nbrOfParams < min) {
    return false
  }
  return true
}

export function getCommonParamCount(params: FunctionLike[]): ParamCount | null {
  return params.reduce((acc: ParamCount | null, param) => {
    if (acc === null) {
      return null
    }
    const paramCount = (typeof param === 'number' || isColl(param)) ? 1 : param.paramCount
    if (typeof acc === 'number' && typeof paramCount === 'number') {
      return acc === paramCount ? acc : null
    }
    if (typeof paramCount === 'number') {
      if (paramCountAccepts(acc, paramCount)) {
        return paramCount
      }
      return null
    }
    if (typeof acc === 'number') {
      if (paramCountAccepts(paramCount, acc)) {
        return acc
      }
      return null
    }
    const { min: aMin, max: aMax, even: aEven, odd: aOdd } = paramCount
    const { min: bMin, max: bMax, even: bEven, odd: bOdd } = acc
    const min = typeof aMin === 'number' && typeof bMin === 'number'
      ? Math.max(aMin, bMin)
      : typeof aMin === 'number' ? aMin : typeof bMin === 'number' ? bMin : undefined
    const max = typeof aMax === 'number' && typeof bMax === 'number'
      ? Math.min(aMax, bMax)
      : typeof aMax === 'number' ? aMax : typeof bMax === 'number' ? bMax : undefined
    const even = aEven ?? bEven
    const odd = aOdd ?? bOdd
    if (min !== undefined && max !== undefined && min > max) {
      return null
    }
    if (even && odd) {
      return null
    }
    if (odd && min !== undefined && min < 1) {
      return null
    }
    return { min, max, even, odd }
  }, {})
}

export function getParamCount(param: FunctionLike): ParamCount {
  return (typeof param === 'number' || isColl(param)) ? 1 : param.paramCount
}

export function paramCountMinus(paramCount: ParamCount, count: number): ParamCount {
  if (typeof paramCount === 'number') {
    return paramCount - count
  }
  const min = paramCount.min === undefined ? undefined : paramCount.min - count
  const max = paramCount.max === undefined ? undefined : paramCount.max - count
  const even = paramCount.even === undefined ? undefined : count % 2 === 0 ? true : undefined
  const odd = paramCount.odd === undefined ? undefined : count % 2 === 0 ? true : undefined
  return { min, max, even, odd }
}
