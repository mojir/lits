import type { Any, Coll, Obj, Seq } from '../interface'
import type { RegularExpression } from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { getAssertionError } from '../utils/getAssertionError'
import { REGEXP_SYMBOL } from '../utils/symbols'
import { isLitsFunction } from './litsFunction'

export function isAny(value: unknown): value is Any {
  // TODO weak test
  return value !== undefined
}
export function asAny(value: unknown, sourceCodeInfo?: SourceCodeInfo): Any {
  assertAny(value, sourceCodeInfo)
  return value
}
export function assertAny(value: unknown, sourceCodeInfo?: SourceCodeInfo): asserts value is Any {
  if (!isAny(value))
    throw getAssertionError('not undefined', value, sourceCodeInfo)
}

export function isSeq(value: unknown): value is Seq {
  return Array.isArray(value) || typeof value === 'string'
}
export function asSeq(value: unknown, sourceCodeInfo?: SourceCodeInfo): Seq {
  assertSeq(value, sourceCodeInfo)
  return value
}
export function assertSeq(value: unknown, sourceCodeInfo?: SourceCodeInfo): asserts value is Seq {
  if (!isSeq(value))
    throw getAssertionError('string or array', value, sourceCodeInfo)
}

export function isObj(value: unknown): value is Obj {
  return !(
    value === null
    || typeof value !== 'object'
    || Array.isArray(value)
    || value instanceof RegExp
    || isLitsFunction(value)
    || isRegularExpression(value)
  )
}
export function asObj(value: unknown, sourceCodeInfo?: SourceCodeInfo): Obj {
  assertObj(value, sourceCodeInfo)
  return value
}
export function assertObj(value: unknown, sourceCodeInfo?: SourceCodeInfo): asserts value is Obj {
  if (!isObj(value))
    throw getAssertionError('object', value, sourceCodeInfo)
}

export function isColl(value: unknown): value is Coll {
  return isSeq(value) || isObj(value)
}
export function asColl(value: unknown, sourceCodeInfo?: SourceCodeInfo): Coll {
  assertColl(value, sourceCodeInfo)
  return value
}
export function assertColl(value: unknown, sourceCodeInfo?: SourceCodeInfo): asserts value is Coll {
  if (!isColl(value))
    throw getAssertionError('string, array or object', value, sourceCodeInfo)
}

export function isRegularExpression(regexp: unknown): regexp is RegularExpression {
  if (regexp === null || typeof regexp !== 'object')
    return false

  return !!(regexp as RegularExpression)[REGEXP_SYMBOL]
}
export function asRegularExpression(value: unknown, sourceCodeInfo?: SourceCodeInfo): RegularExpression {
  assertRegularExpression(value, sourceCodeInfo)
  return value
}
export function assertRegularExpression(
  value: unknown,
  sourceCodeInfo?: SourceCodeInfo,
): asserts value is RegularExpression {
  if (!isRegularExpression(value))
    throw getAssertionError('RegularExpression', value, sourceCodeInfo)
}

export function isStringOrRegularExpression(value: unknown): value is string | RegularExpression {
  return isRegularExpression(value) || typeof value === 'string'
}
export function asStringOrRegularExpression(
  value: unknown,
  sourceCodeInfo?: SourceCodeInfo,
): string | RegularExpression {
  assertStringOrRegularExpression(value, sourceCodeInfo)
  return value
}
export function assertStringOrRegularExpression(
  value: unknown,
  sourceCodeInfo?: SourceCodeInfo,
): asserts value is string | RegularExpression {
  if (!isStringOrRegularExpression(value))
    throw getAssertionError('string or RegularExpression', value, sourceCodeInfo)
}
