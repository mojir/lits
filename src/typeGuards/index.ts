import { LitsError } from '../errors'
import type { UnknownRecord } from '../interface'
import type { SourceCodeInfo } from '../tokenizer/token'
import { valueToString } from '../utils/debug/debugTools'
import { getSourceCodeInfo } from '../utils/debug/getSourceCodeInfo'

function isNonUndefined<T>(value: T | undefined): value is T {
  return value !== undefined
}

export function asNonUndefined<T>(value: T | undefined, sourceCodeInfo?: SourceCodeInfo): T {
  assertNonUndefined(value, sourceCodeInfo)
  return value
}

export function assertNonUndefined<T>(value: T | undefined, sourceCodeInfo?: SourceCodeInfo): asserts value is T {
  if (!isNonUndefined(value))
    throw new LitsError('Unexpected undefined', getSourceCodeInfo(value, sourceCodeInfo))
}

export function isUnknownRecord(value: unknown): value is Record<string, unknown> {
  return value !== null && typeof value === 'object' && !Array.isArray(value)
}

export function assertUnknownRecord(value: unknown, sourceCodeInfo?: SourceCodeInfo): asserts value is UnknownRecord {
  if (!isUnknownRecord(value)) {
    throw new LitsError(
      `Expected ${'UnknownRecord'}, got ${valueToString(value)}.`,
      getSourceCodeInfo(value, sourceCodeInfo),
    )
  }
}

export function asUnknownRecord(value: unknown, sourceCodeInfo?: SourceCodeInfo): UnknownRecord {
  assertUnknownRecord(value, sourceCodeInfo)
  return value
}
