import type { SourceCodeInfo } from '../tokenizer/interface'
import { getAssertionError } from '../utils/getAssertionError'

// isArray not needed, use Array.isArary
export function asArray(value: unknown, sourceCodeInfo?: SourceCodeInfo): unknown[] {
  assertArray(value, sourceCodeInfo)
  return value
}
export function assertArray(value: unknown, sourceCodeInfo?: SourceCodeInfo): asserts value is unknown[] {
  if (!Array.isArray(value))
    throw getAssertionError('array', value, sourceCodeInfo)
}

export function isStringArray(value: unknown): value is string[] {
  return Array.isArray(value) && value.every(v => typeof v === 'string')
}
export function asStringArray(value: unknown, sourceCodeInfo?: SourceCodeInfo): string[] {
  assertStringArray(value, sourceCodeInfo)
  return value
}
export function assertStringArray(value: unknown, sourceCodeInfo?: SourceCodeInfo): asserts value is string[] {
  if (!isStringArray(value))
    throw getAssertionError('array of strings', value, sourceCodeInfo)
}

export function isCharArray(value: unknown): value is string[] {
  return Array.isArray(value) && value.every(v => typeof v === 'string' && v.length === 1)
}
export function asCharArray(value: unknown, sourceCodeInfo?: SourceCodeInfo): string[] {
  assertCharArray(value, sourceCodeInfo)
  return value
}
export function assertCharArray(value: unknown, sourceCodeInfo?: SourceCodeInfo): asserts value is string[] {
  if (!isCharArray(value))
    throw getAssertionError('array of strings', value, sourceCodeInfo)
}
