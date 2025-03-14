import type { SourceCodeInfo } from '../tokenizer/token'
import { getAssertionError } from '../utils/getAssertionError'

type StringAssertionOptions =
  | {
    nonEmpty?: true
    char?: never
  }
  | {
    nonEmpty?: never
    char?: true
  }

export function isString(value: unknown, options: StringAssertionOptions = {}): value is string {
  if (typeof value !== 'string')
    return false

  if (options.nonEmpty && value.length === 0)
    return false

  if (options.char && value.length !== 1)
    return false

  return true
}

export function assertString(
  value: unknown,
  sourceCodeInfo: SourceCodeInfo | undefined,
  options: StringAssertionOptions = {},
): asserts value is string {
  if (!isString(value, options)) {
    throw getAssertionError(
      `${options.nonEmpty ? 'non empty string' : options.char ? 'character' : 'string'}`,
      value,
      sourceCodeInfo,
    )
  }
}

export function asString(
  value: unknown,
  sourceCodeInfo: SourceCodeInfo | undefined,
  options: StringAssertionOptions = {},
): string {
  assertString(value, sourceCodeInfo, options)
  return value
}

export function isStringOrNumber(value: unknown): value is string | number {
  return typeof value === 'string' || typeof value === 'number'
}
export function asStringOrNumber(value: unknown, sourceCodeInfo?: SourceCodeInfo): string | number {
  assertStringOrNumber(value, sourceCodeInfo)
  return value
}
export function assertStringOrNumber(
  value: unknown,
  sourceCodeInfo?: SourceCodeInfo,
): asserts value is string | number {
  if (!isStringOrNumber(value))
    throw getAssertionError('string or number', value, sourceCodeInfo)
}
