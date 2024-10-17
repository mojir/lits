import { LitsError } from '../errors'
import type { UnknownRecord } from '../interface'
import type { NormalExpressionNode, SpecialExpressionNode } from '../parser/interface'
import type { SourceCodeInfo } from '../tokenizer/interface'
import { valueToString } from '../utils/debug/debugTools'
import { getSourceCodeInfo } from '../utils/debug/getSourceCodeInfo'

export function assertEventNumberOfParams(node: NormalExpressionNode): void {
  const length = node.p.length
  if (length % 2 !== 0) {
    throw new LitsError(
      `Wrong number of arguments, expected an even number, got ${valueToString(length)}.`,
      node.tkn?.sourceCodeInfo,
    )
  }
}

export function isNonUndefined<T>(value: T | undefined): value is T {
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

/* v8 ignore next 3 */
export function assertUnreachable(_: never): never {
  throw new Error('This should not be reached')
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

export function assertNumberOfParams(
  count: number | { min?: number, max?: number },
  node: NormalExpressionNode | SpecialExpressionNode,
): void {
  const length = node.p.length
  const sourceCodeInfo = node.tkn?.sourceCodeInfo
  if (typeof count === 'number') {
    if (length !== count) {
      throw new LitsError(
        `Wrong number of arguments to "${node.n}", expected ${count}, got ${valueToString(length)}.`,
        node.tkn?.sourceCodeInfo,
      )
    }
  }
  else {
    const { min, max } = count
    if (min === undefined && max === undefined)
      throw new LitsError('Min or max must be specified.', sourceCodeInfo)

    if (typeof min === 'number' && length < min) {
      throw new LitsError(
        `Wrong number of arguments to "${node.n}", expected at least ${min}, got ${valueToString(length)}.`,
        sourceCodeInfo,
      )
    }

    if (typeof max === 'number' && length > max) {
      throw new LitsError(
        `Wrong number of arguments to "${node.n}", expected at most ${max}, got ${valueToString(length)}.`,
        sourceCodeInfo,
      )
    }
  }
}
