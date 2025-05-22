import type { ParamCount } from '../builtin/interface'
import { getNodeTypeName } from '../constants/constants'
import { LitsError } from '../errors'
import type { UnknownRecord } from '../interface'
import type { NormalExpressionNodeWithName } from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { valueToString } from '../utils/debug/debugTools'
import { getSourceCodeInfo } from '../utils/debug/getSourceCodeInfo'

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

export function canBeOperator(count: ParamCount): boolean {
  if (typeof count === 'number') {
    return count === 2
  }

  if (count.odd) {
    return false
  }

  if (typeof count.max === 'number' && count.max < 2) {
    return false
  }

  if (typeof count.min === 'number' && count.min > 2) {
    return false
  }

  return true
}
