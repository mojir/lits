import { LitsError } from '../../../errors'
import type { SourceCodeInfo } from '../../../tokenizer/token'
import { approxZero } from '../../../utils'

export function getUnit<T extends number[]>(
  value: T,
  sourceCodeInfo: SourceCodeInfo | undefined,
): T {
  if (value.length === 0) {
    return value
  }
  const length = Math.sqrt(value.reduce((acc, item) => acc + item ** 2, 0))
  if (approxZero(length)) {
    throw new LitsError('The vector must not be zero', sourceCodeInfo)
  }
  return value.map(item => item / length) as T
}
