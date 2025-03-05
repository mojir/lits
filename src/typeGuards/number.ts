import { LitsError } from '../errors'
import type { SourceCodeInfo } from '../tokenizer/interface'
import { valueToString } from '../utils/debug/debugTools'
import { getSourceCodeInfo } from '../utils/debug/getSourceCodeInfo'

type SignOptions =
  | {
    positive?: true
    negative?: never
    nonPositive?: never
    nonNegative?: never
    zero?: never
    nonZero?: never
  }
  | {
    positive?: never
    negative?: true
    nonPositive?: never
    nonNegative?: never
    zero?: never
    nonZero?: never
  }
  | {
    positive?: never
    negative?: never
    nonPositive?: true
    nonNegative?: never
    zero?: never
    nonZero?: never
  }
  | {
    positive?: never
    negative?: never
    nonPositive?: never
    nonNegative?: true
    zero?: never
    nonZero?: never
  }
  | {
    positive?: never
    negative?: never
    nonPositive?: never
    nonNegative?: never
    zero?: true
    nonZero?: never
  }
  | {
    positive?: never
    negative?: never
    nonPositive?: never
    nonNegative?: never
    zero?: never
    nonZero?: true
  }

type GtOptions =
  | {
    gt?: number
    gte?: never
  }
  | {
    gt?: never
    gte?: number
  }

type LtOptions =
  | {
    lt?: number
    lte?: never
  }
  | {
    lt?: never
    lte?: number
  }

type NumberOptions = {
  integer?: true
  finite?: true
} & SignOptions &
GtOptions &
LtOptions

function getRangeString(options: NumberOptions): string {
  const hasUpperAndLowerBound
    = (typeof options.gt === 'number' || typeof options.gte === 'number')
      && (typeof options.lt === 'number' || typeof options.lte === 'number')
  if (hasUpperAndLowerBound) {
    return `${typeof options.gt === 'number' ? `${options.gt} < n ` : `${options.gte} <= n `}${
      typeof options.lt === 'number' ? `< ${options.lt}` : `<= ${options.lte}`
    }`
  }
  else if (typeof options.gt === 'number' || typeof options.gte === 'number') {
    return `${typeof options.gt === 'number' ? `n > ${options.gt}` : `n >= ${options.gte}`}`
  }
  else if (typeof options.lt === 'number' || typeof options.lte === 'number') {
    return `${typeof options.lt === 'number' ? `n < ${options.lt}` : `n <= ${options.lte}`}`
  }
  else { return '' }
}

function getSignString(options: NumberOptions): string {
  return options.positive
    ? 'positive'
    : options.negative
      ? 'negative'
      : options.nonNegative
        ? 'non negative'
        : options.nonPositive
          ? 'non positive'
          : options.nonZero
            ? 'non zero'
            : ''
}

function getNumberTypeName(options: NumberOptions): string {
  if (options.zero)
    return 'zero'

  const sign = getSignString(options)
  const numberType = options.integer ? 'integer' : 'number'
  const finite = options.finite ? 'finite' : ''
  const range = getRangeString(options)

  return [sign, finite, numberType, range].filter(x => !!x).join(' ')
}

export function isNumber(value: unknown, options: NumberOptions = {}): value is number {
  if (typeof value !== 'number')
    return false

  if (options.integer && !Number.isInteger(value))
    return false

  if (options.finite && !Number.isFinite(value))
    return false

  if (options.zero && value !== 0)
    return false

  if (options.nonZero && value === 0)
    return false

  if (options.positive && value <= 0)
    return false

  if (options.negative && value >= 0)
    return false

  if (options.nonPositive && value > 0)
    return false

  if (options.nonNegative && value < 0)
    return false

  if (typeof options.gt === 'number' && value <= options.gt)
    return false

  if (typeof options.gte === 'number' && value < options.gte)
    return false

  if (typeof options.lt === 'number' && value >= options.lt)
    return false

  if (typeof options.lte === 'number' && value > options.lte)
    return false

  return true
}

export function assertNumber(
  value: unknown,
  sourceCodeInfo?: SourceCodeInfo,
  options: NumberOptions = {},
): asserts value is number {
  if (!isNumber(value, options)) {
    throw new LitsError(
      `Expected ${getNumberTypeName(options)}, got ${valueToString(value)}.`,
      getSourceCodeInfo(value, sourceCodeInfo),
    )
  }
}

export function asNumber(
  value: unknown,
  sourceCodeInfo: SourceCodeInfo | undefined,
  options: NumberOptions = {},
): number {
  assertNumber(value, sourceCodeInfo, options)
  return value
}
