import { LitsError } from '../errors'
import { SourceCodeInfo } from '../tokenizer/interface'
import { getSourceCodeInfo, valueToString } from './helpers'

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
  if (
    (typeof options.gt === `number` || typeof options.gte === `number`) &&
    (typeof options.lt === `number` || typeof options.lte === `number`)
  ) {
    return `${typeof options.gt === `number` ? `${options.gt} < n ` : `${options.gte} <= n `}${
      typeof options.lt === `number` ? `< ${options.lt}` : `<= ${options.lte}`
    }`
  }
  if (typeof options.gt === `number` || typeof options.gte === `number`) {
    return `${typeof options.gt === `number` ? `n > ${options.gt}` : `n >= ${options.gte}`}`
  }
  if (typeof options.lt === `number` || typeof options.lte === `number`) {
    return `${typeof options.lt === `number` ? `n < ${options.lt}` : `n <= ${options.lte}`}`
  }
  return ``
}

function getNumberTypeName(options: NumberOptions): string {
  if (options.zero) {
    return `zero`
  }
  const sign = options.positive
    ? `positive`
    : options.negative
    ? `negative`
    : options.nonNegative
    ? `non negative`
    : options.nonPositive
    ? `non positive`
    : options.nonZero
    ? `non zero`
    : ``
  const numberType = options.integer ? `integer` : `number`
  const finite = options.finite ? `finite` : ``
  const range = getRangeString(options)

  return [sign, finite, numberType, range].filter(x => !!x).join(` `)
}

function is(value: unknown, options: NumberOptions = {}): value is number {
  if (typeof value !== `number`) {
    return false
  }
  if (options.integer && !Number.isInteger(value)) {
    return false
  }
  if (options.finite && !Number.isFinite(value)) {
    return false
  }
  if (options.zero && value !== 0) {
    return false
  }
  if (options.nonZero && value === 0) {
    return false
  }
  if (options.positive && value <= 0) {
    return false
  }
  if (options.negative && value >= 0) {
    return false
  }
  if (options.nonPositive && value > 0) {
    return false
  }
  if (options.nonNegative && value < 0) {
    return false
  }
  if (typeof options.gt === `number` && value <= options.gt) {
    return false
  }
  if (typeof options.gte === `number` && value < options.gte) {
    return false
  }
  if (typeof options.lt === `number` && value >= options.lt) {
    return false
  }
  if (typeof options.lte === `number` && value > options.lte) {
    return false
  }
  return true
}

function assert(value: unknown, sourceCodeInfo: SourceCodeInfo, options: NumberOptions = {}): asserts value is number {
  if (!is(value, options)) {
    throw new LitsError(
      `Expected ${getNumberTypeName(options)}, got ${valueToString(value)}.`,
      getSourceCodeInfo(value, sourceCodeInfo),
    )
  }
}

function as(value: unknown, sourceCodeInfo: SourceCodeInfo, options: NumberOptions = {}): number {
  assert(value, sourceCodeInfo, options)
  return value
}

export const number: {
  is: (value: unknown, options?: NumberOptions) => value is number
  as: (value: unknown, sourceCodeInfo: SourceCodeInfo, options?: NumberOptions) => number
  assert(value: unknown, sourceCodeInfo: SourceCodeInfo, options?: NumberOptions): asserts value is number
} = {
  is,
  as,
  assert,
}
