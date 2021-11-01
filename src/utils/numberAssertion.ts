import { LitsError } from '../errors'
import { TokenMeta } from '../tokenizer/interface'

type NumberOptions = {
  integer?: true
}
function getNumberTypeName(options: NumberOptions): string {
  if (options.integer) {
    return `integer`
  }
  return `number`
}

function is(value: unknown, options: NumberOptions = {}): value is number {
  if (typeof value !== `number`) {
    return false
  }
  if (options.integer && !Number.isInteger(value)) {
    return false
  }
  return true
}

function assert(value: unknown, meta: TokenMeta, options: NumberOptions = {}): asserts value is number {
  if (!is(value, options)) {
    throw new LitsError(`Expected ${getNumberTypeName(options)}, got ${value}`, meta)
  }
}

function as(value: unknown, meta: TokenMeta, options: NumberOptions = {}): number {
  assert(value, meta, options)
  return value
}

export const number: {
  is: (value: unknown, options?: NumberOptions) => value is number
  as: (value: unknown, meta: TokenMeta, options?: NumberOptions) => number
  assert(value: unknown, meta: TokenMeta, options?: NumberOptions): asserts value is number
} = {
  is,
  as,
  assert,
}
