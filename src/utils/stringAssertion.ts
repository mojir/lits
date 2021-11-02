import { LitsError } from '../errors'
import { SourceCodeInfo } from '../tokenizer/interface'

type Options =
  | {
      nonEmpty?: true
      char?: never
    }
  | {
      nonEmpty?: never
      char?: true
    }

function is(value: unknown, options: Options = {}): value is string {
  if (typeof value !== `string`) {
    return false
  }

  if (options.nonEmpty && value.length === 0) {
    return false
  }

  if (options.char && value.length !== 1) {
    return false
  }

  return true
}

function assert(value: unknown, sourceCodeInfo: SourceCodeInfo, options: Options = {}): asserts value is string {
  if (!is(value, options)) {
    throw new LitsError(
      `Expected ${options.nonEmpty ? `non empty string` : options.nonEmpty ? `character` : `string`}, got ${value}`,
      sourceCodeInfo,
    )
  }
}

function as(value: unknown, sourceCodeInfo: SourceCodeInfo, options: Options = {}): string {
  assert(value, sourceCodeInfo, options)
  return value
}

export const string: {
  is: (value: unknown, options?: Options) => value is string
  as: (value: unknown, sourceCodeInfo: SourceCodeInfo, options?: Options) => string
  assert(value: unknown, sourceCodeInfo: SourceCodeInfo, options?: Options): asserts value is string
} = {
  is,
  as,
  assert,
}
