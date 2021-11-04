import { LitsError } from '../errors'
import { SourceCodeInfo, Token, TokenizerType } from '../tokenizer/interface'
import { getSourceCodeInfo, isToken, valueToString } from './helpers'

type Options =
  | {
      type: TokenizerType
      value?: string
    }
  | {
      type?: never
      value?: never
    }

function is(value: unknown, options: Options = {}): value is Token {
  if (!isToken(value)) {
    return false
  }

  if (options.type && value.type !== options.type) {
    return false
  }

  if (options.value && value.value !== options.value) {
    return false
  }

  return true
}

function assert(value: unknown, sourceCodeInfo: SourceCodeInfo, options: Options = {}): asserts value is Token {
  if (!is(value, options)) {
    if (isToken(value)) {
      sourceCodeInfo = value.sourceCodeInfo
    }

    throw new LitsError(
      `Expected ${options.type ? `${options.type}-` : ``}token${
        typeof options.value === `string` ? ` value='${options.value}'` : ``
      }, got ${valueToString(value)}.`,
      getSourceCodeInfo(value, sourceCodeInfo),
    )
  }
}

function as(value: unknown, sourceCodeInfo: SourceCodeInfo, options: Options = {}): Token {
  assert(value, sourceCodeInfo, options)
  return value
}

export const token: {
  is: (value: unknown, options?: Options) => value is Token
  as: (value: unknown, sourceCodeInfo: SourceCodeInfo, options?: Options) => Token
  assert(value: unknown, sourceCodeInfo: SourceCodeInfo, options?: Options): asserts value is Token
} = {
  is,
  as,
  assert,
}
