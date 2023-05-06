import { LitsError } from '../errors'
import { DebugInfo, Token, TokenizerType } from '../tokenizer/interface'
import { getDebugInfo, isToken, valueToString } from './helpers'

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

function assert(value: unknown, debugInfo: DebugInfo | undefined, options: Options = {}): asserts value is Token {
  if (!is(value, options)) {
    if (isToken(value)) {
      debugInfo = value.debugInfo
    }

    throw new LitsError(
      `Expected ${options.type ? `${options.type}-` : ``}token${
        typeof options.value === `string` ? ` value='${options.value}'` : ``
      }, got ${valueToString(value)}.`,
      getDebugInfo(value, debugInfo),
    )
  }
}

function as(value: unknown, debugInfo: DebugInfo, options: Options = {}): Token {
  assert(value, debugInfo, options)
  return value
}

export const token: {
  is: (value: unknown, options?: Options) => value is Token
  as: (value: unknown, debugInfo: DebugInfo, options?: Options) => Token
  assert(value: unknown, debugInfo: DebugInfo, options?: Options): asserts value is Token
} = {
  is,
  as,
  assert,
}
