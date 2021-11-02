import { LitsError } from '../errors'
import { SourceCodeInfo, Token, TokenizerType } from '../tokenizer/interface'

type Options =
  | {
      type: TokenizerType
      value?: string
    }
  | {
      type?: never
      value?: never
    }

const tokenTypes: Record<TokenizerType, true> = {
  fnShorthand: true,
  modifier: true,
  name: true,
  number: true,
  paren: true,
  regexpShorthand: true,
  reservedName: true,
  string: true,
}

function isToken(value: unknown): value is Token {
  if (typeof value !== `object` || value === null) {
    return false
  }

  const tkn = value as Token
  if (!tkn.sourceCodeInfo || !tkn.type || typeof tkn.value !== `string`) {
    return false
  }

  return !!tokenTypes[tkn.type]
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
      }, got ${value}`,
      sourceCodeInfo,
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
