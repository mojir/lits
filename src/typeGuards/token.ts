import type { TokenType } from '../constants/constants'
import { isTokenType } from '../constants/constants'
import { LitsError } from '../errors'
import type { SourceCodeInfo, Token } from '../tokenizer/interface'
import { valueToString } from '../utils/debug/debugTools'
import { getSourceCodeInfo } from '../utils/debug/getSourceCodeInfo'

type TokenAssertionOptions =
  | {
    type: TokenType
    value?: string
  }
  | {
    type?: never
    value?: never
  }

export function isToken(value: unknown, options: TokenAssertionOptions = {}): value is Token {
  if (typeof value !== 'object' || value === null)
    return false

  const tkn = value as Token
  if (typeof tkn.v !== 'string')
    return false

  if (!isTokenType(tkn.t))
    return false

  if ((options.type != null) && tkn.t !== options.type)
    return false

  if (options.value && tkn.v !== options.value)
    return false

  return true
}

export function assertToken(
  value: unknown,
  filePath: string | undefined,
  options: TokenAssertionOptions = {},
): asserts value is Token {
  if (!isToken(value, options)) {
    const sourceCodeInfo: SourceCodeInfo | undefined = isToken(value) && value.debugData?.sourceCodeInfo
      ? value.debugData.sourceCodeInfo
      : typeof filePath === 'string'
        ? { filePath }
        : undefined

    throw new LitsError(
      `Expected ${(options.type != null) ? `${options.type}-` : ''}token${
        typeof options.value === 'string' ? ` value='${options.value}'` : ''
      }, got ${valueToString(value)}.`,
      getSourceCodeInfo(value, sourceCodeInfo),
    )
  }
}

export function asToken(value: unknown, filePath: string | undefined, options: TokenAssertionOptions = {}): Token {
  assertToken(value, filePath, options)
  return value
}
