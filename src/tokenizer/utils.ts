import { LitsError } from '../errors'
import { getSourceCodeInfo } from '../utils/debug/getSourceCodeInfo'
import type { SourceCodeInfo } from './interface'
import type { Token, TokenType } from './tokens'

export type TokenDebugData = {
  sourceCodeInfo: SourceCodeInfo
}

function isTokenDebugData(tokenDebugData: unknown): tokenDebugData is TokenDebugData {
  return (
    typeof tokenDebugData === 'object'
    && tokenDebugData !== null
    && 'sourceCodeInfo' in tokenDebugData
  )
}

export function getTokenDebugData(token?: Token): TokenDebugData | undefined {
  const debugData = token?.at(-1)
  return isTokenDebugData(debugData) ? debugData : undefined
}

export function hasTokenDebugData(token?: Token): boolean {
  return isTokenDebugData(token?.at(-1))
}

export function addTokenDebugData(token: Token, debugData: TokenDebugData): void {
  if (isTokenDebugData(token.at(-1))) {
    throw new Error(`Token already has debug data: ${token}`)
  }
  ;(token as unknown[]).push(debugData)
}

export function throwUnexpectedToken(expected: TokenType, expectedValue: string | undefined, actual?: Token): never {
  if (actual === undefined) {
    throw new LitsError(`Unexpected end of input, expected ${expected}${expectedValue ? ` '${expectedValue}'` : ''}`, undefined)
  }
  const actualOutput = `${actual[0]}${actual[1] ? ` '${actual[1]}'` : ''}`
  throw new LitsError(`Unexpected token: ${actualOutput}, expected ${expected}${expectedValue ? ` '${expectedValue}'` : ''}`, getSourceCodeInfo(actual))
}
