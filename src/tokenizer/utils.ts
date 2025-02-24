import { LitsError } from '../errors'
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

export function throwUnexpectedToken(expected: TokenType, actual?: Token): never {
  throw new LitsError(`Unexpected token: ${actual}, expected ${expected}`, undefined)
}
