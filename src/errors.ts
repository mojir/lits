import { getCodeMarker } from '../src/utils/debug/getCodeMarker'
import type { Arr } from './interface'
import type { SourceCodeInfo } from './tokenizer/token'

function getLitsErrorMessage(message: string, sourceCodeInfo?: SourceCodeInfo) {
  if (!sourceCodeInfo) {
    return message
  }
  const location = `${sourceCodeInfo.position.line}:${sourceCodeInfo.position.column}`
  const filePathLine = sourceCodeInfo.filePath
    ? `\n${sourceCodeInfo.filePath}:${location}`
    : `\nLocation ${location}`
  const codeLine = `\n${sourceCodeInfo.code}`
  const codeMarker = `\n${getCodeMarker(sourceCodeInfo)}`
  return `${message}${filePathLine}${codeLine}${codeMarker}`
}

export class RecurSignal extends Error {
  public params: Arr
  constructor(params: Arr) {
    super(`recur, params: ${params}`)
    Object.setPrototypeOf(this, RecurSignal.prototype)
    this.name = 'RecurSignal'
    this.params = params
  }
}

export class LitsError extends Error {
  public readonly sourceCodeInfo?: SourceCodeInfo
  public readonly shortMessage: string
  constructor(err: unknown, sourceCodeInfo: SourceCodeInfo | undefined) {
    const message = err instanceof Error
      ? err.message
      : `${err}`

    super(getLitsErrorMessage(message, sourceCodeInfo))
    this.shortMessage = message
    this.sourceCodeInfo = sourceCodeInfo
    Object.setPrototypeOf(this, LitsError.prototype)
    this.name = 'LitsError'
  }

  public getCodeMarker(): string | undefined {
    return this.sourceCodeInfo && getCodeMarker(this.sourceCodeInfo)
  }
}

export class UserDefinedError extends LitsError {
  public userMessage: string
  constructor(message: string, sourceCodeInfo?: SourceCodeInfo) {
    super(message, sourceCodeInfo)
    this.userMessage = message
    Object.setPrototypeOf(this, UserDefinedError.prototype)
    this.name = 'UserDefinedError'
  }
}

export class AssertionError extends LitsError {
  constructor(message: string | Error, sourceCodeInfo?: SourceCodeInfo) {
    super(message, sourceCodeInfo)
    Object.setPrototypeOf(this, AssertionError.prototype)
    this.name = 'AssertionError'
  }
}

export class UndefinedSymbolError extends LitsError {
  public symbol: string
  constructor(symbolName: string, sourceCodeInfo?: SourceCodeInfo) {
    const message = `Undefined symbol '${symbolName}'.`
    super(message, sourceCodeInfo)
    this.symbol = symbolName
    Object.setPrototypeOf(this, UndefinedSymbolError.prototype)
    this.name = 'UndefinedSymbolError'
  }
}

export function isLitsError(error: unknown): error is LitsError {
  return error instanceof LitsError
}
