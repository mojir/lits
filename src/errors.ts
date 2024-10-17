import type { Arr } from './interface'
import type { SourceCodeInfo } from './tokenizer/interface'
import { getCodeMarker, valueToString } from './utils/debug/debugTools'

function getLitsErrorMessage(message: string, sourceCodeInfo?: SourceCodeInfo) {
  const filePathLine = sourceCodeInfo?.filePath ? `\n${sourceCodeInfo.filePath}` : ''
  const codeLine = sourceCodeInfo?.code ? `\n${sourceCodeInfo.code}` : ''
  const codeMarker = sourceCodeInfo && codeLine ? `\n${getCodeMarker(sourceCodeInfo)}` : ''
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
  constructor(message: string | Error, sourceCodeInfo?: SourceCodeInfo) {
    if (message instanceof Error)
      message = `${message.name}${message.message}`

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

export class NotAFunctionError extends LitsError {
  constructor(fn: unknown, sourceCodeInfo?: SourceCodeInfo) {
    const message = `Expected function, got ${valueToString(fn)}.`
    super(message, sourceCodeInfo)
    Object.setPrototypeOf(this, NotAFunctionError.prototype)
    this.name = 'NotAFunctionError'
  }
}

export class UserDefinedError extends LitsError {
  constructor(message: string | Error, sourceCodeInfo?: SourceCodeInfo) {
    super(message, sourceCodeInfo)
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
