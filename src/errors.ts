import { Arr } from './interface'
import { DebugInfo } from './tokenizer/interface'
import { getCodeMarker, valueToString } from './utils/helpers'

function getLitsErrorMessage(message: string, debugInfo?: DebugInfo) {
  return `${message}${
    debugInfo ? `\n${debugInfo === `EOF` ? `EOF` : `${debugInfo.code}\n${getCodeMarker(debugInfo)}`}` : ``
  }`
}

export class RecurSignal extends Error {
  public params: Arr
  constructor(params: Arr) {
    super(`recur, params: ${params}`)
    Object.setPrototypeOf(this, RecurSignal.prototype)
    this.name = `RecurSignal`
    this.params = params
  }
}

export abstract class AbstractLitsError extends Error {
  public debugInfo?: DebugInfo
  public shortMessage: string
  constructor(message: string | Error, debugInfo?: DebugInfo) {
    if (message instanceof Error) {
      message = `${message.name}${message.message ? `: ${message.message}` : ``}`
    }
    super(getLitsErrorMessage(message, debugInfo))
    this.shortMessage = message
    this.debugInfo = debugInfo
    Object.setPrototypeOf(this, AbstractLitsError.prototype)
    this.name = `AbstractLitsError`
  }
}

export class LitsError extends AbstractLitsError {
  constructor(message: string | Error, debugInfo?: DebugInfo) {
    super(message, debugInfo)
    Object.setPrototypeOf(this, LitsError.prototype)
    this.name = `LitsError`
  }
}

export class NotAFunctionError extends AbstractLitsError {
  constructor(fn: unknown, debugInfo?: DebugInfo) {
    const message = `Expected function, got ${valueToString(fn)}.`
    super(message, debugInfo)
    Object.setPrototypeOf(this, NotAFunctionError.prototype)
    this.name = `NotAFunctionError`
  }
}

export class UserDefinedError extends AbstractLitsError {
  constructor(message: string | Error, debugInfo?: DebugInfo) {
    super(message, debugInfo)
    Object.setPrototypeOf(this, UserDefinedError.prototype)
    this.name = `UserDefinedError`
  }
}

export class AssertionError extends AbstractLitsError {
  constructor(message: string | Error, debugInfo?: DebugInfo) {
    super(message, debugInfo)
    Object.setPrototypeOf(this, AssertionError.prototype)
    this.name = `AssertionError`
  }
}

export class UndefinedSymbolError extends AbstractLitsError {
  public symbol: string
  constructor(symbolName: string, debugInfo?: DebugInfo) {
    const message = `Undefined symbol '${symbolName}'.`
    super(message, debugInfo)
    this.symbol = symbolName
    Object.setPrototypeOf(this, UndefinedSymbolError.prototype)
    this.name = `UndefinedSymbolError`
  }
}
