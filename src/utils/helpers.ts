import { FUNCTION_SYMBOL, LitsFunction } from '../parser/interface'
import { SourceCodeInfo } from '../tokenizer/interface'

export function isLitsFunction(func: unknown): func is LitsFunction {
  if (func === null || typeof func !== `object`) {
    return false
  }
  return !!(func as LitsFunction)[FUNCTION_SYMBOL]
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any,@typescript-eslint/explicit-module-boundary-types
export function getSourceCodeInfo(anyValue: any, sourceCodeInfo: SourceCodeInfo): SourceCodeInfo {
  return anyValue?.sourceCodeInfo || sourceCodeInfo
}

export function valueToString(value: unknown): string {
  if (isLitsFunction(value)) {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    return `<function ${(value as any).name || `λ`}>`
  }
  if (value === null) {
    return `null`
  }
  if (typeof value === `object` && value instanceof RegExp) {
    return `${value}`
  }
  if (typeof value === `object` && value instanceof Error) {
    return value.toString()
  }
  return JSON.stringify(value)
}
