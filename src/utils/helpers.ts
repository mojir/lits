import { AstNode, FUNCTION_SYMBOL, LitsFunction, NodeType } from '../parser/interface'
import { SourceCodeInfo, Token, TokenizerType } from '../tokenizer/interface'

// eslint-disable-next-line @typescript-eslint/no-explicit-any,@typescript-eslint/explicit-module-boundary-types
export function getSourceCodeInfo(anyValue: any, sourceCodeInfo: SourceCodeInfo): SourceCodeInfo {
  return anyValue?.sourceCodeInfo || sourceCodeInfo
}

export function valueToString(value: unknown): string {
  if (isLitsFunction(value)) {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    return `<function ${(value as any).name || `λ`}>`
  }
  if (isToken(value)) {
    return `${value.type}-token "${value.value}"`
  }
  if (isAstNode(value)) {
    return `${value.type}-node`
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

export function isToken(value: unknown): value is Token {
  if (typeof value !== `object` || value === null) {
    return false
  }

  const tkn = value as Token
  if (!tkn.type || typeof tkn.value !== `string`) {
    return false
  }

  if (!tkn.sourceCodeInfo && tkn.sourceCodeInfo !== null) {
    return false
  }

  return !!tokenTypes[tkn.type]
}

const astTypes: Record<NodeType, true> = {
  Number: true,
  String: true,
  NormalExpression: true,
  SpecialExpression: true,
  Name: true,
  Modifier: true,
  ReservedName: true,
  Binding: true,
  Argument: true,
  Partial: true,
}

export function isAstNode(value: unknown): value is AstNode {
  if (value === null || typeof value !== `object`) {
    return false
  }
  if (!(value as AstNode).token) {
    return false
  }
  if (!astTypes[(value as AstNode).type]) {
    return false
  }
  return true
}

export function isLitsFunction(func: unknown): func is LitsFunction {
  if (func === null || typeof func !== `object`) {
    return false
  }
  return !!(func as LitsFunction)[FUNCTION_SYMBOL]
}
