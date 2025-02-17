import { astNodeTypeName, isAstNodeType, isFunctionType } from '../../constants/constants'
import type { AstNode, LitsFunction } from '../../parser/interface'
import { type Token, isTokenType } from '../../tokenizer/Token'
import { FUNCTION_SYMBOL } from '../symbols'

function isLitsFunction(func: unknown): func is LitsFunction {
  if (!isUnknownRecord(func))
    return false

  return !!func[FUNCTION_SYMBOL] && isFunctionType(func.t)
}

function isUnknownRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null
}

function isToken(value: unknown): value is Token {
  return Array.isArray(value) && value.length >= 1 && value.length <= 3 && typeof value[0] === 'string' && isTokenType(value[0])
}

function isAstNode(value: unknown): value is AstNode {
  return isUnknownRecord(value) && isAstNodeType(value.t)
}

export function valueToString(value: unknown): string {
  if (isLitsFunction(value))
    // eslint-disable-next-line ts/no-unsafe-member-access
    return `<function ${(value as any).name || '\u03BB'}>`

  if (isToken(value))
    return `${value[0]}-token${value[1] ? `"${value[1]}"` : ''}`

  if (isAstNode(value))
    return `${astNodeTypeName.get(value.t)}-node`

  if (value === null)
    return 'nil'

  if (typeof value === 'object' && value instanceof RegExp)
    return `${value}`

  if (typeof value === 'object' && value instanceof Error)
    return value.toString()

  return JSON.stringify(value)
}
