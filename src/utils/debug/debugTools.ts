import { astNodeTypeName, isAstNodeType, isFunctionType } from '../../constants/constants'
import type { AstNode, LitsFunction } from '../../parser/types'
import { FUNCTION_SYMBOL } from '../symbols'

function isLitsFunction(func: unknown): func is LitsFunction {
  if (func === null || typeof func !== 'object')
    return false

  return FUNCTION_SYMBOL in func && 't' in func && isFunctionType(func.t)
}

function isAstNode(value: unknown): value is AstNode {
  if (value === null || typeof value !== 'object')
    return false
  return 't' in value && isAstNodeType(value.t)
}

export function valueToString(value: unknown): string {
  if (isLitsFunction(value))
    // eslint-disable-next-line ts/no-unsafe-member-access
    return `<function ${(value as any).name || '\u03BB'}>`

  if (isAstNode(value))
    return `${astNodeTypeName.get(value.t)}-node`

  if (value === null)
    return 'null'

  if (typeof value === 'object' && value instanceof RegExp)
    return `${value}`

  if (typeof value === 'object' && value instanceof Error)
    return value.toString()

  return JSON.stringify(value)
}
