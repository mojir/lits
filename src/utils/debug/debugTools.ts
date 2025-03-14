import { astNodeTypeName, isAstNodeType, isFunctionType } from '../../constants/constants'
import type { AstNode, LitsFunction } from '../../parser/interface'
import { isUnknownRecord } from '../../typeGuards'
import { FUNCTION_SYMBOL } from '../symbols'

function isLitsFunction(func: unknown): func is LitsFunction {
  if (!isUnknownRecord(func))
    return false

  return !!func[FUNCTION_SYMBOL] && isFunctionType(func.t)
}

function isAstNode(value: unknown): value is AstNode {
  return isUnknownRecord(value) && isAstNodeType(value.t)
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
