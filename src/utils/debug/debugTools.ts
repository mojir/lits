import { getNodeTypeName, isFunctionType, isNodeType } from '../../constants/constants'
import type { LitsFunction, Node } from '../../parser/types'
import { FUNCTION_SYMBOL } from '../symbols'

function isLitsFunction(func: unknown): func is LitsFunction {
  if (func === null || typeof func !== 'object')
    return false

  return FUNCTION_SYMBOL in func && 'functionType' in func && isFunctionType(func.functionType)
}

function isNode(value: unknown): value is Node {
  if (!Array.isArray(value) || value.length < 2)
    return false
  return isNodeType(value[0])
}

export function valueToString(value: unknown): string {
  if (isLitsFunction(value))
    // eslint-disable-next-line ts/no-unsafe-member-access
    return `<function ${(value as any).name || '\u03BB'}>`

  if (isNode(value))
    return `${getNodeTypeName(value[0])}-node`

  if (value === null)
    return 'null'

  if (typeof value === 'object' && value instanceof RegExp)
    return `${value}`

  if (typeof value === 'object' && value instanceof Error)
    return value.toString()

  return JSON.stringify(value)
}
