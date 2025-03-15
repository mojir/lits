import type { Colorizer } from '../colorizer'
import type { Argument, TypedValue } from '../../../reference'

export function getCliType(fmt: Colorizer, arg: Argument | TypedValue) {
  const argType = arg.type
  const types = Array.isArray(argType) ? argType : [argType]
  const typeString = types.map((type) => {
    return fmt.dim.red(type)
  }).join(' | ')
  return arg.array || arg.rest
    ? `${fmt.bright.blue('Array')}${fmt.bright.gray('<')}${fmt.white(typeString)}${fmt.bright.gray('>')}`
    : fmt.white(typeString)
}
