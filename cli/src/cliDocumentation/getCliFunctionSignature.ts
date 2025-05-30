import type { FunctionReference } from '../../../reference'
import type { Colorizer } from '../colorizer'
import { getCliType } from './getCliType'

export function getCliFunctionSignature(fmt: Colorizer, { title: name, variants, args, returns }: FunctionReference) {
  return variants.map((variant) => {
    const form = (variant.argumentNames.length === 0)
      ? `${fmt.white('(')}${fmt.blue(name)}${fmt.white(')')}`
      : `${fmt.white('(')}${fmt.blue(name)} ${variant.argumentNames.map((argName) => {
        let result = ''
        const arg = args[argName]
        if (arg) {
          if (arg.rest)
            result += fmt.white('& ')
          result += `${fmt.green(argName)}`
        }
        return result
      }).join(' ')}${fmt.white(')')}`

    return `${form} ${fmt.gray('->')} ${getCliType(fmt, returns)}`
  }).join('\n')
}
