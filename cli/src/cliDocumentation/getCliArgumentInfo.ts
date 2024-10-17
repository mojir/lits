import type { FunctionReference } from '../../../reference'
import type { Colorizer } from '../colorizer'
import { formatCliDescription } from './formatCliDescription'
import { getCliType } from './getCliType'

export function getArgumentInfo(fmt: Colorizer, reference: FunctionReference) {
  const { args } = reference
  return `${Object.entries(args).map(([argName, arg]) => {
    return `${fmt.green(argName)}: ${getCliType(fmt, arg)}  ${arg.description
      ? fmt.italic.gray(formatCliDescription(fmt, arg.description, reference))
      : ''}`
  }).join('\n')}`
}
