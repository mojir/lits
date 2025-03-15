import { apiReference, isFunctionReference } from '../../../reference'
import type { Colorizer } from '../colorizer'
import { isApiName } from '../../../reference/api'
import { getCliFunctionSignature } from './getCliFunctionSignature'
import { getCliTitle } from './getCliTitle'
import { formatCliDescription } from './formatCliDescription'
import { getArgumentInfo } from './getCliArgumentInfo'
import { getCliFunctionExamples } from './getCliFunctionExamples'

export function getCliDocumentation(fmt: Colorizer, name: string) {
  if (!isApiName(name))
    return `No documentation available for ${name}`

  const reference = apiReference[name]

  return `${getCliTitle(fmt, reference)}
${isFunctionReference(reference) ? getCliFunctionSignature(fmt, reference) : ''}

Description
${formatCliDescription(fmt, reference.description.trim(), reference)}

${isFunctionReference(reference)
  ? `Arguments
${getArgumentInfo(fmt, reference)}`
  : ''}

Examples
${getCliFunctionExamples(fmt, reference)}`
}
