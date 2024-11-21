import { type FunctionReference, type Reference, isFunctionReference } from '../../../reference'
import { nameCharacterClass } from '../../../src/tokenizer/tokenizers'
import { findAllOccurrences } from '../../../common/utils'
import { createFormatter } from '../../../common/createFormatter'
import { createVariableRule, getMdRules } from '../cliFormatterRules'
import type { Colorizer } from '../colorizer'

const variableRegExp = new RegExp(`\\$${nameCharacterClass}+`, 'g')

export function formatCliDescription(fmt: Colorizer, description: string, reference: Reference): string {
  if (isFunctionReference(reference))
    return formatFunctionDescription(fmt, description, reference)
  else
    return formatCommonDescription(fmt, description)
}

function formatCommonDescription(fmt: Colorizer, description: string) {
  const formattedDescription = createFormatter(getMdRules(fmt))(description)
  return fmt.gray(formattedDescription)
}

function formatFunctionDescription(fmt: Colorizer, description: string, reference: FunctionReference) {
  const descriptionVariables = findAllOccurrences(description, variableRegExp)

  const currentFunctionNameRule = createVariableRule(
    variableName => fmt.blue(variableName),
    variableName => variableName === reference.title,
  )

  const argumentRule = createVariableRule(
    variableName => fmt.green(variableName),
    variableName => isArgumentName(variableName, reference),
  )

  checkVariables(reference, descriptionVariables)
  return createFormatter([...getMdRules(fmt), currentFunctionNameRule, argumentRule])(description)
}

function checkVariables(reference: FunctionReference, variables: Set<string>) {
  variables.forEach((variable) => {
    const variableName = variable.slice(1)
    if (variableName === reference.title)
      return

    if (!isArgumentName(variableName, reference)) {
      console.error(`Unknown argument ${variable}`, reference)
      throw new Error(`Unknown argument ${variable}`)
    }
  })
}

function isArgumentName(variableName: string, reference: FunctionReference) {
  return Object.keys(reference.args).includes(variableName)
}
