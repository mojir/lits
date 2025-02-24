import { type FunctionReference, type Reference, isFunctionReference } from '../../../../reference'
import { createFormatter } from '../../../../common/createFormatter'
import { createVariableRule, mdRules } from '../../formatter/rules'
import { styles } from '../../styles'
import { findAllOccurrences } from '../../../../common/utils'
import { polishIdentifierCharacterClass, polishIdentifierFirstCharacterClass } from '../../../../src/identifier'

const variableRegExp = new RegExp(`\\$${polishIdentifierFirstCharacterClass}${polishIdentifierCharacterClass}*`, 'g')

export function formatDescription(description: string, reference: Reference): string {
  if (isFunctionReference(reference))
    return formatFunctionDescription(description, reference)
  else
    return formatCommonDescription(description)
}

function formatCommonDescription(description: string) {
  const formattedDescription = createFormatter(mdRules)(description)
  return `<div ${styles('text-color-gray-400')}>
    ${formattedDescription}
  </div>`
}

function formatFunctionDescription(description: string, reference: FunctionReference) {
  const descriptionVariables = findAllOccurrences(description, variableRegExp)

  const currentFunctionNameRule = createVariableRule(
    variableName => `<span ${styles('text-color-Blue')}>${variableName}</span>`,
    variableName => variableName === reference.title,
  )

  const argumentRule = createVariableRule(
    variableName => `<span ${styles('text-color-Viola')}>${variableName}</span>`,
    variableName => isArgumentName(variableName, reference),
  )

  checkVariables(reference, descriptionVariables)
  const formattedDescription = createFormatter([...mdRules, currentFunctionNameRule, argumentRule])(description)
  return `<div ${styles('text-color-gray-400')}>
    ${formattedDescription}
  </div>`
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
