import { smartTrim } from '..'
import type { Argument, FunctionReference, TypedValue } from '../../../reference'

export function generateDocString(reference: FunctionReference): string {
  return smartTrim(`
    ${reference.title}${getAliases(reference)}

    ${reference.description
        .replace(/`(.+?)`/g, '$1')
        .replace(/\$(\w+)/g, '$1')
        .replace(/\*\*\*(.+)\*\*\*/g, '$1')
        .replace(/\*\*(.+)\*\*/g, '$1')
    }

    Signature:
    ${signature(reference).join('\n    ')}

    Arguments:
      ${argStrings(reference).join('\n      ')}
  
    Examples:
${reference.examples.map(example => smartTrim(example, 4)).join('\n\n')}`)
}

function signature({ title, variants, args, returns, _isOperator }: FunctionReference): string[] {
  const functionForms = variants.map((variant) => {
    const form = `  ${title}(${variant.argumentNames.map((argName) => {
      let result = ''
      const arg = args[argName]!
      if (arg.rest) {
        result += '...'
      }
      result += argName
      return result
    }).join(', ')})`

    return `${form} -> ${type(returns)}`
  })

  const operatorForm = _isOperator ? ['', 'Operator:', `  a ${title} b -> ${type(returns)}`] : []

  return [
    ...functionForms,
    ...operatorForm,
  ]
}

function type(arg: Argument | TypedValue) {
  const argType = arg.type
  const types = Array.isArray(argType) ? argType : [argType]
  const typeString = types.join(' | ')
  return arg.array || arg.rest ? `Array<${typeString}>` : typeString
}

function argStrings(reference: FunctionReference): string[] {
  return Object.entries(reference.args).map(([argName, arg]) => `${argName}: ${type(arg)}`)
}

function getAliases(reference: FunctionReference): string {
  if (!reference.aliases || reference.aliases.length === 0)
    return ''

  return `\n\n    Alias:\n      ${reference.aliases.join('\n      ')}`
}
