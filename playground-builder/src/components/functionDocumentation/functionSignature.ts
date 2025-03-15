import type { FunctionReference } from '../../../../reference'
import { isSymbolicOperator } from '../../../../src/tokenizer/operators'
import type { StyleOverride } from '../../formatter/rules'
import { formatLitsExpression } from '../../formatter/rules'
import { styles } from '../../styles'
import { getType } from './getType'

function getOperatorExpression(name: string, styleOverride: StyleOverride) {
  return formatLitsExpression(`a ${name} b`, styleOverride)
}

function getOperatorSignature({ title: name, returns, _isOperator }: Pick<FunctionReference, 'title' | 'returns' | '_isOperator'>, styleOverride: StyleOverride) {
  return _isOperator
    ? `
  <tr>
    <td>${getOperatorExpression(name, styleOverride)}</td>
    <td><span ${styles('text-color-gray-400', 'mx-4', 'text-xl', 'line-height: 1rem;')}>&rarr;</span></td>
    <td><span>${getType(returns)}</span></td>
  </tr>`
    : ''
}

export function getFunctionSignature({ title: name, variants, args, returns, _isOperator }: FunctionReference) {
  const styleOverride: StyleOverride = {
    values: isSymbolicOperator(name) ? [] : [name],
    style: styles('text-color-Blue'),
  }

  return `<table>
  ${getOperatorSignature({ title: name, returns, _isOperator }, styleOverride)}
  ${variants.map((variant) => {
    const expression = `${_isOperator ? `${name}` : name}(${variant.argumentNames.map((argName) => {
      let result = ''
      const arg = args[argName]
      if (arg) {
        if (arg.rest)
          result += '...'
        result += argName
      }
      return result
    }).join(', ')})`
    //
    return `
      <tr>
        <td>${formatLitsExpression(expression, styleOverride)}</td>
        <td><span ${styles('text-color-gray-400', 'mx-4', 'text-xl', 'line-height: 1rem;')}>&rarr;</span></td>
        <td><span>${getType(returns)}</span></td>
      </tr>`
  }).join('')}
  </table>`
}
