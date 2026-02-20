import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { Node, SpecialExpressionNode, SymbolNode } from '../../parser/types'
import { joinSets } from '../../utils'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type TryNode = SpecialExpressionNode<[typeof specialExpressionTypes['try'], Node, SymbolNode | undefined, Node]>

const docs: CustomDocs = {
  category: 'Special-Expression',
  customVariants: ['try { try-body } catch { catch-body }', 'try { try-body } catch(error) { catch-body }'],
  details: [
    ['try-body', 'expressions', 'The expressions to try.'],
    ['error', 'symbol', 'The error variable to bind.'],
    ['catch-body', 'expression', 'The expressions to evaluate if the try-body throws an error.'],
  ],
  description: 'Executes `try-body`. If that throws, the `catch-body` gets executed. See examples for details.',
  examples: [
    `
try
  2 / 4
catch
  "Oops!"
end`,
    `
try
  foo()
catch(error)
  "Error: " ++ error.message
end`,
    `
try
  foo()
catch
  42
end`,
  ],
}

export const trySpecialExpression: BuiltinSpecialExpression<Any, TryNode> = {
  arity: {},
  docs,
  evaluate: (node, contextStack, { evaluateNode }) => {
    const [, tryExpression, errorSymbol, catchExpression] = node[1]
    try {
      return evaluateNode(tryExpression, contextStack)
    }
    catch (error) {
      const newContext: Context = errorSymbol
        ? {
            [errorSymbol[1]]: { value: error as Any },
          }
        : {}
      return evaluateNode(catchExpression, contextStack.create(newContext))
    }
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const [, tryExpression, errorSymbol, catchExpression] = node[1]
    const tryResult = getUndefinedSymbols([tryExpression], contextStack, builtin, evaluateNode)
    const newContext: Context = errorSymbol
      ? {
          [errorSymbol[1]]: { value: true },
        }
      : {}
    const catchResult = getUndefinedSymbols([catchExpression], contextStack.create(newContext), builtin, evaluateNode)
    return joinSets(tryResult, catchResult)
  },
}
