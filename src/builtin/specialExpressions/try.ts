import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { Node, SpecialExpressionNode, SymbolNode } from '../../parser/types'
import { joinSets } from '../../utils'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type TryNode = SpecialExpressionNode<[typeof specialExpressionTypes['try'], Node, SymbolNode | undefined, Node]>

export const trySpecialExpression: BuiltinSpecialExpression<Any, TryNode> = {
  paramCount: 1,
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
