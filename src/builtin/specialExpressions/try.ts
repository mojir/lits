import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { AstNode, CommonSpecialExpressionNode, SymbolNode } from '../../parser/types'
import { asAny } from '../../typeGuards/lits'
import { joinSets } from '../../utils'
import type { BuiltinSpecialExpression } from '../interface'

export interface TryNode extends CommonSpecialExpressionNode<'try'> {
  e: SymbolNode | undefined
  ce: AstNode
}

export const trySpecialExpression: BuiltinSpecialExpression<Any, TryNode> = {
  paramCount: 1,
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const { params: tryExpressions, ce: catchExpression, e: errorNode } = node
    try {
      return evaluateAstNode(tryExpressions[0]!, contextStack)
    }
    catch (error) {
      const newContext: Context = errorNode
        ? {
            [errorNode.value]: { value: asAny(error, node.sourceCodeInfo) },
          }
        : {}
      return evaluateAstNode(catchExpression, contextStack.create(newContext))
    }
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateAstNode }) => {
    const { params: tryExpressions, ce: catchExpression, e: errorNode } = node
    const tryResult = getUndefinedSymbols(tryExpressions, contextStack, builtin, evaluateAstNode)
    const newContext: Context = errorNode
      ? {
          [errorNode.value]: { value: true },
        }
      : {}
    const catchResult = getUndefinedSymbols([catchExpression], contextStack.create(newContext), builtin, evaluateAstNode)
    return joinSets(tryResult, catchResult)
  },
}
