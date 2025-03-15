import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { AstNode, CommonSpecialExpressionNode, SymbolNode } from '../../parser/types'
import { tokenSourceCodeInfo } from '../../tokenizer/token'
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
    const { p: tryExpressions, ce: catchExpression, e: errorNode } = node
    try {
      return evaluateAstNode(tryExpressions[0]!, contextStack)
    }
    catch (error) {
      const newContext: Context = errorNode
        ? {
            [errorNode.v]: { value: asAny(error, tokenSourceCodeInfo(node.token)) },
          }
        : {}
      return evaluateAstNode(catchExpression, contextStack.create(newContext))
    }
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    const { p: tryExpressions, ce: catchExpression, e: errorNode } = node
    const tryResult = getUndefinedSymbols(tryExpressions, contextStack, builtin)
    const newContext: Context = errorNode
      ? {
          [errorNode.v]: { value: true },
        }
      : {}
    const catchResult = getUndefinedSymbols([catchExpression], contextStack.create(newContext), builtin)
    return joinSets(tryResult, catchResult)
  },
}
