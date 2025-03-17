import type { CommonSpecialExpressionNode, SymbolNode } from '../../parser/types'
import { asAstNode, asSymbolNode } from '../../typeGuards/astNode'
import type { BuiltinSpecialExpression } from '../interface'
import { assertNameNotDefined } from '../utils'

export interface DefNode extends CommonSpecialExpressionNode<'def'> {}

export const defSpecialExpression: BuiltinSpecialExpression<null, DefNode> = {
  paramCount: 2,
  evaluate: (node, contextStack, { evaluateAstNode, builtin }) => {
    const name = (node.params[0] as SymbolNode).value

    assertNameNotDefined(name, contextStack, builtin, node.sourceCodeInfo)

    contextStack.exportValue(name, evaluateAstNode(node.params[1]!, contextStack))

    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    const subNode = asAstNode(node.params[1])
    const result = getUndefinedSymbols([subNode], contextStack, builtin)
    const name = asSymbolNode(node.params[0]).value
    assertNameNotDefined(name, contextStack, builtin, node.sourceCodeInfo)
    contextStack.exportValue(name, true)
    return result
  },
}
