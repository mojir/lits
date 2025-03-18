import type { BindingNode, GenericNode } from '../../parser/types'
import { evalueateBindingNodeValues, getAllBindingTargetNames } from '../bindingNode'
import type { BuiltinSpecialExpression } from '../interface'

export interface DefNode extends GenericNode {
  name: 'def'
  type: 'SpecialExpression'
  bindingNode: BindingNode
}

export const defSpecialExpression: BuiltinSpecialExpression<null, DefNode> = {
  paramCount: 2,
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const bindingValue = evaluateAstNode(node.bindingNode.value, contextStack)
    const values = evalueateBindingNodeValues(node.bindingNode, bindingValue, astNode => evaluateAstNode(astNode, contextStack))
    contextStack.exportValues(values)
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    const bindingResult = getUndefinedSymbols([node.bindingNode.value], contextStack, builtin)
    contextStack.addValues(getAllBindingTargetNames(node.bindingNode.target))
    return bindingResult
  },
}
