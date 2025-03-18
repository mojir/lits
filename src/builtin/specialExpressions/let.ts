import type { Any } from '../../interface'
import type { BindingNode, GenericNode } from '../../parser/types'
import { evalueateBindingNodeValues, getAllBindingTargetNames } from '../bindingNode'
import type { BuiltinSpecialExpression } from '../interface'

export interface LetNode extends GenericNode {
  type: 'SpecialExpression'
  bindingNode: BindingNode
  name: 'let'
}

export const letSpecialExpression: BuiltinSpecialExpression<Any, LetNode> = {
  paramCount: 0,
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const bindingValue = evaluateAstNode(node.bindingNode.value, contextStack)
    const values = evalueateBindingNodeValues(node.bindingNode, bindingValue, astNode => evaluateAstNode(astNode, contextStack))
    contextStack.addValues(values)
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    const bindingResult = getUndefinedSymbols([node.bindingNode.value], contextStack, builtin)
    contextStack.addValues(getAllBindingTargetNames(node.bindingNode.target))
    return bindingResult
  },
}
