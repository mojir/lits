import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { BindingNode, CommonSpecialExpressionNode } from '../../parser/types'
import { joinSets } from '../../utils'
import type { BuiltinSpecialExpression } from '../interface'

export interface LetNode extends CommonSpecialExpressionNode<'let'> {
  bindingNodes: BindingNode[]
}

export const letSpecialExpression: BuiltinSpecialExpression<Any, LetNode> = {
  paramCount: 0,
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    for (const binding of node.bindingNodes) {
      const bindingValueNode = binding.value
      const bindingValue = evaluateAstNode(bindingValueNode, contextStack)
      contextStack.addValue(binding.name, bindingValue)
    }
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    const newContext = node.bindingNodes
      .map(binding => binding.name)
      .reduce((context: Context, name) => {
        context[name] = { value: true }
        return context
      }, {})
    const bindingResults = node.bindingNodes.map((bindingNode) => {
      const valueNode = bindingNode.value
      const bindingsResult = getUndefinedSymbols([valueNode], contextStack, builtin)
      contextStack.addValue(bindingNode.name, { value: true })
      return bindingsResult
    })

    const paramsResult = getUndefinedSymbols(node.params, contextStack.create(newContext), builtin)
    return joinSets(...bindingResults, paramsResult)
  },
}
