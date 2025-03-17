import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { BindingNode, CommonSpecialExpressionNode } from '../../parser/types'
import { joinSets } from '../../utils'
import { bindingNodeEntries, getAllBindingTargetNames } from '../bindingNode'
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
      bindingNodeEntries(binding, bindingValue, (name, value) => {
        contextStack.addValue(name, value)
      })
    }
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    const newContext = node.bindingNodes
      .reduce((context: Context, bindingNode) => {
        getAllBindingTargetNames(bindingNode.target).forEach((name) => {
          context[name] = { value: true }
        })
        return context
      }, {})
    const bindingResults = node.bindingNodes.map((bindingNode) => {
      const valueNode = bindingNode.value
      const bindingsResult = getUndefinedSymbols([valueNode], contextStack, builtin)
      getAllBindingTargetNames(bindingNode.target).forEach((name) => {
        contextStack.addValue(name, { value: true })
      })
      return bindingsResult
    })

    const paramsResult = getUndefinedSymbols(node.params, contextStack.create(newContext), builtin)
    return joinSets(...bindingResults, paramsResult)
  },
}
