import type { Any } from '../../interface'
import type { BindingNode, SpecialExpressionNode } from '../../parser/types'
import { evalueateBindingNodeValues, getAllBindingTargetNames } from '../bindingNode'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type LetNode = SpecialExpressionNode<[typeof specialExpressionTypes['let'], BindingNode]>

export const letSpecialExpression: BuiltinSpecialExpression<Any, LetNode> = {
  paramCount: 0,
  evaluate: (node, contextStack, { evaluateNode }) => {
    const bindingNode = node[1][1]
    const target = bindingNode[1][0]
    const value = bindingNode[1][1]
    const bindingValue = evaluateNode(value, contextStack)
    const values = evalueateBindingNodeValues(target, bindingValue, Node => evaluateNode(Node, contextStack))
    contextStack.addValues(values)
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const bindingNode = node[1][1]
    const target = bindingNode[1][0]
    const value = bindingNode[1][1]
    const bindingResult = getUndefinedSymbols([value], contextStack, builtin, evaluateNode)
    contextStack.addValues(getAllBindingTargetNames(target))
    return bindingResult
  },
}
