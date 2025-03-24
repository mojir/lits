import type { BindingNode, SpecialExpressionNode } from '../../parser/types'
import { addToSet } from '../../utils'
import { evalueateBindingNodeValues as evalueateBindingTargetValues, getAllBindingTargetNames, walkDefaults } from '../bindingNode'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type DefNode = SpecialExpressionNode<[typeof specialExpressionTypes['0_def'], BindingNode]> // binding, value

export const defSpecialExpression: BuiltinSpecialExpression<null, DefNode> = {
  paramCount: 2,
  evaluate: (node, contextStack, { evaluateNode }) => {
    const bindingNode: BindingNode = node[1][1]
    const target = bindingNode[1][0]
    const value = bindingNode[1][1]
    const bindingValue = evaluateNode(value, contextStack)
    const values = evalueateBindingTargetValues(target, bindingValue, Node => evaluateNode(Node, contextStack))
    contextStack.exportValues(values, target[2])
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const bindingNode: BindingNode = node[1][1]
    const target = bindingNode[1][0]
    const value = bindingNode[1][1]
    const bindingResult = getUndefinedSymbols([value], contextStack, builtin, evaluateNode)
    walkDefaults(target, (defaultNode) => {
      addToSet(bindingResult, getUndefinedSymbols([defaultNode], contextStack, builtin, evaluateNode))
    })
    contextStack.addValues(getAllBindingTargetNames(target), target[2])
    return bindingResult
  },
}
