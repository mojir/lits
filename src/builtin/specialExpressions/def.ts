import type { Any } from '../../interface'
import type { BindingNode, SpecialExpressionNode } from '../../parser/types'
import { addToSet } from '../../utils'
import { chain } from '../../utils/maybePromise'
import { evaluateBindingNodeValues as evaluateBindingTargetValues, getAllBindingTargetNames, walkDefaults } from '../bindingNode'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type DefNode = SpecialExpressionNode<[typeof specialExpressionTypes['0_def'], BindingNode]> // binding, value

export const defSpecialExpression: BuiltinSpecialExpression<Any, DefNode> = {
  arity: {},
  evaluate: (node, contextStack, { evaluateNode }) => {
    const bindingNode: BindingNode = node[1][1]
    const target = bindingNode[1][0]
    const value = bindingNode[1][1]
    return chain(evaluateNode(value, contextStack), (bindingValue) => {
      return chain(evaluateBindingTargetValues(target, bindingValue, Node => evaluateNode(Node, contextStack)), (values) => {
        contextStack.exportValues(values, target[2])
        return bindingValue
      })
    })
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
