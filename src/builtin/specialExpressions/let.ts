import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { BindingNode, CommonSpecialExpressionNode } from '../../parser/interface'
import { joinSets } from '../../utils'
import type { BuiltinSpecialExpression } from '../interface'

export interface LetNode extends CommonSpecialExpressionNode<'let'> {
  bs: BindingNode[]
}

export const letSpecialExpression: BuiltinSpecialExpression<Any, LetNode> = {
  paramCount: 0,
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    for (const binding of node.bs) {
      const bindingValueNode = binding.v
      const bindingValue = evaluateAstNode(bindingValueNode, contextStack)
      contextStack.addValue(binding.n, bindingValue)
    }
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    const newContext = node.bs
      .map(binding => binding.n)
      .reduce((context: Context, name) => {
        context[name] = { value: true }
        return context
      }, {})
    const bindingResults = node.bs.map((bindingNode) => {
      const valueNode = bindingNode.v
      const bindingsResult = getUndefinedSymbols([valueNode], contextStack, builtin)
      contextStack.addValue(bindingNode.n, { value: true })
      return bindingsResult
    })

    const paramsResult = getUndefinedSymbols(node.p, contextStack.create(newContext), builtin)
    return joinSets(...bindingResults, paramsResult)
  },
}
