import { LitsError, RecurSignal } from '../../errors'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { BindingNode, CommonSpecialExpressionNode } from '../../parser/types'
import { asNonUndefined } from '../../typeGuards'
import { asAny } from '../../typeGuards/lits'
import { joinSets } from '../../utils'
import { valueToString } from '../../utils/debug/debugTools'
import { evalueateBindingNodeValues, getAllBindingTargetNames } from '../bindingNode'
import type { BuiltinSpecialExpression } from '../interface'

export interface LoopNode extends CommonSpecialExpressionNode<'loop'> {
  bindingNodes: BindingNode[]
}

export const loopSpecialExpression: BuiltinSpecialExpression<Any, LoopNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const sourceCodeInfo = node.sourceCodeInfo
    const bindingContext: Context = node.bindingNodes.reduce((result: Context, binding) => {
      const val = evaluateAstNode(binding.value, contextStack.create(result))
      const valueRecord = evalueateBindingNodeValues(binding, val, astNode => evaluateAstNode(astNode, contextStack))
      Object.entries(valueRecord).forEach(([name, value]) => {
        result[name] = { value }
      })
      return result
    }, {})
    const newContextStack = contextStack.create(bindingContext)

    for (;;) {
      let result: Any = null
      try {
        for (const form of node.params) {
          result = evaluateAstNode(form, newContextStack)
        }
      }
      catch (error) {
        if (error instanceof RecurSignal) {
          const params = error.params
          if (params.length !== node.bindingNodes.length) {
            throw new LitsError(
              `recur expected ${node.bindingNodes.length} parameters, got ${valueToString(params.length)}`,
              sourceCodeInfo,
            )
          }
          node.bindingNodes.forEach((binding, index) => {
            const valueRecord = evalueateBindingNodeValues(binding, asAny(params[index], sourceCodeInfo), astNode => evaluateAstNode(astNode, contextStack))
            for (const [name, value] of Object.entries(valueRecord)) {
              asNonUndefined(bindingContext[name], sourceCodeInfo).value = value
            }
          })
          continue
        }
        throw error
      }
      return result
    }
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    const newContext = node.bindingNodes
      .reduce((context: Context, bindingNode) => {
        const names = getAllBindingTargetNames(bindingNode.target)

        Object.keys(names).forEach((name) => {
          context[name] = { value: true }
        })
        return context
      }, {})

    const bindingValueNodes = node.bindingNodes.map(binding => binding.value)
    const bindingsResult = getUndefinedSymbols(bindingValueNodes, contextStack, builtin)
    const paramsResult = getUndefinedSymbols(node.params, contextStack.create(newContext), builtin)
    return joinSets(bindingsResult, paramsResult)
  },
}
