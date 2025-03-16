import { LitsError, RecurSignal } from '../../errors'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { BindingNode, CommonSpecialExpressionNode } from '../../parser/types'
import { tokenSourceCodeInfo } from '../../tokenizer/token'
import { asNonUndefined } from '../../typeGuards'
import { asAny } from '../../typeGuards/lits'
import { joinSets } from '../../utils'
import { valueToString } from '../../utils/debug/debugTools'
import type { BuiltinSpecialExpression } from '../interface'

export interface LoopNode extends CommonSpecialExpressionNode<'loop'> {
  bindingNodes: BindingNode[]
}

export const loopSpecialExpression: BuiltinSpecialExpression<Any, LoopNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const sourceCodeInfo = tokenSourceCodeInfo(node.token)
    const bindingContext: Context = node.bindingNodes.reduce((result: Context, binding) => {
      result[binding.name] = { value: evaluateAstNode(binding.value, contextStack) }
      return result
    }, {})
    const newContextStack = contextStack.create(bindingContext)

    for (;;) {
      let result: Any = null
      try {
        for (const form of node.params)
          result = evaluateAstNode(form, newContextStack)
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
          ;node.bindingNodes.forEach((binding, index) => {
            asNonUndefined(bindingContext[binding.name], sourceCodeInfo).value = asAny(params[index], sourceCodeInfo)
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
      .map(binding => binding.name)
      .reduce((context: Context, name) => {
        context[name] = { value: true }
        return context
      }, {})

    const bindingValueNodes = node.bindingNodes.map(binding => binding.value)
    const bindingsResult = getUndefinedSymbols(bindingValueNodes, contextStack, builtin)
    const paramsResult = getUndefinedSymbols(node.params, contextStack.create(newContext), builtin)
    return joinSets(bindingsResult, paramsResult)
  },
}
