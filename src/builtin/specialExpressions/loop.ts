import { LitsError, RecurSignal } from '../../errors'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { BindingNode, CommonSpecialExpressionNode } from '../../parser/types'
import { getTokenDebugData } from '../../tokenizer/token'
import { asNonUndefined } from '../../typeGuards'
import { asAny } from '../../typeGuards/lits'
import { joinSets } from '../../utils'
import { valueToString } from '../../utils/debug/debugTools'
import type { BuiltinSpecialExpression } from '../interface'

export interface LoopNode extends CommonSpecialExpressionNode<'loop'> {
  bs: BindingNode[]
}

export const loopSpecialExpression: BuiltinSpecialExpression<Any, LoopNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const sourceCodeInfo = getTokenDebugData(node.token)?.sourceCodeInfo
    const bindingContext: Context = node.bs.reduce((result: Context, binding) => {
      result[binding.n] = { value: evaluateAstNode(binding.v, contextStack) }
      return result
    }, {})
    const newContextStack = contextStack.create(bindingContext)

    for (;;) {
      let result: Any = null
      try {
        for (const form of node.p)
          result = evaluateAstNode(form, newContextStack)
      }
      catch (error) {
        if (error instanceof RecurSignal) {
          const params = error.params
          if (params.length !== node.bs.length) {
            throw new LitsError(
              `recur expected ${node.bs.length} parameters, got ${valueToString(params.length)}`,
              sourceCodeInfo,
            )
          }
          ;node.bs.forEach((binding, index) => {
            asNonUndefined(bindingContext[binding.n], sourceCodeInfo).value = asAny(params[index], sourceCodeInfo)
          })
          continue
        }
        throw error
      }
      return result
    }
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    const newContext = node.bs
      .map(binding => binding.n)
      .reduce((context: Context, name) => {
        context[name] = { value: true }
        return context
      }, {})

    const bindingValueNodes = node.bs.map(binding => binding.v)
    const bindingsResult = getUndefinedSymbols(bindingValueNodes, contextStack, builtin)
    const paramsResult = getUndefinedSymbols(node.p, contextStack.create(newContext), builtin)
    return joinSets(bindingsResult, paramsResult)
  },
}
