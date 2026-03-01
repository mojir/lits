import type { ContextStack } from '../../evaluator/ContextStack'
import type { Context, EvaluateNode } from '../../evaluator/interface'
import type { GetUndefinedSymbols, UndefinedSymbols } from '../../getUndefinedSymbols'
import type {
  LitsFunction,
  SpecialExpressionNode,
} from '../../parser/types'
import { addToSet } from '../../utils'
import { getAllBindingTargetNames, walkDefaults } from '../bindingNode'
import type { Builtin, BuiltinSpecialExpression } from '../interface'
import type { Function } from '../utils'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type LambdaNode = SpecialExpressionNode<[typeof specialExpressionTypes['0_lambda'], Function, string]>

export const lambdaSpecialExpression: BuiltinSpecialExpression<LitsFunction, LambdaNode> = {
  arity: {},
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const fn = node[1][1]
    return getFunctionUnresolvedSymbols(fn, contextStack, getUndefinedSymbols, builtin, evaluateNode)
  },

}

function getFunctionUnresolvedSymbols(
  fn: Function,
  contextStack: ContextStack,
  getUndefinedSymbols: GetUndefinedSymbols,
  builtin: Builtin,
  evaluateNode: EvaluateNode,
): UndefinedSymbols {
  const result = new Set<string>()
  const newContext: Context = { self: { value: null } }

  fn[0].forEach((arg) => {
    Object.assign(newContext, getAllBindingTargetNames(arg))

    walkDefaults(arg, (defaultNode) => {
      addToSet(result, getUndefinedSymbols([defaultNode], contextStack, builtin, evaluateNode))
    })
  })

  const newContextStack = contextStack.create(newContext)
  const overloadResult = getUndefinedSymbols(fn[1], newContextStack, builtin, evaluateNode)
  addToSet(result, overloadResult)
  return result
}
