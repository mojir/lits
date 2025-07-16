import type { ContextStack } from '../../evaluator/ContextStack'
import type { Context, EvaluateNode } from '../../evaluator/interface'
import type { GetUndefinedSymbols, UndefinedSymbols } from '../../getUndefinedSymbols'
import {
  type EvaluatedFunction,
  type LitsFunction,
  type SpecialExpressionNode,
  bindingTargetTypes,
} from '../../parser/types'
import { isAny } from '../../typeGuards/lits'
import { addToSet } from '../../utils'
import { FUNCTION_SYMBOL } from '../../utils/symbols'
import { getAllBindingTargetNames, walkDefaults } from '../bindingNode'
import type { Builtin, BuiltinSpecialExpression } from '../interface'
import type { Function } from '../utils'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type LambdaNode = SpecialExpressionNode<[typeof specialExpressionTypes['0_lambda'], Function, string]>

export const lambdaSpecialExpression: BuiltinSpecialExpression<LitsFunction, LambdaNode> = {
  arity: {},
  evaluate: (node, contextStack, { builtin, getUndefinedSymbols, evaluateNode }) => {
    const fn = node[1][1]
    const docString = node[1][2]
    const evaluatedFunction = evaluateFunction(fn, contextStack, builtin, getUndefinedSymbols, evaluateNode)

    const min = evaluatedFunction[0].filter(arg => arg[0] !== bindingTargetTypes.rest && arg[1][1] === undefined).length
    const max = evaluatedFunction[0].some(arg => arg[0] === bindingTargetTypes.rest) ? undefined : evaluatedFunction[0].length
    const arity = { min: min > 0 ? min : undefined, max }

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node[2],
      functionType: 'UserDefined',
      name: undefined,
      evaluatedfunction: evaluatedFunction,
      arity,
      docString,
    }

    return litsFunction
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const fn = node[1][1]
    return getFunctionUnresolvedSymbols(fn, contextStack, getUndefinedSymbols, builtin, evaluateNode)
  },

}

function evaluateFunction(
  fn: Function,
  contextStack: ContextStack,
  builtin: Builtin,
  getUndefinedSymbols: GetUndefinedSymbols,
  evaluateNode: EvaluateNode,
): EvaluatedFunction {
  const functionContext: Context = {}

  const context = fn[0].reduce((ctx: Context, arg) => {
    Object.keys(getAllBindingTargetNames(arg)).forEach((name) => {
      ctx[name] = { value: null }
    })
    return ctx
  }, {})
  const undefinedSymbols = getUndefinedSymbols(fn[1], contextStack.new(context), builtin, evaluateNode)
  undefinedSymbols.forEach((name) => {
    const value = contextStack.getValue(name)
    if (isAny(value)) {
      functionContext[name] = { value }
    }
  })

  const evaluatedFunction: EvaluatedFunction = [
    fn[0],
    fn[1],
    functionContext,
  ]

  return evaluatedFunction
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
