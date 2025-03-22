import type { ContextStack } from '../../evaluator/ContextStack'
import type { Context, EvaluateNode } from '../../evaluator/interface'
import type { GetUndefinedSymbols, UndefinedSymbols } from '../../getUndefinedSymbols'
import type {
  EvaluatedFunction,
  LitsFunction,
  SpecialExpressionNode,
  SymbolNode,
} from '../../parser/types'
import { isAny } from '../../typeGuards/lits'
import { addToSet } from '../../utils'
import { FUNCTION_SYMBOL } from '../../utils/symbols'
import { getAllBindingTargetNames } from '../bindingNode'
import type { Builtin, BuiltinSpecialExpression } from '../interface'
import type { Function } from '../utils'
import { assertNameNotDefined } from '../utils'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type DefnNode = SpecialExpressionNode<[typeof specialExpressionTypes['defn'], SymbolNode, Function]>
export type FunctionNode = SpecialExpressionNode<[typeof specialExpressionTypes['function'], SymbolNode, Function]>
export type FnNode = SpecialExpressionNode<[typeof specialExpressionTypes['fn'], Function]>

export const functionSpecialExpression: BuiltinSpecialExpression<null, FunctionNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { builtin, getUndefinedSymbols, evaluateNode }) => {
    const [, [, functionName], fn] = node[1]

    assertNameNotDefined(functionName, contextStack, builtin, node[2])

    const evaluatedFunction = evaluateFunction(fn, contextStack, builtin, getUndefinedSymbols, evaluateNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node[2],
      functionType: 'UserDefined',
      name: functionName,
      evaluatedfunction: evaluatedFunction,
    }

    contextStack.addValues({ [functionName]: litsFunction })
    return null
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const functionName = node[1][1][1]
    contextStack.addValues({ [functionName]: true })
    const newContext: Context = { [functionName]: { value: true } }
    return addFunctionUnresolvedSymbols(node[1][2], contextStack, getUndefinedSymbols, builtin, evaluateNode, newContext)
  },
}

export const defnSpecialExpression: BuiltinSpecialExpression<null, DefnNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { builtin, getUndefinedSymbols, evaluateNode }) => {
    const [, [, functionName], fn] = node[1]

    assertNameNotDefined(functionName, contextStack, builtin, node[2])

    const evaluatedFunctionOverloades = evaluateFunction(fn, contextStack, builtin, getUndefinedSymbols, evaluateNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node[2],
      functionType: 'UserDefined',
      name: functionName,
      evaluatedfunction: evaluatedFunctionOverloades,
    }

    contextStack.exportValues({ [functionName]: litsFunction })
    return null
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const functionName = node[1][1][1]
    const fn = node[1][2]
    contextStack.exportValues({ [functionName]: true })
    const newContext: Context = { [functionName]: { value: true } }
    return addFunctionUnresolvedSymbols(fn, contextStack, getUndefinedSymbols, builtin, evaluateNode, newContext)
  },
}

export const fnSpecialExpression: BuiltinSpecialExpression<LitsFunction, FnNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { builtin, getUndefinedSymbols, evaluateNode }) => {
    const fn = node[1][1]
    const evaluatedFunction = evaluateFunction(fn, contextStack, builtin, getUndefinedSymbols, evaluateNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node[2],
      functionType: 'UserDefined',
      name: undefined,
      evaluatedfunction: evaluatedFunction,
    }

    return litsFunction
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const fn = node[1][1]
    return addFunctionUnresolvedSymbols(fn, contextStack, getUndefinedSymbols, builtin, evaluateNode)
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

  const context = fn.arguments.reduce((ctx: Context, arg) => {
    Object.keys(getAllBindingTargetNames(arg)).forEach((name) => {
      ctx[name] = { value: null }
    })
    return ctx
  }, {})
  const undefinedSymbols = getUndefinedSymbols(fn.body, contextStack.new(context), builtin, evaluateNode)
  undefinedSymbols.forEach((name) => {
    const value = contextStack.getValue(name)
    if (isAny(value)) {
      functionContext[name] = { value }
    }
  })

  const evaluatedFunction: EvaluatedFunction = {
    arguments: fn.arguments,
    body: fn.body,
    context: functionContext,
  }

  return evaluatedFunction
}

function addFunctionUnresolvedSymbols(
  fn: Function,
  contextStack: ContextStack,
  getUndefinedSymbols: GetUndefinedSymbols,
  builtin: Builtin,
  evaluateNode: EvaluateNode,
  functionNameContext?: Context,
): UndefinedSymbols {
  const result = new Set<string>()

  const contextStackWithFunctionName = functionNameContext ? contextStack.create(functionNameContext) : contextStack
  const newContext: Context = {}

  fn.arguments.forEach((arg) => {
    Object.assign(newContext, getAllBindingTargetNames(arg))
  })

  const newContextStack = contextStackWithFunctionName.create(newContext)
  const overloadResult = getUndefinedSymbols(fn.body, newContextStack, builtin, evaluateNode)
  addToSet(result, overloadResult)
  return result
}
