import type { ContextStack } from '../../evaluator/ContextStack'
import type { Context, EvaluateNode } from '../../evaluator/interface'
import type { GetUndefinedSymbols, UndefinedSymbols } from '../../getUndefinedSymbols'
import {
  type EvaluatedFunction,
  type LitsFunction,
  type SpecialExpressionNode,
  type SymbolNode,
  bindingTargetTypes,
} from '../../parser/types'
import { isAny } from '../../typeGuards/lits'
import { addToSet } from '../../utils'
import { FUNCTION_SYMBOL } from '../../utils/symbols'
import { getAllBindingTargetNames, walkDefaults } from '../bindingNode'
import type { Builtin, BuiltinSpecialExpression } from '../interface'
import type { Function } from '../utils'
import { assertNameNotDefined } from '../utils'
import type { specialExpressionTypes } from '../specialExpressionTypes'
import { assertUserDefinedSymbolNode } from '../../typeGuards/astNode'

export type DefnNode = SpecialExpressionNode<[typeof specialExpressionTypes['0_defn'], SymbolNode, Function, string]> // last string is docString
export type FunctionNode = SpecialExpressionNode<[typeof specialExpressionTypes['function'], SymbolNode, Function, string]> // last string is docString
export type FnNode = SpecialExpressionNode<[typeof specialExpressionTypes['0_fn'], Function]>

export const functionSpecialExpression: BuiltinSpecialExpression<LitsFunction, FunctionNode> = {
  arity: {},
  evaluate: (node, contextStack, { builtin, getUndefinedSymbols, evaluateNode }) => {
    const [, functionSymbol, fn, docString] = node[1]

    assertUserDefinedSymbolNode(functionSymbol, node[2])
    assertNameNotDefined(functionSymbol[1], contextStack, builtin, node[2])

    const evaluatedFunction = evaluateFunction(fn, contextStack, builtin, getUndefinedSymbols, evaluateNode)

    const min = evaluatedFunction[0].filter(arg => arg[0] !== bindingTargetTypes.rest && arg[1][1] === undefined).length
    const max = evaluatedFunction[0].some(arg => arg[0] === bindingTargetTypes.rest) ? undefined : evaluatedFunction[0].length
    const arity = { min: min > 0 ? min : undefined, max }

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node[2],
      functionType: 'UserDefined',
      name: functionSymbol[1],
      evaluatedfunction: evaluatedFunction,
      arity,
      docString,
    }

    contextStack.addValues({ [functionSymbol[1]]: litsFunction }, functionSymbol[2])
    return litsFunction
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const functionName = node[1][1][1]
    contextStack.addValues({ [functionName]: true }, node[1][1][2])
    const newContext: Context = { [functionName]: { value: true } }
    return getFunctionUnresolvedSymbols(node[1][2], contextStack, getUndefinedSymbols, builtin, evaluateNode, newContext)
  },
}

export const defnSpecialExpression: BuiltinSpecialExpression<LitsFunction, DefnNode> = {
  arity: {},
  evaluate: (node, contextStack, { builtin, getUndefinedSymbols, evaluateNode }) => {
    const [, functionSymbol, fn, docString] = node[1]

    assertUserDefinedSymbolNode(functionSymbol, node[2])
    assertNameNotDefined(functionSymbol[1], contextStack, builtin, node[2])

    const evaluatedFunction = evaluateFunction(fn, contextStack, builtin, getUndefinedSymbols, evaluateNode)

    const min = evaluatedFunction[0].filter(arg => arg[0] !== bindingTargetTypes.rest && arg[1][1] === undefined).length
    const arity = { min }

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node[2],
      functionType: 'UserDefined',
      name: functionSymbol[1],
      evaluatedfunction: evaluatedFunction,
      arity,
      docString,
    }

    contextStack.exportValues({ [functionSymbol[1]]: litsFunction }, functionSymbol[2])
    return litsFunction
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const functionName = node[1][1][1]
    const fn = node[1][2]
    contextStack.exportValues({ [functionName]: true }, node[1][1][2])
    const newContext: Context = { [functionName]: { value: true } }
    return getFunctionUnresolvedSymbols(fn, contextStack, getUndefinedSymbols, builtin, evaluateNode, newContext)
  },
}

export const fnSpecialExpression: BuiltinSpecialExpression<LitsFunction, FnNode> = {
  arity: {},
  evaluate: (node, contextStack, { builtin, getUndefinedSymbols, evaluateNode }) => {
    const fn = node[1][1]
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
      docString: '',
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
  functionNameContext?: Context,
): UndefinedSymbols {
  const result = new Set<string>()

  const contextStackWithFunctionName = functionNameContext ? contextStack.create(functionNameContext) : contextStack
  const newContext: Context = {}

  fn[0].forEach((arg) => {
    Object.assign(newContext, getAllBindingTargetNames(arg))

    walkDefaults(arg, (defaultNode) => {
      addToSet(result, getUndefinedSymbols([defaultNode], contextStack, builtin, evaluateNode))
    })
  })

  const newContextStack = contextStackWithFunctionName.create(newContext)
  const overloadResult = getUndefinedSymbols(fn[1], newContextStack, builtin, evaluateNode)
  addToSet(result, overloadResult)
  return result
}
