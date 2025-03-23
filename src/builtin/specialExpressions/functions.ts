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
import { getAllBindingTargetNames, walkDefaults } from '../bindingNode'
import type { Builtin, BuiltinSpecialExpression } from '../interface'
import type { Function } from '../utils'
import { assertNameNotDefined } from '../utils'
import type { specialExpressionTypes } from '../specialExpressionTypes'
import { assertUserDefinedSymbolNode } from '../../typeGuards/astNode'

export type DefnNode = SpecialExpressionNode<[typeof specialExpressionTypes['0_defn'], SymbolNode, Function]>
export type FunctionNode = SpecialExpressionNode<[typeof specialExpressionTypes['function'], SymbolNode, Function]>
export type FnNode = SpecialExpressionNode<[typeof specialExpressionTypes['0_fn'], Function]>

export const functionSpecialExpression: BuiltinSpecialExpression<null, FunctionNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { builtin, getUndefinedSymbols, evaluateNode }) => {
    const [, functionSymbol, fn] = node[1]

    assertUserDefinedSymbolNode(functionSymbol, node[2])
    assertNameNotDefined(functionSymbol[1], contextStack, builtin, node[2])

    const evaluatedFunction = evaluateFunction(fn, contextStack, builtin, getUndefinedSymbols, evaluateNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node[2],
      functionType: 'UserDefined',
      name: functionSymbol[1],
      evaluatedfunction: evaluatedFunction,
    }

    contextStack.addValues({ [functionSymbol[1]]: litsFunction })
    return null
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const functionName = node[1][1][1]
    contextStack.addValues({ [functionName]: true })
    const newContext: Context = { [functionName]: { value: true } }
    return getFunctionUnresolvedSymbols(node[1][2], contextStack, getUndefinedSymbols, builtin, evaluateNode, newContext)
  },
}

export const defnSpecialExpression: BuiltinSpecialExpression<null, DefnNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { builtin, getUndefinedSymbols, evaluateNode }) => {
    const [, functionSymbol, fn] = node[1]

    assertUserDefinedSymbolNode(functionSymbol, node[2])
    assertNameNotDefined(functionSymbol[1], contextStack, builtin, node[2])

    const evaluatedFunctionOverloades = evaluateFunction(fn, contextStack, builtin, getUndefinedSymbols, evaluateNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node[2],
      functionType: 'UserDefined',
      name: functionSymbol[1],
      evaluatedfunction: evaluatedFunctionOverloades,
    }

    contextStack.exportValues({ [functionSymbol[1]]: litsFunction })
    return null
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const functionName = node[1][1][1]
    const fn = node[1][2]
    contextStack.exportValues({ [functionName]: true })
    const newContext: Context = { [functionName]: { value: true } }
    return getFunctionUnresolvedSymbols(fn, contextStack, getUndefinedSymbols, builtin, evaluateNode, newContext)
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
