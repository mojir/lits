import type { SpecialExpressionNode } from '..'
import type { ContextStack } from '../../evaluator/ContextStack'
import type { Context, EvaluateAstNode } from '../../evaluator/interface'
import type { GetUndefinedSymbols, UndefinedSymbols } from '../../getUndefinedSymbols'
import type {
  AstNode,
  CommonSpecialExpressionNode,
  EvaluatedFunction,
  LitsFunction,
  SymbolNode,
} from '../../parser/types'
import { isAny } from '../../typeGuards/lits'
import { addToSet } from '../../utils'
import { FUNCTION_SYMBOL } from '../../utils/symbols'
import { getAllBindingTargetNames } from '../bindingNode'
import type { Builtin, BuiltinSpecialExpression } from '../interface'
import type { Function } from '../utils'
import { assertNameNotDefined } from '../utils'

export interface DefnNode extends CommonSpecialExpressionNode<'defn'> {
  functionName: SymbolNode
  function: Function
}

export interface FunctionNode extends CommonSpecialExpressionNode<'function'> {
  functionName: SymbolNode
  function: Function
}

export interface FnNode extends CommonSpecialExpressionNode<'fn'> {
  params: AstNode[]
  function: Function
}

export const functionSpecialExpression: BuiltinSpecialExpression<null, FunctionNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { builtin, getUndefinedSymbols, evaluateAstNode }) => {
    const name = node.functionName.value

    assertNameNotDefined(name, contextStack, builtin, node.sourceCodeInfo)

    const evaluatedFunction = evaluateFunction(node, contextStack, builtin, getUndefinedSymbols, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node.sourceCodeInfo,
      functionType: 'UserDefined',
      name,
      evaluatedfunction: evaluatedFunction,
    }

    contextStack.addValues({ [name]: litsFunction })
    return null
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateAstNode }) => {
    contextStack.addValues({ [node.functionName.value]: true })
    const newContext: Context = { [node.functionName.value]: { value: true } }
    return addFunctionUnresolvedSymbols(node.function, contextStack, getUndefinedSymbols, builtin, evaluateAstNode, newContext)
  },
}

export const defnSpecialExpression: BuiltinSpecialExpression<null, DefnNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { builtin, getUndefinedSymbols, evaluateAstNode }) => {
    const name = node.functionName.value

    assertNameNotDefined(name, contextStack, builtin, node.sourceCodeInfo)

    const evaluatedFunctionOverloades = evaluateFunction(node, contextStack, builtin, getUndefinedSymbols, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node.sourceCodeInfo,
      functionType: 'UserDefined',
      name,
      evaluatedfunction: evaluatedFunctionOverloades,
    }

    contextStack.exportValues({ [name]: litsFunction })
    return null
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateAstNode }) => {
    contextStack.exportValues({ [node.functionName.value]: true })
    const newContext: Context = { [node.functionName.value]: { value: true } }
    return addFunctionUnresolvedSymbols(node.function, contextStack, getUndefinedSymbols, builtin, evaluateAstNode, newContext)
  },
}

export const fnSpecialExpression: BuiltinSpecialExpression<LitsFunction, FnNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { builtin, getUndefinedSymbols, evaluateAstNode }) => {
    const evaluatedFunction = evaluateFunction(node, contextStack, builtin, getUndefinedSymbols, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node.sourceCodeInfo,
      functionType: 'UserDefined',
      name: undefined,
      evaluatedfunction: evaluatedFunction,
    }

    return litsFunction
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateAstNode }) =>
    addFunctionUnresolvedSymbols(node.function, contextStack, getUndefinedSymbols, builtin, evaluateAstNode),
}

function evaluateFunction(
  node: SpecialExpressionNode,
  contextStack: ContextStack,
  builtin: Builtin,
  getUndefinedSymbols: GetUndefinedSymbols,
  evaluateAstNode: EvaluateAstNode,
): EvaluatedFunction {
  const fn = (node as DefnNode | FnNode).function
  const functionContext: Context = {}

  const context = fn.arguments.reduce((ctx: Context, arg) => {
    Object.keys(getAllBindingTargetNames(arg)).forEach((name) => {
      ctx[name] = { value: null }
    })
    return ctx
  }, {})
  const undefinedSymbols = getUndefinedSymbols(fn.body, contextStack.new(context), builtin, evaluateAstNode)
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
  evaluateAstNode: EvaluateAstNode,
  functionNameContext?: Context,
): UndefinedSymbols {
  const result = new Set<string>()

  const contextStackWithFunctionName = functionNameContext ? contextStack.create(functionNameContext) : contextStack
  const newContext: Context = {}

  fn.arguments.forEach((arg) => {
    Object.assign(newContext, getAllBindingTargetNames(arg))
  })

  const newContextStack = contextStackWithFunctionName.create(newContext)
  const overloadResult = getUndefinedSymbols(fn.body, newContextStack, builtin, evaluateAstNode)
  addToSet(result, overloadResult)
  return result
}
