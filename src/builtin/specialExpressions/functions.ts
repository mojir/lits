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
import { addToSet } from '../../utils'
import { FUNCTION_SYMBOL } from '../../utils/symbols'
import { evalueateBindingNodeValues, getAllBindingTargetNames } from '../bindingNode'
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
  evaluate: (node, contextStack, { builtin, evaluateAstNode }) => {
    const name = node.functionName.value

    assertNameNotDefined(name, contextStack, builtin, node.sourceCodeInfo)

    const evaluatedFunctionOverloades = evaluateFunction(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node.sourceCodeInfo,
      functionType: 'UserDefined',
      name,
      evaluatedfunction: evaluatedFunctionOverloades,
    }

    contextStack.addValues({ [name]: litsFunction })
    return null
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    contextStack.exportValues({ [node.functionName.value]: true })
    const newContext: Context = { [node.functionName.value]: { value: true } }
    return addFunctionUnresolvedSymbols(node.function, contextStack, getUndefinedSymbols, builtin, newContext)
  },
}

export const defnSpecialExpression: BuiltinSpecialExpression<null, DefnNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { builtin, evaluateAstNode }) => {
    const name = node.functionName.value

    assertNameNotDefined(name, contextStack, builtin, node.sourceCodeInfo)

    const evaluatedFunctionOverloades = evaluateFunction(node, contextStack, evaluateAstNode)

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

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    contextStack.exportValues({ [node.functionName.value]: true })
    const newContext: Context = { [node.functionName.value]: { value: true } }
    return addFunctionUnresolvedSymbols(node.function, contextStack, getUndefinedSymbols, builtin, newContext)
  },
}

export const fnSpecialExpression: BuiltinSpecialExpression<LitsFunction, FnNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const evaluatedFunctionOverloades = evaluateFunction(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node.sourceCodeInfo,
      functionType: 'UserDefined',
      name: undefined,
      evaluatedfunction: evaluatedFunctionOverloades,
    }

    return litsFunction
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) =>
    addFunctionUnresolvedSymbols(node.function, contextStack, getUndefinedSymbols, builtin),
}

function evaluateFunction(
  node: SpecialExpressionNode,
  contextStack: ContextStack,
  evaluateAstNode: EvaluateAstNode,
): EvaluatedFunction {
  const fn = (node as DefnNode | FnNode).function
  const functionContext: Context = {}
  for (const binding of fn.bindingNodes) {
    const bindingValueNode = binding.value
    const bindingValue = evaluateAstNode(bindingValueNode, contextStack)
    const valueRecord = evalueateBindingNodeValues(binding, bindingValue, astNode => evaluateAstNode(astNode, contextStack))
    Object.entries(valueRecord).forEach(([name, value]) => {
      functionContext[name] = { value }
    })
  }

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
  functionNameContext?: Context,
): UndefinedSymbols {
  const result = new Set<string>()

  const contextStackWithFunctionName = functionNameContext ? contextStack.create(functionNameContext) : contextStack
  const newContext: Context = {}
  fn.bindingNodes.forEach((binding) => {
    const bindingResult = getUndefinedSymbols([binding.value], contextStack, builtin)
    addToSet(result, bindingResult)
    Object.assign(newContext, getAllBindingTargetNames(binding.target))
  })
  fn.arguments.forEach((arg) => {
    Object.assign(newContext, getAllBindingTargetNames(arg))
  })

  const newContextStack = contextStackWithFunctionName.create(newContext)
  const overloadResult = getUndefinedSymbols(fn.body, newContextStack, builtin)
  addToSet(result, overloadResult)
  return result
}
