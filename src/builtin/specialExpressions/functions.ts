import type { SpecialExpressionNode } from '..'
import type { GetUndefinedSymbols, UndefinedSymbols } from '../../getUndefinedSymbols'
import type { ContextStack } from '../../evaluator/ContextStack'
import type { Context, EvaluateAstNode } from '../../evaluator/interface'
import type {
  AstNode,
  CommonSpecialExpressionNode,
  EvaluatedFunction,
  LitsFunction,
  SymbolNode,
} from '../../parser/types'
import { tokenSourceCodeInfo } from '../../tokenizer/token'
import { FUNCTION_SYMBOL } from '../../utils/symbols'
import type { Builtin, BuiltinSpecialExpression } from '../interface'
import type { FunctionOverload } from '../utils'
import { assertNameNotDefined } from '../utils'
import { addToSet } from '../../utils'

export interface DefnNode extends CommonSpecialExpressionNode<'defn'> {
  f: SymbolNode
  function: FunctionOverload
}

export interface FunctionNode extends CommonSpecialExpressionNode<'function'> {
  f: SymbolNode
  function: FunctionOverload
}

export interface FnNode extends CommonSpecialExpressionNode<'fn'> {
  params: AstNode[]
  function: FunctionOverload
}

export const functionSpecialExpression: BuiltinSpecialExpression<null, FunctionNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { builtin, evaluateAstNode }) => {
    const name = node.f.value

    assertNameNotDefined(name, contextStack, builtin, tokenSourceCodeInfo(node.token))

    const evaluatedFunctionOverloades = evaluateFunction(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: tokenSourceCodeInfo(node.token),
      functionType: 'UserDefined',
      name,
      function: evaluatedFunctionOverloades,
    }

    contextStack.addValue(name, litsFunction)
    return null
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    contextStack.exportValue(node.f.value, true)
    const newContext: Context = { [node.f.value]: { value: true } }
    return addFunctionUnresolvedSymbols(node.function, contextStack, getUndefinedSymbols, builtin, newContext)
  },
}

export const defnSpecialExpression: BuiltinSpecialExpression<null, DefnNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { builtin, evaluateAstNode }) => {
    const name = node.f.value

    assertNameNotDefined(name, contextStack, builtin, tokenSourceCodeInfo(node.token))

    const evaluatedFunctionOverloades = evaluateFunction(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: tokenSourceCodeInfo(node.token),
      functionType: 'UserDefined',
      name,
      function: evaluatedFunctionOverloades,
    }

    contextStack.exportValue(name, litsFunction)
    return null
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    contextStack.exportValue(node.f.value, true)
    const newContext: Context = { [node.f.value]: { value: true } }
    return addFunctionUnresolvedSymbols(node.function, contextStack, getUndefinedSymbols, builtin, newContext)
  },
}

export const fnSpecialExpression: BuiltinSpecialExpression<LitsFunction, FnNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const evaluatedFunctionOverloades = evaluateFunction(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: tokenSourceCodeInfo(node.token),
      functionType: 'UserDefined',
      name: undefined,
      function: evaluatedFunctionOverloades,
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
  for (const binding of fn.as.b) {
    const bindingValueNode = binding.value
    const bindingValue = evaluateAstNode(bindingValueNode, contextStack)
    functionContext[binding.name] = { value: bindingValue }
  }

  const evaluatedFunction: EvaluatedFunction = {
    arguments: {
      mandatoryArguments: fn.as.m,
      restArgument: fn.as.r,
    },
    arity: fn.a,
    body: fn.b,
    context: functionContext,
  }

  return evaluatedFunction
}

function addFunctionUnresolvedSymbols(
  fn: FunctionOverload,
  contextStack: ContextStack,
  getUndefinedSymbols: GetUndefinedSymbols,
  builtin: Builtin,
  functionNameContext?: Context,
): UndefinedSymbols {
  const result = new Set<string>()

  const contextStackWithFunctionName = functionNameContext ? contextStack.create(functionNameContext) : contextStack
  const newContext: Context = {}
  fn.as.b.forEach((binding) => {
    const bindingResult = getUndefinedSymbols([binding.value], contextStack, builtin)
    addToSet(result, bindingResult)
    newContext[binding.name] = { value: true }
  })
  fn.as.m.forEach((arg) => {
    newContext[arg] = { value: true }
  })
  if (typeof fn.as.r === 'string')
    newContext[fn.as.r] = { value: true }

  const newContextStack = contextStackWithFunctionName.create(newContext)
  const overloadResult = getUndefinedSymbols(fn.b, newContextStack, builtin)
  addToSet(result, overloadResult)
  return result
}
