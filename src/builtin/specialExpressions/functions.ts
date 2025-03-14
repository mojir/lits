import type { SpecialExpressionNode } from '..'
import type { GetUndefinedSymbols, UndefinedSymbols } from '../../getUndefinedSymbols'
import { FunctionType } from '../../constants/constants'
import type { ContextStack } from '../../evaluator/ContextStack'
import type { Context, EvaluateAstNode } from '../../evaluator/interface'
import type {
  AstNode,
  CommonSpecialExpressionNode,
  EvaluatedFunctionOverload,
  LitsFunction,
  SymbolNode,
} from '../../parser/interface'
import { getTokenDebugData } from '../../tokenizer/token'
import { FUNCTION_SYMBOL } from '../../utils/symbols'
import type { Builtin, BuiltinSpecialExpression } from '../interface'
import type { FunctionOverload } from '../utils'
import { assertNameNotDefined } from '../utils'
import { addToSet } from '../../utils'

export interface DefnNode extends CommonSpecialExpressionNode<'defn'> {
  f: SymbolNode
  o: FunctionOverload[]
}

export interface FunctionNode extends CommonSpecialExpressionNode<'function'> {
  f: SymbolNode
  o: FunctionOverload[]
}

export interface FnNode extends CommonSpecialExpressionNode<'fn'> {
  p: AstNode[]
  o: FunctionOverload[]
}

export const functionSpecialExpression: BuiltinSpecialExpression<null, FunctionNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { builtin, evaluateAstNode }) => {
    const name = node.f.v

    assertNameNotDefined(name, contextStack, builtin, getTokenDebugData(node.token)?.sourceCodeInfo)

    const evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: getTokenDebugData(node.token)?.sourceCodeInfo,
      t: FunctionType.UserDefined,
      n: name,
      o: evaluatedFunctionOverloades,
    }

    contextStack.addValue(name, litsFunction)
    return null
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    contextStack.exportValue(node.f.v, true)
    const newContext: Context = { [node.f.v]: { value: true } }
    return addOverloadsUnresolvedSymbols(node.o, contextStack, getUndefinedSymbols, builtin, newContext)
  },
}

export const defnSpecialExpression: BuiltinSpecialExpression<null, DefnNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { builtin, evaluateAstNode }) => {
    const name = node.f.v

    assertNameNotDefined(name, contextStack, builtin, getTokenDebugData(node.token)?.sourceCodeInfo)

    const evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: getTokenDebugData(node.token)?.sourceCodeInfo,
      t: FunctionType.UserDefined,
      n: name,
      o: evaluatedFunctionOverloades,
    }

    contextStack.exportValue(name, litsFunction)
    return null
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    contextStack.exportValue(node.f.v, true)
    const newContext: Context = { [node.f.v]: { value: true } }
    return addOverloadsUnresolvedSymbols(node.o, contextStack, getUndefinedSymbols, builtin, newContext)
  },
}

export const fnSpecialExpression: BuiltinSpecialExpression<LitsFunction, FnNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: getTokenDebugData(node.token)?.sourceCodeInfo,
      t: FunctionType.UserDefined,
      n: undefined,
      o: evaluatedFunctionOverloades,
    }

    return litsFunction
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) =>
    addOverloadsUnresolvedSymbols(node.o, contextStack, getUndefinedSymbols, builtin),
}

function evaluateFunctionOverloades(
  node: SpecialExpressionNode,
  contextStack: ContextStack,
  evaluateAstNode: EvaluateAstNode,
): EvaluatedFunctionOverload[] {
  const evaluatedFunctionOverloades: EvaluatedFunctionOverload[] = []
  for (const functionOverload of (node as DefnNode | FnNode).o) {
    const functionContext: Context = {}
    for (const binding of functionOverload.as.b) {
      const bindingValueNode = binding.v
      const bindingValue = evaluateAstNode(bindingValueNode, contextStack)
      functionContext[binding.n] = { value: bindingValue }
    }

    const evaluatedFunctionOverload: EvaluatedFunctionOverload = {
      as: {
        mandatoryArguments: functionOverload.as.m,
        restArgument: functionOverload.as.r,
      },
      a: functionOverload.a,
      b: functionOverload.b,
      f: functionContext,
    }

    evaluatedFunctionOverloades.push(evaluatedFunctionOverload)
  }
  return evaluatedFunctionOverloades
}

function addOverloadsUnresolvedSymbols(
  overloads: FunctionOverload[],
  contextStack: ContextStack,
  getUndefinedSymbols: GetUndefinedSymbols,
  builtin: Builtin,
  functionNameContext?: Context,
): UndefinedSymbols {
  const result = new Set<string>()
  const contextStackWithFunctionName = functionNameContext ? contextStack.create(functionNameContext) : contextStack
  for (const overload of overloads) {
    const newContext: Context = {}
    overload.as.b.forEach((binding) => {
      const bindingResult = getUndefinedSymbols([binding.v], contextStack, builtin)
      addToSet(result, bindingResult)
      newContext[binding.n] = { value: true }
    })
    overload.as.m.forEach((arg) => {
      newContext[arg] = { value: true }
    })
    if (typeof overload.as.r === 'string')
      newContext[overload.as.r] = { value: true }

    const newContextStack = contextStackWithFunctionName.create(newContext)
    const overloadResult = getUndefinedSymbols(overload.b, newContextStack, builtin)
    addToSet(result, overloadResult)
  }
  return result
}
