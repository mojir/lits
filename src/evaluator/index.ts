import {
  NormalExpressionNode,
  SpecialExpressionNode,
  NameNode,
  NumberNode,
  StringNode,
  ReservedNameNode,
  LispishFunction,
  ExpressionExpressionNode,
} from '../parser/interface'
import get from 'lodash/get'
import { Ast } from '../parser/interface'
import { builtin } from '../builtin'
import { reservedNamesRecord } from '../reservedNames'
import { asLispishFunction, asNotUndefined, isLispishFunction, isUserDefinedLispishFunction } from '../utils'
import { Context, EvaluateAstNode, EvaluateLispishFunction, VariableScope } from './interface'
import { normalExpressions } from '../builtin/normalExpressions'

export function evaluate(
  ast: Ast,
  globalVariables: VariableScope = {},
  topScope: Context = { variables: {}, functions: {} },
): unknown {
  const frozenGlobalVariables = { ...globalVariables }
  Object.freeze(frozenGlobalVariables)

  // First element is the global context. E.g. setq will assign to this if no local variable is available
  // Second element is the context sent in from outside (this should never be mutated)
  const contextStack: Context[] = [topScope, { variables: frozenGlobalVariables, functions: {} }]

  let result: unknown
  for (const node of ast.body) {
    result = evaluateAstNode(node, contextStack)
  }
  return result
}

export const evaluateAstNode: EvaluateAstNode = (node, contextStack) => {
  switch (node.type) {
    case 'Number':
      return evaluateNumber(node)
    case 'String':
      return evaluateString(node)
    case 'Name':
      return evaluateName(node, contextStack)
    case 'ReservedName':
      return evaluateReservedName(node)
    case 'NormalExpression':
      return evaluateNormalExpression(node, contextStack)
    case 'SpecialExpression':
      return evaluateSpecialExpression(node, contextStack)
    case 'ExpressionExpression':
      return evaluateExpressionExpression(node, contextStack)
  }
}

function evaluateNumber(node: NumberNode): number {
  return node.value
}

function evaluateString(node: StringNode): string {
  return node.value
}

function evaluateReservedName(node: ReservedNameNode): unknown {
  return asNotUndefined(reservedNamesRecord[node.value]).value
}

function evaluateName(node: NameNode, contextStack: Context[]): unknown {
  const path = node.value
  const dotPosition = path.indexOf('.')
  const bracketPosition = path.indexOf('[')
  const index =
    dotPosition === -1 ? bracketPosition : bracketPosition === -1 ? dotPosition : Math.min(dotPosition, bracketPosition)
  const contextPrefix = index === -1 ? path : path.substring(0, index)
  for (const context of contextStack) {
    if (Object.getOwnPropertyDescriptor(context.variables, contextPrefix)) {
      return get(context.variables, path)
    }
  }
  throw Error(`Undefined identifier ${path}`)
}

function evaluateNormalExpression(node: NormalExpressionNode, contextStack: Context[]): unknown {
  const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))

  let lispishFunction: LispishFunction | undefined = undefined
  for (const context of contextStack) {
    lispishFunction = context.functions[node.name]
    if (lispishFunction) {
      break
    }
  }

  if (lispishFunction) {
    return evaluateLispishFunction(lispishFunction, params, contextStack)
  } else {
    return evaluateBuiltinNormalExpression(node, params, contextStack)
  }
}

const evaluateLispishFunction: EvaluateLispishFunction = (
  lispishFunction: LispishFunction,
  params: unknown[],
  contextStack: Context[],
) => {
  const newContext: Context = {
    functions: {},
    variables: {},
  }

  if (isUserDefinedLispishFunction(lispishFunction)) {
    if (lispishFunction.arguments.length !== params.length) {
      throw Error(
        `Function "${lispishFunction.name}" requires ${lispishFunction.arguments.length} arguments. Got ${params.length}`,
      )
    }

    for (let i = 0; i < params.length; i += 1) {
      const param = params[i]
      const key = lispishFunction.arguments[i]
      if (key === undefined) {
        throw Error('Expected string, got undefined')
      }
      if (isLispishFunction(param)) {
        newContext.functions[key] = param
      } else {
        newContext.variables[key] = param
      }
    }

    let result: unknown = undefined
    for (const node of lispishFunction.body) {
      result = evaluateAstNode(node, [newContext, ...contextStack])
    }
    return result
  } else {
    const normalExpression = normalExpressions[lispishFunction.builtin]
    if (normalExpression) {
      return normalExpression.evaluate(params, contextStack, { evaluateLispishFunction })
    }
    throw Error(`Could not find builtin normal expression with name "${lispishFunction.builtin}"`)
  }
}

function evaluateBuiltinNormalExpression(
  node: NormalExpressionNode,
  params: unknown[],
  contextStack: Context[],
): unknown {
  const normalExpressionEvaluator = asNotUndefined(builtin.normalExpressions[node.name]).evaluate

  try {
    return normalExpressionEvaluator(params, contextStack, { evaluateLispishFunction })
  } catch (e: unknown) {
    if (e instanceof Error) {
      throw Error(e.message + '\n' + JSON.stringify(node, null, 2))
    }
    throw Error(e + '\n' + JSON.stringify(node, null, 2))
  }
}

function evaluateSpecialExpression(node: SpecialExpressionNode, contextStack: Context[]): unknown {
  const specialExpressionEvaluator = builtin.specialExpressions[node.name]?.evaluate
  if (specialExpressionEvaluator) {
    return specialExpressionEvaluator(node, contextStack, evaluateAstNode)
  }
  throw Error(`Unrecognized special expression node: ${node.name}`)
}

function evaluateExpressionExpression(node: ExpressionExpressionNode, contextStack: Context[]): unknown {
  const lispishFunction = asLispishFunction(evaluateAstNode(node.expression, contextStack))

  const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))

  return evaluateLispishFunction(lispishFunction, params, contextStack)
}
