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
import { Ast } from '../parser/interface'
import { builtin } from '../builtin'
import { reservedNamesRecord } from '../reservedNames'
import { asLispishFunction, asNotUndefined, isLispishFunction, isUserDefinedLispishFunction } from '../utils'
import { Context, EvaluateAstNode, EvaluateLispishFunction, VariableScope } from './interface'
import { normalExpressions } from '../builtin/normalExpressions'
import { ReturnFromSignal, ReturnSignal } from '../errors'

export function evaluate(ast: Ast, globalVariables: VariableScope, topScope: Context): unknown {
  // First element is the global context. E.g. setq will assign to this if no local variable is available
  // Second element is the context sent in from outside (this should never be mutated)
  const contextStack: Context[] = [topScope, { variables: globalVariables, functions: {} }]

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

function evaluateName({ value }: NameNode, contextStack: Context[]): unknown {
  for (const context of contextStack) {
    if (Object.getOwnPropertyDescriptor(context.variables, value)) {
      return context.variables[value]
    }
  }
  throw Error(`Undefined identifier ${value}`)
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
    const args = lispishFunction.arguments
    if (lispishFunction.varArgs) {
      if (params.length < args.length - 1) {
        throw Error(
          `Function "${lispishFunction.name}" requires at least ${args.length - 1} arguments. Got ${params.length}`,
        )
      }
    } else {
      if (args.length !== params.length) {
        throw Error(`Function "${lispishFunction.name}" requires ${args.length} arguments. Got ${params.length}`)
      }
    }

    for (let i = 0; i < args.length - 1; i += 1) {
      const param = params[i]
      const key = asNotUndefined(args[i])
      if (isLispishFunction(param)) {
        newContext.functions[key] = param
      } else {
        newContext.variables[key] = param
      }
    }

    const lastIndex = args.length - 1
    if (lastIndex >= 0) {
      if (lispishFunction.varArgs) {
        const rest: unknown[] = []
        for (let i = lastIndex; i < params.length; i += 1) {
          const param = params[i]
          if (isLispishFunction(param)) {
            throw Error('A function cannot be a &rest parameter')
          }
          rest.push(param)
        }
        const key = asNotUndefined(args[lastIndex])
        newContext.variables[key] = rest
      } else {
        const param = params[lastIndex]
        const key = asNotUndefined(args[lastIndex])
        if (isLispishFunction(param)) {
          newContext.functions[key] = param
        } else {
          newContext.variables[key] = param
        }
      }
    }

    try {
      let result: unknown = undefined
      for (const node of lispishFunction.body) {
        result = evaluateAstNode(node, [newContext, ...contextStack])
      }
      return result
    } catch (error) {
      if (error instanceof ReturnSignal) {
        return error.value
      }
      if (error instanceof ReturnFromSignal && lispishFunction.name === error.blockName) {
        return error.value
      }
      throw error
    }
  } else {
    const normalExpression = asNotUndefined(normalExpressions[lispishFunction.builtin])
    return normalExpression.evaluate(params, contextStack, { evaluateLispishFunction })
  }
}

function evaluateBuiltinNormalExpression(
  node: NormalExpressionNode,
  params: unknown[],
  contextStack: Context[],
): unknown {
  const normalExpressionEvaluator = asNotUndefined(builtin.normalExpressions[node.name]).evaluate

  return normalExpressionEvaluator(params, contextStack, { evaluateLispishFunction })
}

function evaluateSpecialExpression(node: SpecialExpressionNode, contextStack: Context[]): unknown {
  const specialExpressionEvaluator = asNotUndefined(builtin.specialExpressions[node.name]).evaluate
  return specialExpressionEvaluator(node, contextStack, evaluateAstNode)
}

function evaluateExpressionExpression(node: ExpressionExpressionNode, contextStack: Context[]): unknown {
  const lispishFunction = asLispishFunction(evaluateAstNode(node.expression, contextStack))

  const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))

  return evaluateLispishFunction(lispishFunction, params, contextStack)
}
