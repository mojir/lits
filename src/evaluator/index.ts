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
import { Context, EvaluateAstNode, EvaluateLispishFunction } from './interface'
import { normalExpressions } from '../builtin/normalExpressions'
import { ReturnFromSignal, ReturnSignal } from '../errors'

export function evaluate(ast: Ast, globalScope: Context, importScope: Context): unknown {
  // First element is the global context. E.g. setq will assign to this if no local variable is available
  // Second element is the context sent in from outside (this should never be mutated)
  const contextStack: Context[] = [globalScope, importScope]

  let result: unknown
  for (const node of ast.body) {
    result = evaluateAstNode(node, contextStack)
  }
  return result
}

export const evaluateAstNode: EvaluateAstNode = (node, contextStack) => {
  switch (node.type) {
    case `Number`:
      return evaluateNumber(node)
    case `String`:
      return evaluateString(node)
    case `Name`:
      return evaluateName(node, contextStack)
    case `ReservedName`:
      return evaluateReservedName(node)
    case `NormalExpression`:
      return evaluateNormalExpression(node, contextStack)
    case `SpecialExpression`:
      return evaluateSpecialExpression(node, contextStack)
    case `ExpressionExpression`:
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
  return asNotUndefined(reservedNamesRecord[node.value], `${node.value} is not a reserved name`).value
}

function evaluateName({ value }: NameNode, contextStack: Context[]): unknown {
  for (const context of contextStack) {
    const variable = context.variables[value]
    if (variable) {
      return variable.value
    }
  }
  throw Error(`Undefined identifier ${value}`)
}

function evaluateNormalExpression(node: NormalExpressionNode, contextStack: Context[]): unknown {
  const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))

  let lispishFunction: LispishFunction | undefined = undefined
  for (const context of contextStack) {
    lispishFunction = context.functions[node.name]?.fun
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
  if (isUserDefinedLispishFunction(lispishFunction)) {
    const newContext: Context = lispishFunction.functionContext
    const args = lispishFunction.arguments
    const nbrOfMandatoryArgs: number = args.mandatoryArguments.length
    const nbrOfOptionalArgs: number = args.optionalArguments.length
    const maxNbrOfParameters: null | number = args.restArgument ? null : nbrOfMandatoryArgs + nbrOfOptionalArgs

    if (params.length < args.mandatoryArguments.length) {
      throw Error(
        `Function "${lispishFunction.name}" requires at least ${args.mandatoryArguments.length} arguments. Got ${params.length}`,
      )
    }

    if (maxNbrOfParameters !== null && params.length > maxNbrOfParameters) {
      throw Error(
        `Function "${lispishFunction.name}" requires at most ${maxNbrOfParameters} arguments. Got ${params.length}`,
      )
    }

    const length = Math.max(params.length, args.mandatoryArguments.length + args.optionalArguments.length)
    const rest: unknown[] = []
    for (let i = 0; i < length; i += 1) {
      if (i < nbrOfMandatoryArgs) {
        const param = params[i]
        const key = asNotUndefined(args.mandatoryArguments[i], ``)
        if (isLispishFunction(param)) {
          newContext.functions[key] = { fun: param, constant: false }
        } else {
          newContext.variables[key] = { value: param, constant: false }
        }
      } else if (i < nbrOfMandatoryArgs + nbrOfOptionalArgs) {
        const arg = asNotUndefined(args.optionalArguments[i - nbrOfMandatoryArgs], ``)
        const param = i < params.length ? params[i] : arg.defaultValue !== undefined ? arg.defaultValue : undefined
        const key = arg.name
        if (isLispishFunction(param)) {
          newContext.functions[key] = { fun: param, constant: false }
        } else {
          newContext.variables[key] = { value: param, constant: false }
        }
      } else {
        const param = params[i]
        if (isLispishFunction(param)) {
          throw Error(`A function cannot be a &rest parameter`) //  TODO, is this a fact?
        }
        rest.push(param)
      }
    }

    if (args.restArgument) {
      newContext.variables[args.restArgument] = { value: rest, constant: false }
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
    const normalExpression = asNotUndefined(
      normalExpressions[lispishFunction.builtin],
      `${lispishFunction.builtin} is not a function`,
    )
    return normalExpression.evaluate(params, contextStack, { evaluateLispishFunction })
  }
}

function evaluateBuiltinNormalExpression(
  node: NormalExpressionNode,
  params: unknown[],
  contextStack: Context[],
): unknown {
  const normalExpressionEvaluator = asNotUndefined(
    builtin.normalExpressions[node.name],
    `${node.name} is not a function`,
  ).evaluate

  return normalExpressionEvaluator(params, contextStack, { evaluateLispishFunction })
}

function evaluateSpecialExpression(node: SpecialExpressionNode, contextStack: Context[]): unknown {
  const specialExpressionEvaluator = asNotUndefined(
    builtin.specialExpressions[node.name],
    `${node.name} is not a built in special expression`,
  ).evaluate
  return specialExpressionEvaluator(node, contextStack, evaluateAstNode)
}

function evaluateExpressionExpression(node: ExpressionExpressionNode, contextStack: Context[]): unknown {
  const lispishFunction = asLispishFunction(evaluateAstNode(node.expression, contextStack))

  const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))

  return evaluateLispishFunction(lispishFunction, params, contextStack)
}
