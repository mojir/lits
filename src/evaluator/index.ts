import {
  NormalExpressionNode,
  SpecialExpressionNode,
  NameNode,
  NumberNode,
  StringNode,
  ReservedNameNode,
  LispishFunction,
  functionSymbol,
  NormalExpressionNodeName,
  BuiltinLispishFunction,
} from '../parser/interface'
import { Ast } from '../parser/interface'
import { builtin } from '../builtin'
import { reservedNamesRecord } from '../reservedNames'
import {
  asNotUndefined,
  assertInteger,
  assertNonNegativeInteger,
  assertSeq,
  assertString,
  isInteger,
  isLispishFunction,
  isNormalExpressionNodeName,
  isNumber,
  isObj,
  isString,
} from '../utils'
import { Context, EvaluateAstNode, ExecuteFunction, ExecuteLispishFunction } from './interface'
import { normalExpressions } from '../builtin/normalExpressions'
import { RecurSignal } from '../errors'
import { Arr, Obj } from '../interface'

export function evaluate(ast: Ast, globalScope: Context, importScope: Context): unknown {
  // First element is the global context. E.g. def will assign to this if no local variable is available
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
    const variable = context[value]
    if (variable) {
      return variable.value
    }
  }
  if (builtin.normalExpressions[value]) {
    const builtinFunction: BuiltinLispishFunction = {
      [functionSymbol]: true,
      type: `builtin`,
      builtin: value,
    }
    return builtinFunction
  }
  throw Error(`Undefined identifier ${value}`)
}

function evaluateNormalExpression(node: NormalExpressionNode, contextStack: Context[]): unknown {
  const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))
  if (isNormalExpressionNodeName(node)) {
    for (const context of contextStack) {
      const fn = context[node.name]?.value
      try {
        return executeFunction(fn, params, contextStack)
      } catch {
        continue
      }
    }

    return evaluateBuiltinNormalExpression(node, params, contextStack)
  } else {
    const fn = evaluateAstNode(node.expression, contextStack)
    return executeFunction(fn, params, contextStack)
  }
}

export const executeFunction: ExecuteFunction = (fn, params, contextStack) => {
  if (isLispishFunction(fn)) {
    return executeLispishFunction(fn, params, contextStack)
  }
  if (Array.isArray(fn)) {
    return evaluateArrayAsFunction(fn, params)
  }
  if (isObj(fn)) {
    return evalueateObjectAsFunction(fn, params)
  }
  if (isString(fn)) {
    return evaluateStringAsFunction(fn, params)
  }
  if (isNumber(fn)) {
    return evaluateNumberAsFunction(fn, params)
  }
  throw Error(`Expected function, got ${fn}`)
}

const executeLispishFunction: ExecuteLispishFunction = (
  lispishFunction: LispishFunction,
  params: Arr,
  contextStack: Context[],
) => {
  switch (lispishFunction.type) {
    case `user-defined`: {
      const args = lispishFunction.arguments
      const nbrOfMandatoryArgs: number = args.mandatoryArguments.length
      const nbrOfOptionalArgs: number = args.optionalArguments.length
      const maxNbrOfParameters: null | number = args.restArgument ? null : nbrOfMandatoryArgs + nbrOfOptionalArgs

      for (;;) {
        const newContext: Context = { ...lispishFunction.functionContext }
        if (params.length < args.mandatoryArguments.length) {
          throw Error(
            `Function ${lispishFunction.name ?? `(fn)`} requires at least ${
              args.mandatoryArguments.length
            } arguments. Got ${params.length}`,
          )
        }

        if (maxNbrOfParameters !== null && params.length > maxNbrOfParameters) {
          throw Error(
            `Function "${lispishFunction.name ?? `λ`}" requires at most ${maxNbrOfParameters} arguments. Got ${
              params.length
            }`,
          )
        }

        const length = Math.max(params.length, args.mandatoryArguments.length + args.optionalArguments.length)
        const rest: Arr = []
        for (let i = 0; i < length; i += 1) {
          if (i < nbrOfMandatoryArgs) {
            const param = params[i]
            const key = asNotUndefined(args.mandatoryArguments[i], ``)
            newContext[key] = { value: param }
          } else if (i < nbrOfMandatoryArgs + nbrOfOptionalArgs) {
            const arg = asNotUndefined(args.optionalArguments[i - nbrOfMandatoryArgs], ``)
            const param = i < params.length ? params[i] : arg.defaultValue !== undefined ? arg.defaultValue : undefined
            const key = arg.name
            newContext[key] = { value: param }
          } else {
            rest.push(params[i])
          }
        }

        if (args.restArgument) {
          newContext[args.restArgument] = { value: rest }
        }

        try {
          let result: unknown = undefined
          for (const node of lispishFunction.body) {
            result = evaluateAstNode(node, [newContext, ...contextStack])
          }
          return result
        } catch (error) {
          if (error instanceof RecurSignal) {
            params = error.params
            continue
          }
          throw error
        }
      }
    }
    case `partial`: {
      return executeFunction(lispishFunction.fn, [...lispishFunction.params, ...params], contextStack)
    }
    case `comp`: {
      const { fns } = lispishFunction
      if (fns.length === 0) {
        if (params.length !== 1) {
          throw Error(`(comp) expects one argument, got ${params.length}`)
        }
        return params[0]
      }
      return fns.reduceRight((result: unknown[], fn) => {
        return [executeFunction(fn, result, contextStack)]
      }, params)[0]
    }
    case `constantly`: {
      return lispishFunction.value
    }
    case `juxt`: {
      return lispishFunction.fns.map(fn => executeFunction(fn, params, contextStack))
    }
    case `complement`: {
      return !executeFunction(lispishFunction.fn, params, contextStack)
    }
    case `every-pred`: {
      for (const fn of lispishFunction.fns) {
        for (const param of params) {
          const result = executeFunction(fn, [param], contextStack)
          if (!result) {
            return false
          }
        }
      }
      return true
    }
    case `some-pred`: {
      for (const fn of lispishFunction.fns) {
        for (const param of params) {
          const result = executeFunction(fn, [param], contextStack)
          if (result) {
            return true
          }
        }
      }
      return false
    }
    default: {
      const normalExpression = asNotUndefined(
        normalExpressions[lispishFunction.builtin],
        `${lispishFunction.builtin} is not a function`,
      )
      return normalExpression.evaluate(params, contextStack, { evaluateFunction: executeFunction })
    }
  }
}

function evaluateBuiltinNormalExpression(
  node: NormalExpressionNodeName,
  params: Arr,
  contextStack: Context[],
): unknown {
  const normalExpressionEvaluator = asNotUndefined(
    builtin.normalExpressions[node.name],
    `${node.name} is not a function`,
  ).evaluate

  return normalExpressionEvaluator(params, contextStack, { evaluateFunction: executeFunction })
}

function evaluateSpecialExpression(node: SpecialExpressionNode, contextStack: Context[]): unknown {
  const specialExpressionEvaluator = asNotUndefined(
    builtin.specialExpressions[node.name],
    `${node.name} is not a built in special expression`,
  ).evaluate
  return specialExpressionEvaluator(node, contextStack, { evaluateAstNode, builtin })
}

function evalueateObjectAsFunction(fn: Obj, params: unknown[]) {
  if (params.length !== 1) {
    throw Error(`Object as function requires one string parameter`)
  }
  const key = params[0]
  assertString(key)
  return fn[key]
}

function evaluateArrayAsFunction(fn: Arr, params: unknown[]) {
  if (params.length !== 1) {
    throw Error(`Array as function requires one non negative integer parameter`)
  }
  const index = params[0]
  assertNonNegativeInteger(index)
  return fn[index]
}

function evaluateStringAsFunction(fn: string, params: unknown[]) {
  if (params.length !== 1) {
    throw Error(`String as function requires one Obj parameter`)
  }
  const param = params[0]
  if (isObj(param)) {
    return param[fn]
  }
  if (isInteger(param)) {
    return fn[param]
  }
  throw Error(`string as function expects Obj or integer parameter, got ${param}`)
}

function evaluateNumberAsFunction(fn: number, params: unknown[]) {
  assertInteger(fn)
  if (params.length !== 1) {
    throw Error(`String as function requires one Arr parameter`)
  }
  const param = params[0]
  assertSeq(param)
  return param[fn]
}
