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
  asAny,
  asNotUndefined,
  assertInteger,
  assertNonNegativeInteger,
  assertSeq,
  assertString,
  asString,
  isInteger,
  isLispishFunction,
  isNormalExpressionNodeName,
  isNumber,
  isObj,
  isString,
  toAny,
} from '../utils'
import { Context, EvaluateAstNode, ExecuteFunction, ExecuteLispishFunction } from './interface'
import { normalExpressions } from '../builtin/normalExpressions'
import { RecurSignal } from '../errors'
import { Any, Arr, Obj } from '../interface'

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
    default:
      throw Error(`${node.type}-node cannot be evaluated`)
  }
}

function evaluateNumber(node: NumberNode): number {
  return node.value
}

function evaluateString(node: StringNode): string {
  return node.value
}

function evaluateReservedName(node: ReservedNameNode): Any {
  return asNotUndefined(reservedNamesRecord[node.value]).value
}

function evaluateName({ value }: NameNode, contextStack: Context[]): Any {
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

function evaluateNormalExpression(node: NormalExpressionNode, contextStack: Context[]): Any {
  const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))
  if (isNormalExpressionNodeName(node)) {
    for (const context of contextStack) {
      const fn = context[node.name]?.value
      if (fn === undefined) {
        continue
      }
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
            const param = asAny(params[i])
            const key = asString(args.mandatoryArguments[i])
            newContext[key] = { value: param }
          } else if (i < nbrOfMandatoryArgs + nbrOfOptionalArgs) {
            const arg = asNotUndefined(args.optionalArguments[i - nbrOfMandatoryArgs])
            const param = i < params.length ? asAny(params[i]) : arg.defaultValue ?? null
            const key = arg.name
            newContext[key] = { value: param }
          } else {
            rest.push(asAny(params[i]))
          }
        }

        if (args.restArgument) {
          newContext[args.restArgument] = { value: rest }
        }

        try {
          let result: Any = null
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
        return asAny(params[0])
      }
      return asAny(
        fns.reduceRight((result: Arr, fn) => {
          return [executeFunction(toAny(fn), result, contextStack)]
        }, params)[0],
      )
    }
    case `constantly`: {
      return lispishFunction.value
    }
    case `juxt`: {
      return lispishFunction.fns.map(fn => executeFunction(toAny(fn), params, contextStack))
    }
    case `complement`: {
      return !executeFunction(lispishFunction.fn, params, contextStack)
    }
    case `every-pred`: {
      for (const fn of lispishFunction.fns) {
        for (const param of params) {
          const result = executeFunction(toAny(fn), [param], contextStack)
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
          const result = executeFunction(toAny(fn), [param], contextStack)
          if (result) {
            return true
          }
        }
      }
      return false
    }
    default: {
      const normalExpression = asNotUndefined(normalExpressions[lispishFunction.builtin])
      return normalExpression.evaluate(params, contextStack, { executeFunction })
    }
  }
}

function evaluateBuiltinNormalExpression(node: NormalExpressionNodeName, params: Arr, contextStack: Context[]): Any {
  const normalExpressionEvaluator = asNotUndefined(builtin.normalExpressions[node.name]).evaluate

  return normalExpressionEvaluator(params, contextStack, { executeFunction })
}

function evaluateSpecialExpression(node: SpecialExpressionNode, contextStack: Context[]): Any {
  const specialExpressionEvaluator = asNotUndefined(builtin.specialExpressions[node.name]).evaluate
  return specialExpressionEvaluator(node, contextStack, { evaluateAstNode, builtin })
}

function evalueateObjectAsFunction(fn: Obj, params: Arr): Any {
  if (params.length !== 1) {
    throw Error(`Object as function requires one string parameter`)
  }
  const key = params[0]
  assertString(key)
  return toAny(fn[key])
}

function evaluateArrayAsFunction(fn: Arr, params: Arr): Any {
  if (params.length !== 1) {
    throw Error(`Array as function requires one non negative integer parameter`)
  }
  const index = params[0]
  assertNonNegativeInteger(index)
  return toAny(fn[index])
}

function evaluateStringAsFunction(fn: string, params: Arr): Any {
  if (params.length !== 1) {
    throw Error(`String as function requires one Obj parameter`)
  }
  const param = toAny(params[0])
  if (isObj(param)) {
    return toAny((param as Obj)[fn])
  }
  if (isInteger(param)) {
    return toAny(fn[param])
  }
  throw Error(`string as function expects Obj or integer parameter, got ${param}`)
}

function evaluateNumberAsFunction(fn: number, params: Arr): Any {
  assertInteger(fn)
  if (params.length !== 1) {
    throw Error(`String as function requires one Arr parameter`)
  }
  const param = params[0]
  assertSeq(param)
  return toAny(param[fn])
}
