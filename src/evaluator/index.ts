import {
  NormalExpressionNode,
  SpecialExpressionNode,
  NameNode,
  NumberNode,
  StringNode,
  ReservedNameNode,
  functionSymbol,
  NormalExpressionNodeName,
  BuiltinFunction,
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
  toAny,
} from '../utils'
import { Context, EvaluateAstNode, ExecuteFunction } from './interface'
import { Any, Arr, Obj } from '../interface'
import { functionExecutors } from './functionExecutors'

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
    const builtinFunction: BuiltinFunction = {
      [functionSymbol]: true,
      type: `builtin`,
      name: value,
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
    return functionExecutors[fn.type](fn, params, contextStack, { evaluateAstNode, executeFunction })
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
