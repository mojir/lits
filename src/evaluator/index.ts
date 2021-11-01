import {
  NormalExpressionNode,
  SpecialExpressionNode,
  NameNode,
  NumberNode,
  StringNode,
  ReservedNameNode,
  FUNCTION_SYMBOL,
  NormalExpressionNodeName,
  BuiltinFunction,
} from '../parser/interface'
import { Ast } from '../parser/interface'
import { builtin } from '../builtin'
import { reservedNamesRecord } from '../reservedNames'
import {
  asNotUndefined,
  assertNonNegativeInteger,
  assertString,
  isNormalExpressionNodeName,
  isString,
  toAny,
} from '../utils'
import { Context, EvaluateAstNode, ExecuteFunction } from './interface'
import { Any, Arr, Obj } from '../interface'
import { ContextStack } from './interface'
import { functionExecutors } from './functionExecutors'
import { TokenMeta } from '../tokenizer/interface'
import { LitsError, UndefinedSymbolError } from '../errors'
import { litsFunction, number, object, sequence } from '../utils/assertion'

export function createContextStack(contexts: Context[] = []): ContextStack {
  if (contexts.length === 0) {
    contexts.push({})
  }

  return new ContextStackImpl(contexts, 0)
}

class ContextStackImpl implements ContextStack {
  public stack: Context[]
  public globalContext: Context
  public numberOfImportedContexts: number
  constructor(contexts: Context[], globalContextIndex: number) {
    this.stack = contexts
    this.numberOfImportedContexts = contexts.length - (globalContextIndex + 1)
    this.globalContext = contexts[globalContextIndex] as Context
  }

  public withContext(context: Context): ContextStack {
    return new ContextStackImpl([context, ...this.stack], this.stack.length - this.numberOfImportedContexts)
  }
}

export function evaluate(ast: Ast, contextStack: ContextStack): Any {
  let result: Any = null
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
      throw new LitsError(`${node.type}-node cannot be evaluated`, node.token.meta)
  }
}

function evaluateNumber(node: NumberNode): number {
  return node.value
}

function evaluateString(node: StringNode): string {
  return node.value
}

function evaluateReservedName(node: ReservedNameNode): Any {
  return asNotUndefined(reservedNamesRecord[node.value], node.token.meta).value
}

function evaluateName(node: NameNode, contextStack: ContextStack): Any {
  const {
    value,
    token: { meta },
  } = node
  for (const context of contextStack.stack) {
    const variable = context[value]
    if (variable) {
      return variable.value
    }
  }
  if (builtin.normalExpressions[value]) {
    const builtinFunction: BuiltinFunction = {
      [FUNCTION_SYMBOL]: true,
      type: `builtin`,
      name: value,
    }
    return builtinFunction
  }

  throw new UndefinedSymbolError(value, meta)
}

function evaluateNormalExpression(node: NormalExpressionNode, contextStack: ContextStack): Any {
  const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))
  if (isNormalExpressionNodeName(node)) {
    for (const context of contextStack.stack) {
      const fn = context[node.name]?.value
      if (fn === undefined) {
        continue
      }
      try {
        return executeFunction(fn, params, node.token.meta, contextStack)
      } catch {
        continue
      }
    }

    return evaluateBuiltinNormalExpression(node, params, contextStack)
  } else {
    const fn = evaluateAstNode(node.expression, contextStack)
    return executeFunction(fn, params, node.token.meta, contextStack)
  }
}

export const executeFunction: ExecuteFunction = (fn, params, meta, contextStack) => {
  if (litsFunction.is(fn)) {
    return functionExecutors[fn.type](fn, params, meta, contextStack, { evaluateAstNode, executeFunction })
  }
  if (Array.isArray(fn)) {
    return evaluateArrayAsFunction(fn, params, meta)
  }
  if (object.is(fn)) {
    return evalueateObjectAsFunction(fn, params, meta)
  }
  if (isString(fn)) {
    return evaluateStringAsFunction(fn, params, meta)
  }
  if (number.is(fn)) {
    return evaluateNumberAsFunction(fn, params, meta)
  }
  throw new LitsError(`Expected function, got ${fn}`, meta)
}

function evaluateBuiltinNormalExpression(node: NormalExpressionNodeName, params: Arr, contextStack: ContextStack): Any {
  const normalExpression = builtin.normalExpressions[node.name]
  if (!normalExpression) {
    throw new UndefinedSymbolError(node.name, node.token.meta)
  }

  return normalExpression.evaluate(params, node.token.meta, contextStack, { executeFunction })
}

function evaluateSpecialExpression(node: SpecialExpressionNode, contextStack: ContextStack): Any {
  const specialExpression = asNotUndefined(builtin.specialExpressions[node.name], node.token.meta)

  return specialExpression.evaluate(node, contextStack, { evaluateAstNode, builtin })
}

function evalueateObjectAsFunction(fn: Obj, params: Arr, meta: TokenMeta): Any {
  if (params.length !== 1) {
    throw new LitsError(`Object as function requires one string parameter`, meta)
  }
  const key = params[0]
  assertString(key, meta)
  return toAny(fn[key])
}

function evaluateArrayAsFunction(fn: Arr, params: Arr, meta: TokenMeta): Any {
  if (params.length !== 1) {
    throw new LitsError(`Array as function requires one non negative integer parameter`, meta)
  }
  const index = params[0]
  assertNonNegativeInteger(index, meta)
  return toAny(fn[index])
}

function evaluateStringAsFunction(fn: string, params: Arr, meta: TokenMeta): Any {
  if (params.length !== 1) {
    throw new LitsError(`String as function requires one Obj parameter`, meta)
  }
  const param = toAny(params[0])
  if (object.is(param)) {
    return toAny((param as Obj)[fn])
  }
  if (number.is(param, { integer: true })) {
    return toAny(fn[param])
  }
  throw new LitsError(`string as function expects Obj or integer parameter, got ${param}`, meta)
}

function evaluateNumberAsFunction(fn: number, params: Arr, meta: TokenMeta): Any {
  number.assert(fn, meta, { integer: true })
  if (params.length !== 1) {
    throw new LitsError(`String as function requires one Arr parameter`, meta)
  }
  const param = params[0]
  sequence.assert(param, meta)
  return toAny(param[fn])
}
