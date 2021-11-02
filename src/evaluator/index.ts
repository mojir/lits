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
import { SourceCodeInfo } from '../tokenizer/interface'
import { LitsError, NotAFunctionError, UndefinedSymbolError } from '../errors'
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
      throw new LitsError(`${node.type}-node cannot be evaluated`, node.token.sourceCodeInfo)
  }
}

function evaluateNumber(node: NumberNode): number {
  return node.value
}

function evaluateString(node: StringNode): string {
  return node.value
}

function evaluateReservedName(node: ReservedNameNode): Any {
  return asNotUndefined(reservedNamesRecord[node.value], node.token.sourceCodeInfo).value
}

function evaluateName(node: NameNode, contextStack: ContextStack): Any {
  const {
    value,
    token: { sourceCodeInfo },
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

  throw new UndefinedSymbolError(value, sourceCodeInfo)
}

function evaluateNormalExpression(node: NormalExpressionNode, contextStack: ContextStack): Any {
  const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))
  const { sourceCodeInfo } = node.token
  if (isNormalExpressionNodeName(node)) {
    for (const context of contextStack.stack) {
      const fn = context[node.name]?.value
      if (fn === undefined) {
        continue
      }
      const result = executeFunctionOrUndefined(fn, params, sourceCodeInfo, contextStack)
      if (result === undefined) {
        continue
      }
      return result.value
    }

    return evaluateBuiltinNormalExpression(node, params, contextStack)
  } else {
    const fn = evaluateAstNode(node.expression, contextStack)
    return executeFunction(fn, params, sourceCodeInfo, contextStack)
  }
}

const executeFunction: ExecuteFunction = (fn, params, sourceCodeInfo, contextStack) => {
  const result = executeFunctionOrUndefined(fn, params, sourceCodeInfo, contextStack)
  if (!result) {
    throw new NotAFunctionError(fn, sourceCodeInfo)
  }
  return result.value
}

function executeFunctionOrUndefined(fn: Any, params: Arr, sourceCodeInfo: SourceCodeInfo, contextStack: ContextStack) {
  if (litsFunction.is(fn)) {
    return {
      value: functionExecutors[fn.type](fn, params, sourceCodeInfo, contextStack, { evaluateAstNode, executeFunction }),
    }
  }
  if (Array.isArray(fn)) {
    return { value: evaluateArrayAsFunction(fn, params, sourceCodeInfo) }
  }
  if (object.is(fn)) {
    return { value: evalueateObjectAsFunction(fn, params, sourceCodeInfo) }
  }
  if (isString(fn)) {
    return { value: evaluateStringAsFunction(fn, params, sourceCodeInfo) }
  }
  if (number.is(fn)) {
    return { value: evaluateNumberAsFunction(fn, params, sourceCodeInfo) }
  }
  return undefined
}

function evaluateBuiltinNormalExpression(node: NormalExpressionNodeName, params: Arr, contextStack: ContextStack): Any {
  const normalExpression = builtin.normalExpressions[node.name]
  if (!normalExpression) {
    throw new UndefinedSymbolError(node.name, node.token.sourceCodeInfo)
  }

  return normalExpression.evaluate(params, node.token.sourceCodeInfo, contextStack, { executeFunction })
}

function evaluateSpecialExpression(node: SpecialExpressionNode, contextStack: ContextStack): Any {
  const specialExpression = asNotUndefined(builtin.specialExpressions[node.name], node.token.sourceCodeInfo)

  return specialExpression.evaluate(node, contextStack, { evaluateAstNode, builtin })
}

function evalueateObjectAsFunction(fn: Obj, params: Arr, sourceCodeInfo: SourceCodeInfo): Any {
  if (params.length !== 1) {
    throw new LitsError(`Object as function requires one string parameter`, sourceCodeInfo)
  }
  const key = params[0]
  assertString(key, sourceCodeInfo)
  return toAny(fn[key])
}

function evaluateArrayAsFunction(fn: Arr, params: Arr, sourceCodeInfo: SourceCodeInfo): Any {
  if (params.length !== 1) {
    throw new LitsError(`Array as function requires one non negative integer parameter`, sourceCodeInfo)
  }
  const index = params[0]
  assertNonNegativeInteger(index, sourceCodeInfo)
  return toAny(fn[index])
}

function evaluateStringAsFunction(fn: string, params: Arr, sourceCodeInfo: SourceCodeInfo): Any {
  if (params.length !== 1) {
    throw new LitsError(`String as function requires one Obj parameter`, sourceCodeInfo)
  }
  const param = toAny(params[0])
  if (object.is(param)) {
    return toAny((param as Obj)[fn])
  }
  if (number.is(param, { integer: true })) {
    return toAny(fn[param])
  }
  throw new LitsError(`string as function expects Obj or integer parameter, got ${param}`, sourceCodeInfo)
}

function evaluateNumberAsFunction(fn: number, params: Arr, sourceCodeInfo: SourceCodeInfo): Any {
  number.assert(fn, sourceCodeInfo, { integer: true })
  if (params.length !== 1) {
    throw new LitsError(`String as function requires one Arr parameter`, sourceCodeInfo)
  }
  const param = params[0]
  sequence.assert(param, sourceCodeInfo)
  return toAny(param[fn])
}
