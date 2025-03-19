import type {
  Ast,
  AstNode,
  NormalExpressionNode,
  NormalExpressionNodeWithName,
  NumberNode,
  ReservedSymbolNode,
  StringNode,
} from '../parser/types'
import type { SpecialExpressionNode } from '../builtin'
import { builtin } from '../builtin'
import { toAny } from '../utils'
import type { Any, Arr, Obj } from '../interface'
import type { SourceCodeInfo } from '../tokenizer/token'
import { LitsError, NotAFunctionError, UndefinedSymbolError } from '../errors'
import { isNormalExpressionNodeWithName } from '../typeGuards/astNode'
import { valueToString } from '../utils/debug/debugTools'
import { isLitsFunction } from '../typeGuards/litsFunction'
import { assertNumber, isNumber } from '../typeGuards/number'
import { asNonUndefined } from '../typeGuards'
import { asAny, assertSeq, isObj } from '../typeGuards/lits'
import { assertString } from '../typeGuards/string'
import type { ReservedSymbol } from '../tokenizer/reservedNames'
import { reservedSymbolRecord } from '../tokenizer/reservedNames'
import { getUndefinedSymbols } from '../getUndefinedSymbols'
import type { ContextStack } from './ContextStack'
import { functionExecutors } from './functionExecutors'

export function evaluate(ast: Ast, contextStack: ContextStack): Any {
  let result: Any = null

  for (const node of ast.body) {
    result = evaluateAstNode(node, contextStack)
  }

  return result
}

export function evaluateAstNode(node: AstNode, contextStack: ContextStack): Any {
  switch (node.type) {
    case 'Number':
      return evaluateNumber(node)
    case 'String':
      return evaluateString(node)
    case 'Symbol':
      return contextStack.evaluateName(node)
    case 'ReservedSymbol':
      return evaluateReservedName(node)
    case 'NormalExpression':
      return evaluateNormalExpression(node, contextStack)
    case 'SpecialExpression':
      return evaluateSpecialExpression(node, contextStack)
    /* v8 ignore next 2 */
    default:
      throw new LitsError(`${node.type}-node cannot be evaluated`, node.sourceCodeInfo)
  }
}

function evaluateNumber(node: NumberNode): number {
  return node.value
}

function evaluateString(node: StringNode): string {
  return node.value
}

function evaluateReservedName(node: ReservedSymbolNode): Any {
  const reservedName = node.value as ReservedSymbol
  const value = reservedSymbolRecord[reservedName]
  return asNonUndefined(value, node.sourceCodeInfo)
}

function evaluateNormalExpression(node: NormalExpressionNode, contextStack: ContextStack): Any {
  const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))
  const sourceCodeInfo = node.sourceCodeInfo
  if (isNormalExpressionNodeWithName(node)) {
    const value = contextStack.getValue(node.name)
    if (value !== undefined)
      return executeFunction(asAny(value), params, contextStack, sourceCodeInfo)

    return evaluateBuiltinNormalExpression(node, params, contextStack)
  }
  else {
    const fn = params[0]!
    return executeFunction(fn, params.slice(1), contextStack, sourceCodeInfo)
  }
}

function executeFunction(fn: Any, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): Any {
  if (isLitsFunction(fn))
    return functionExecutors[fn.functionType](fn, params, sourceCodeInfo, contextStack, { evaluateAstNode, executeFunction })

  if (Array.isArray(fn))
    return evaluateArrayAsFunction(fn, params, sourceCodeInfo)

  if (isObj(fn))
    return evalueateObjectAsFunction(fn, params, sourceCodeInfo)

  if (typeof fn === 'string')
    return evaluateStringAsFunction(fn, params, sourceCodeInfo)

  if (isNumber(fn))
    return evaluateNumberAsFunction(fn, params, sourceCodeInfo)

  throw new NotAFunctionError(fn, sourceCodeInfo)
}

function evaluateBuiltinNormalExpression(
  node: NormalExpressionNodeWithName,
  params: Arr,
  contextStack: ContextStack,
): Any {
  const normalExpression = builtin.normalExpressions[node.name]
  if (!normalExpression)
    throw new UndefinedSymbolError(node.name, node.sourceCodeInfo)

  return normalExpression.evaluate(params, node.sourceCodeInfo, contextStack, { executeFunction })
}

function evaluateSpecialExpression(node: SpecialExpressionNode, contextStack: ContextStack): Any {
  const specialExpression = asNonUndefined(builtin.specialExpressions[node.name], node.sourceCodeInfo)

  // eslint-disable-next-line ts/no-unsafe-argument
  return specialExpression.evaluate(node as any, contextStack, { evaluateAstNode, builtin, getUndefinedSymbols })
}

function evalueateObjectAsFunction(fn: Obj, params: Arr, sourceCodeInfo?: SourceCodeInfo): Any {
  if (params.length !== 1)
    throw new LitsError('Object as function requires one string parameter.', sourceCodeInfo)

  const key = params[0]
  assertString(key, sourceCodeInfo)
  return toAny(fn[key])
}

function evaluateArrayAsFunction(fn: Arr, params: Arr, sourceCodeInfo?: SourceCodeInfo): Any {
  if (params.length !== 1)
    throw new LitsError('Array as function requires one non negative integer parameter.', sourceCodeInfo)

  const index = params[0]
  assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true })
  return toAny(fn[index])
}

function evaluateStringAsFunction(fn: string, params: Arr, sourceCodeInfo?: SourceCodeInfo): Any {
  if (params.length !== 1)
    throw new LitsError('String as function requires one Obj parameter.', sourceCodeInfo)

  const param = toAny(params[0])
  if (isObj(param))
    return toAny((param)[fn])

  if (isNumber(param, { integer: true }))
    return toAny(fn[param])

  throw new LitsError(
    `string as function expects Obj or integer parameter, got ${valueToString(param)}`,
    sourceCodeInfo,
  )
}

function evaluateNumberAsFunction(fn: number, params: Arr, sourceCodeInfo?: SourceCodeInfo): Any {
  assertNumber(fn, sourceCodeInfo, { integer: true })
  if (params.length !== 1)
    throw new LitsError('Number as function requires one Arr parameter.', sourceCodeInfo)

  const param = params[0]
  assertSeq(param, sourceCodeInfo)
  return toAny(param[fn])
}
