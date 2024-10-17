import type {
  Ast,
  AstNode,
  NormalExpressionNode,
  NormalExpressionNodeWithName,
  NumberNode,
  ReservedNameNode,
  SpecialExpressionNode,
  StringNode,
} from '../parser/interface'
import { builtin } from '../builtin'
import { reservedNamesRecord } from '../reservedNames'
import { toAny } from '../utils'
import type { Any, Arr, Obj } from '../interface'
import type { SourceCodeInfo } from '../tokenizer/interface'
import { LitsError, NotAFunctionError, UndefinedSymbolError } from '../errors'
import { AstNodeType } from '../constants/constants'
import { isNormalExpressionNodeWithName } from '../typeGuards/astNode'
import { valueToString } from '../utils/debug/debugTools'
import { isLitsFunction } from '../typeGuards/litsFunction'
import { assertNumber, isNumber } from '../typeGuards/number'
import { asNonUndefined } from '../typeGuards'
import { asAny, assertSeq, isObj } from '../typeGuards/lits'
import { assertString } from '../typeGuards/string'
import type { ContextStack } from './ContextStack'
import { functionExecutors } from './functionExecutors'

export function evaluate(ast: Ast, contextStack: ContextStack): Any {
  let result: Any = null
  for (const node of ast.b)
    result = evaluateAstNode(node, contextStack)

  return result
}

export function evaluateAstNode(node: AstNode, contextStack: ContextStack): Any {
  switch (node.t) {
    case AstNodeType.Number:
      return evaluateNumber(node)
    case AstNodeType.String:
      return evaluateString(node)
    case AstNodeType.Name:
      return contextStack.evaluateName(node)
    case AstNodeType.ReservedName:
      return evaluateReservedName(node)
    case AstNodeType.NormalExpression:
      return evaluateNormalExpression(node, contextStack)
    case AstNodeType.SpecialExpression:
      return evaluateSpecialExpression(node, contextStack)
    default:
      throw new LitsError(`${node.t}-node cannot be evaluated`, node.tkn?.sourceCodeInfo)
  }
}

function evaluateNumber(node: NumberNode): number {
  return node.v
}

function evaluateString(node: StringNode): string {
  return node.v
}

function evaluateReservedName(node: ReservedNameNode): Any {
  return asNonUndefined(reservedNamesRecord[node.v], node.tkn?.sourceCodeInfo).value
}

function evaluateNormalExpression(node: NormalExpressionNode, contextStack: ContextStack): Any {
  const params = node.p.map(paramNode => evaluateAstNode(paramNode, contextStack))
  const sourceCodeInfo = node.tkn?.sourceCodeInfo
  if (isNormalExpressionNodeWithName(node)) {
    const value = contextStack.getValue(node.n)
    if (value !== undefined)
      return executeFunction(asAny(value), params, contextStack, sourceCodeInfo)

    return evaluateBuiltinNormalExpression(node, params, contextStack)
  }
  else {
    const fn = evaluateAstNode(node.e, contextStack)
    return executeFunction(fn, params, contextStack, sourceCodeInfo)
  }
}

function executeFunction(fn: Any, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): Any {
  if (isLitsFunction(fn))
    return functionExecutors[fn.t](fn, params, sourceCodeInfo, contextStack, { evaluateAstNode, executeFunction })

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
  const normalExpression = builtin.normalExpressions[node.n]
  if (!normalExpression)
    throw new UndefinedSymbolError(node.n, node.tkn?.sourceCodeInfo)

  return normalExpression.evaluate(params, node.tkn?.sourceCodeInfo, contextStack, { executeFunction })
}

function evaluateSpecialExpression(node: SpecialExpressionNode, contextStack: ContextStack): Any {
  const specialExpression = asNonUndefined(builtin.specialExpressions[node.n], node.tkn?.sourceCodeInfo)

  return specialExpression.evaluate(node, contextStack, { evaluateAstNode, builtin })
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
