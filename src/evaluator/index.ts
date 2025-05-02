import type { SpecialExpression } from '../builtin'
import { builtin } from '../builtin'
import { NodeTypes, getNodeTypeName } from '../constants/constants'
import { LitsError, UndefinedSymbolError } from '../errors'
import { getUndefinedSymbols } from '../getUndefinedSymbols'
import type { Any, Arr, Obj } from '../interface'
import type {
  Ast,
  FunctionLike,
  Node,
  NormalExpressionNode,
  NumberNode,
  PartialFunction,
  ReservedSymbolNode,
  SpecialExpressionNode,
  StringNode,
  SymbolNode,
} from '../parser/types'
import { reservedSymbolRecord } from '../tokenizer/reservedNames'
import type { SourceCodeInfo } from '../tokenizer/token'
import { asNonUndefined } from '../typeGuards'
import { annotate } from '../typeGuards/annotatedArrays'
import { isNormalBuiltinSymbolNode, isNormalExpressionNodeWithName, isSpreadNode } from '../typeGuards/astNode'
import { asFunctionLike, assertSeq, isObj } from '../typeGuards/lits'
import { isLitsFunction } from '../typeGuards/litsFunction'
import { assertNumber, isNumber } from '../typeGuards/number'
import { assertString } from '../typeGuards/string'
import { toAny } from '../utils'
import { valueToString } from '../utils/debug/debugTools'
import { FUNCTION_SYMBOL } from '../utils/symbols'
import type { ContextStack } from './ContextStack'
import { functionExecutors } from './functionExecutors'

export function evaluate(ast: Ast, contextStack: ContextStack): Any {
  let result: Any = null

  for (const node of ast.body) {
    result = evaluateNode(node, contextStack)
  }

  return result
}

export function evaluateNode(node: Node, contextStack: ContextStack): Any {
  switch (node[0]) {
    case NodeTypes.Number:
      return evaluateNumber(node as NumberNode)
    case NodeTypes.String:
      return evaluateString(node as StringNode)
    case NodeTypes.NormalBuiltinSymbol:
    case NodeTypes.SpecialBuiltinSymbol:
    case NodeTypes.UserDefinedSymbol:
      return contextStack.evaluateSymbol(node as SymbolNode)
    case NodeTypes.ReservedSymbol:
      return evaluateReservedSymbol(node as ReservedSymbolNode)
    case NodeTypes.NormalExpression: {
      const result = evaluateNormalExpression(node as NormalExpressionNode, contextStack)
      if (typeof result === 'number' && Number.isNaN(result)) {
        throw new LitsError('Number is NaN', node[2])
      }
      return annotate(result)
    }
    case NodeTypes.SpecialExpression:
      return annotate(evaluateSpecialExpression(node as SpecialExpressionNode, contextStack))
    /* v8 ignore next 2 */
    default:
      throw new LitsError(`${getNodeTypeName(node[0])}-node cannot be evaluated`, node[2])
  }
}

function evaluateNumber(node: NumberNode): number {
  return node[1]
}

function evaluateString(node: StringNode): string {
  return node[1]
}

function evaluateReservedSymbol(node: ReservedSymbolNode): Any {
  const reservedName = node[1]
  if (!['true', 'false', 'null'].includes(reservedName)) {
    throw new LitsError(`Reserved symbol ${reservedName} cannot be evaluated`, node[2])
  }
  const value = reservedSymbolRecord[reservedName]
  return asNonUndefined(value, node[2])
}

function evaluateNormalExpression(node: NormalExpressionNode, contextStack: ContextStack): Any {
  const sourceCodeInfo = node[2]
  const paramNodes: Node[] = node[1][1]
  const params: Arr = []
  const placeholders: number[] = []
  paramNodes.forEach((paramNode, index) => {
    if (isSpreadNode(paramNode)) {
      const spreadValue = evaluateNode(paramNode[1], contextStack)
      if (Array.isArray(spreadValue)) {
        params.push(...spreadValue)
      }
      else {
        throw new LitsError(`Spread operator requires an array, got ${valueToString(paramNode)}`, paramNode[2])
      }
    }
    else if (paramNode[0] === NodeTypes.ReservedSymbol && paramNode[1] === '_') {
      placeholders.push(index)
    }
    else {
      params.push(evaluateNode(paramNode, contextStack))
    }
  })
  if (isNormalExpressionNodeWithName(node)) {
    const nameSymbol = node[1][0]
    if (placeholders.length > 0) {
      const fn = evaluateNode(nameSymbol, contextStack)
      const partialFunction: PartialFunction = {
        [FUNCTION_SYMBOL]: true,
        function: asFunctionLike(fn, sourceCodeInfo),
        functionType: 'Partial',
        params,
        placeholders,
        sourceCodeInfo,
      }
      return partialFunction
    }

    if (isNormalBuiltinSymbolNode(nameSymbol)) {
      const type = nameSymbol[1]
      const normalExpression = builtin.allNormalExpressions[type]!
      return normalExpression.evaluate(params, node[2], contextStack, { executeFunction })
    }
    else {
      const fn = contextStack.getValue(nameSymbol[1])
      if (fn !== undefined) {
        return executeFunction(asFunctionLike(fn, sourceCodeInfo), params, contextStack, sourceCodeInfo)
      }
      throw new UndefinedSymbolError(nameSymbol[1], node[2])
    }
  }
  else {
    const fnNode: Node = node[1][0]
    const fn = asFunctionLike(evaluateNode(fnNode, contextStack), sourceCodeInfo)
    if (placeholders.length > 0) {
      const partialFunction: PartialFunction = {
        [FUNCTION_SYMBOL]: true,
        function: asFunctionLike(fn, sourceCodeInfo),
        functionType: 'Partial',
        params,
        placeholders,
        sourceCodeInfo,
      }
      return partialFunction
    }
    return executeFunction(fn, params, contextStack, sourceCodeInfo)
  }
}

function executeFunction(fn: FunctionLike, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): Any {
  if (isLitsFunction(fn))
    return functionExecutors[fn.functionType](fn, params, sourceCodeInfo, contextStack, { evaluateNode, executeFunction })

  if (Array.isArray(fn))
    return evaluateArrayAsFunction(fn, params, sourceCodeInfo)

  if (isObj(fn))
    return evalueateObjectAsFunction(fn, params, sourceCodeInfo)

  if (typeof fn === 'string')
    return evaluateStringAsFunction(fn, params, sourceCodeInfo)

  if (isNumber(fn)) {
    return evaluateNumberAsFunction(fn, params, sourceCodeInfo)
    /* v8 ignore next 4 */
  }
  throw new LitsError('Unexpected function type', sourceCodeInfo)
}

function evaluateSpecialExpression(node: SpecialExpressionNode, contextStack: ContextStack): Any {
  const specialExpressionType = node[1][0]
  const specialExpression: SpecialExpression = asNonUndefined(builtin.specialExpressions[specialExpressionType], node[2])
  const castedEvaluate = specialExpression.evaluate as Function

  return castedEvaluate(node, contextStack, { evaluateNode, builtin, getUndefinedSymbols }) as Any
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
