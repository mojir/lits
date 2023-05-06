import {
  NormalExpressionNode,
  NameNode,
  NumberNode,
  StringNode,
  ReservedNameNode,
  NormalExpressionNodeWithName,
  SpecialExpressionNode,
  TypeNameNode,
} from '../parser/interface'
import { Ast } from '../parser/interface'
import { builtin } from '../builtin'
import { reservedNamesRecord } from '../reservedNames'
import { MAX_NUMBER, MIN_NUMBER, toAny } from '../utils'
import { EvaluateAstNode, ExecuteFunction } from './interface'
import { Any, Arr, Obj } from '../interface'
import { functionExecutors } from './functionExecutors'
import { DebugInfo } from '../tokenizer/interface'
import { LitsError, NotAFunctionError, UndefinedSymbolError } from '../errors'
import {
  asNotNull,
  asValue,
  litsFunction,
  normalExpressionNodeWithName,
  number,
  object,
  sequence,
  string,
} from '../utils/assertion'
import { valueToString } from '../utils/helpers'
import { ContextStack } from '../ContextStack'
import { lookUp } from '../lookup'
import { Type } from '../types/Type'
import { ArrayVariant } from '../types/ArrayVariant'

export function evaluate(ast: Ast, contextStack: ContextStack): Any {
  let result: Any = null
  for (const node of ast.body) {
    result = evaluateAstNode(node, contextStack)
  }
  return typeof result === `number` ? toSafeNumber(result) : result
}

function toSafeNumber(value: number): number {
  if (value <= MAX_NUMBER && value >= MIN_NUMBER) {
    return value
  }

  return Math.sign(value) * Infinity
}

export const evaluateAstNode: EvaluateAstNode = (node, contextStack) => {
  switch (node.type) {
    case `Number`:
      return evaluateNumber(node)
    case `String`:
      return evaluateString(node)
    case `TypeName`:
      return evaluateTypeName(node)
    case `Name`:
      return evaluateName(node, contextStack)
    case `ReservedName`:
      return evaluateReservedName(node)
    case `NormalExpression`:
      return evaluateNormalExpression(node, contextStack)
    case `SpecialExpression`:
      return evaluateSpecialExpression(node, contextStack)
    default:
      throw new LitsError(`${node.type}-node cannot be evaluated`, node.token?.debugInfo)
  }
}

function evaluateNumber(node: NumberNode): number {
  return toSafeNumber(node.value)
}

function evaluateString(node: StringNode): string {
  return node.value
}

function evaluateTypeName(node: TypeNameNode): Type {
  switch (node.value) {
    case `never`:
      return Type.never
    case `nil`:
      return Type.nil
    case `nan`:
      return Type.nan
    case `empty-string`:
      return Type.emptyString
    case `non-empty-string`:
      return Type.nonEmptyString
    case `string`:
      return Type.string
    case `number`:
      return Type.number
    case `positive-number`:
      return Type.positiveNumber
    case `negative-number`:
      return Type.negativeNumber
    case `non-zero-number`:
      return Type.nonZeroNumber
    case `non-positive-number`:
      return Type.nonPositiveNumber
    case `non-negative-number`:
      return Type.nonNegativeNumber
    case `float`:
      return Type.float
    case `positive-infinity`:
      return Type.positiveInfinity
    case `negative-infinity`:
      return Type.negativeInfinity
    case `infinity`:
      return Type.infinity
    case `positive-zero`:
      return Type.positiveZero
    case `negative-zero`:
      return Type.negativeZero
    case `zero`:
      return Type.zero
    case `non-zero-float`:
      return Type.nonZeroFloat
    case `positive-float`:
      return Type.positiveFloat
    case `negative-float`:
      return Type.negativeFloat
    case `non-positive-float`:
      return Type.nonPositiveFloat
    case `non-negative-float`:
      return Type.nonNegativeFloat
    case `integer`:
      return Type.integer
    case `non-zero-integer`:
      return Type.nonZeroInteger
    case `positive-integer`:
      return Type.positiveInteger
    case `negative-integer`:
      return Type.negativeInteger
    case `non-positive-integer`:
      return Type.nonPositiveInteger
    case `non-negative-integer`:
      return Type.nonNegativeInteger
    case `true`:
      return Type.true
    case `false`:
      return Type.false
    case `boolean`:
      return Type.boolean
    case `empty-array`:
      return Type.emptyArray
    case `non-empty-array`:
      return Type.nonEmptyArray
    case `array`:
      return Type.array
    case `empty-object`:
      return Type.emptyObject
    case `non-empty-object`:
      return Type.nonEmptyObject
    case `object`:
      return Type.object
    case `regexp`:
      return Type.regexp
    case `function`:
      return Type.function
    case `unknown`:
      return Type.unknown
    case `truthy`:
      return Type.truthy
    case `falsy`:
      return Type.falsy
  }
}

function evaluateReservedName(node: ReservedNameNode): Any {
  return asValue(reservedNamesRecord[node.value], node.token?.debugInfo).value
}

function evaluateName(node: NameNode, contextStack: ContextStack): Any {
  const lookUpResult = lookUp(node, contextStack)
  if (lookUpResult.contextEntry) {
    return lookUpResult.contextEntry.value
  } else if (lookUpResult.builtinFunction) {
    return lookUpResult.builtinFunction
  }
  throw new UndefinedSymbolError(node.value, node.token?.debugInfo)
}

function evaluateNormalExpression(node: NormalExpressionNode, contextStack: ContextStack): Any {
  const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))
  const debugInfo = node.token?.debugInfo
  if (normalExpressionNodeWithName.is(node)) {
    for (const context of contextStack.stack) {
      const fn = context[node.name]?.value
      if (fn === undefined) {
        continue
      }
      return executeFunction(fn, params, contextStack, debugInfo)
    }

    return evaluateBuiltinNormalExpression(node, params, contextStack)
  } else {
    const fn = evaluateAstNode(node.expression, contextStack)
    return executeFunction(fn, params, contextStack, debugInfo)
  }
}

const executeFunction: ExecuteFunction = (fn, params, contextStack, debugInfo) => {
  if (litsFunction.is(fn)) {
    return functionExecutors[fn.type](fn, params, debugInfo, contextStack, { evaluateAstNode, executeFunction })
  }
  if (Array.isArray(fn)) {
    return evaluateArrayAsFunction(fn, params, debugInfo)
  }
  if (object.is(fn)) {
    return evalueateObjectAsFunction(fn, params, debugInfo)
  }
  if (string.is(fn)) {
    return evaluateStringAsFunction(fn, params, debugInfo)
  }
  if (number.is(fn)) {
    return evaluateNumberAsFunction(fn, params, debugInfo)
  }
  if (Type.isType(fn)) {
    return evaluateTypeAsFunction(fn, params, debugInfo)
  }
  throw new NotAFunctionError(fn, debugInfo)
}

function evaluateBuiltinNormalExpression(
  node: NormalExpressionNodeWithName,
  params: Arr,
  contextStack: ContextStack,
): Any {
  const normalExpression = builtin.normalExpressions[node.name]
  if (!normalExpression) {
    throw new UndefinedSymbolError(node.name, node.token?.debugInfo)
  }

  return normalExpression.evaluate(params, node.token?.debugInfo, contextStack, { executeFunction })
}

function evaluateSpecialExpression(node: SpecialExpressionNode, contextStack: ContextStack): Any {
  const specialExpression = asValue(builtin.specialExpressions[node.name], node.token?.debugInfo)

  return specialExpression.evaluate(node, contextStack, { evaluateAstNode, builtin, lookUp })
}

function evalueateObjectAsFunction(fn: Obj, params: Arr, debugInfo?: DebugInfo): Any {
  if (params.length !== 1) {
    throw new LitsError(`Object as function requires one string parameter.`, debugInfo)
  }
  const key = params[0]
  string.assert(key, debugInfo)
  return toAny(fn[key])
}

function evaluateArrayAsFunction(fn: Arr, params: Arr, debugInfo?: DebugInfo): Any {
  if (params.length !== 1) {
    throw new LitsError(`Array as function requires one non negative integer parameter.`, debugInfo)
  }
  const index = params[0]
  number.assert(index, debugInfo, { integer: true, nonNegative: true })
  return toAny(fn[index])
}

function evaluateStringAsFunction(fn: string, params: Arr, debugInfo?: DebugInfo): Any {
  if (params.length !== 1) {
    throw new LitsError(`String as function requires one Obj parameter.`, debugInfo)
  }
  const param = toAny(params[0])
  if (object.is(param)) {
    return toAny((param as Obj)[fn])
  }
  if (number.is(param, { integer: true })) {
    return toAny(fn[param])
  }
  throw new LitsError(`string as function expects Obj or integer parameter, got ${valueToString(param)}`, debugInfo)
}

function evaluateNumberAsFunction(fn: number, params: Arr, debugInfo?: DebugInfo): Any {
  number.assert(fn, debugInfo, { integer: true })
  if (params.length !== 1) {
    throw new LitsError(`Number as function requires one Arr parameter.`, debugInfo)
  }
  const param = params[0]
  sequence.assert(param, debugInfo)
  return toAny(param[fn])
}

function evaluateTypeAsFunction(typeFunction: Type, params: Arr, debugInfo?: DebugInfo) {
  if (params.length !== 1) {
    throw new LitsError(`ArrayType as function requires one parameter.`, debugInfo)
  }
  if (typeFunction.is(Type.array)) {
    const size = asValue(asNotNull(typeFunction.arrayVariants)[0]).size
    if (size === ArrayVariant.Size.Empty) {
      return Type.emptyArray
    }
    const type = Type.of(params[0])
    return size === ArrayVariant.Size.Unknown ? Type.createTypedArray(type) : Type.createNonEmpyTypedArray(type)
  }
  throw new LitsError(`Type as function requires type to be ::array or ::object.`, debugInfo)
}
