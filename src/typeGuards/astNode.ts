import { AstNodeType, isAstNodeType } from '../constants/constants'
import type {
  AstNode,
  ExpressionNode,
  NormalExpressionNode,
  NormalExpressionNodeWithName,
  SymbolNode,
} from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { getAssertionError } from '../utils/getAssertionError'

export function isAstNode(value: unknown): value is AstNode {
  if (value === null || typeof value !== 'object')
    return false

  if (!isAstNodeType((value as AstNode).t))
    return false

  return true
}
export function asAstNode(value: unknown, sourceCodeInfo?: SourceCodeInfo): AstNode {
  assertAstNode(value, sourceCodeInfo)
  return value
}
export function assertAstNode(value: unknown, sourceCodeInfo?: SourceCodeInfo): asserts value is AstNode {
  if (!isAstNode(value))
    throw getAssertionError('AstNode', value, sourceCodeInfo)
}

export function isSymbolNode(value: unknown): value is SymbolNode {
  if (!isAstNode(value))
    return false

  return value.t === AstNodeType.Symbol
}
export function asSymbolNode(value: unknown, sourceCodeInfo?: SourceCodeInfo): SymbolNode {
  assertSymbolNode(value, sourceCodeInfo)
  return value
}
export function assertSymbolNode(value: unknown, sourceCodeInfo?: SourceCodeInfo): asserts value is SymbolNode {
  if (!isSymbolNode(value))
    throw getAssertionError('SymbolNode', value, sourceCodeInfo)
}

export function isNormalExpressionNode(value: unknown): value is NormalExpressionNode {
  if (!isAstNode(value))
    return false

  return value.t === AstNodeType.NormalExpression
}
export function asNormalExpressionNode(value: unknown, sourceCodeInfo?: SourceCodeInfo): NormalExpressionNode {
  assertNormalExpressionNode(value, sourceCodeInfo)
  return value
}
export function assertNormalExpressionNode(
  value: unknown,
  sourceCodeInfo?: SourceCodeInfo,
): asserts value is NormalExpressionNode {
  if (!isNormalExpressionNode(value))
    throw getAssertionError('NormalExpressionNode', value, sourceCodeInfo)
}

export function isNormalExpressionNodeWithName(value: unknown): value is NormalExpressionNodeWithName {
  if (!isAstNode(value))
    return false

  return value.t === AstNodeType.NormalExpression && typeof value.n === 'string'
}
export function asNormalExpressionNodeWithName(
  value: unknown,
  sourceCodeInfo?: SourceCodeInfo,
): NormalExpressionNodeWithName {
  assertNormalExpressionNodeWithName(value, sourceCodeInfo)
  return value
}
export function assertNormalExpressionNodeWithName(
  value: unknown,
  sourceCodeInfo?: SourceCodeInfo,
): asserts value is NormalExpressionNodeWithName {
  if (!isNormalExpressionNodeWithName(value))
    throw getAssertionError('NormalExpressionNodeWithName', value, sourceCodeInfo)
}

export function isExpressionNode(value: unknown): value is ExpressionNode {
  if (!isAstNode(value))
    return false

  return (
    value.t === AstNodeType.NormalExpression
    || value.t === AstNodeType.SpecialExpression
    || value.t === AstNodeType.Number
    || value.t === AstNodeType.String
  )
}
export function asExpressionNode(value: unknown, sourceCodeInfo?: SourceCodeInfo): ExpressionNode {
  assertExpressionNode(value, sourceCodeInfo)
  return value
}
export function assertExpressionNode(value: unknown, sourceCodeInfo?: SourceCodeInfo): asserts value is ExpressionNode {
  if (!isExpressionNode(value))
    throw getAssertionError('ExpressionNode', value, sourceCodeInfo)
}
