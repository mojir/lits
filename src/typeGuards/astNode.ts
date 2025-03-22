import { NodeTypes } from '../constants/constants'
import type {
  ExpressionNode,
  Node,
  NormalExpressionNode,
  NormalExpressionNodeWithName,
  SpreadNode,
  SymbolNode,
} from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { getAssertionError } from '../utils/getAssertionError'

// export function isNode(value: Node): value is Node {
//   if (value === null || typeof value !== 'object')
//     return false

//   if (!isNodeType((value as Node).type))
//     return false

//   return true
// }
// export function asNode(value: Node, sourceCodeInfo?: SourceCodeInfo): Node {
//   assertNode(value, sourceCodeInfo)
//   return value
// }
// export function assertNode(value: Node, sourceCodeInfo?: SourceCodeInfo): asserts value is Node {
//   if (!isNode(value))
//     throw getAssertionError('Node', value, sourceCodeInfo)
// }

export function isSymbolNode(value: Node): value is SymbolNode {
  return value[0] === NodeTypes.Symbol
}
export function asSymbolNode(value: Node, sourceCodeInfo?: SourceCodeInfo): SymbolNode {
  assertSymbolNode(value, sourceCodeInfo)
  return value
}
export function assertSymbolNode(value: Node, sourceCodeInfo?: SourceCodeInfo): asserts value is SymbolNode {
  if (!isSymbolNode(value))
    throw getAssertionError('SymbolNode', value, sourceCodeInfo)
}

// export function isNumberNode(value: Node): value is NumberNode {
//   return value[0] === NodeTypes.Number
// }
// export function asNumberNode(value: Node, sourceCodeInfo?: SourceCodeInfo): NumberNode {
//   assertNumberNode(value, sourceCodeInfo)
//   return value
// }
// export function assertNumberNode(value: Node, sourceCodeInfo?: SourceCodeInfo): asserts value is NumberNode {
//   if (!isNumberNode(value))
//     throw getAssertionError('NumberNode', value, sourceCodeInfo)
// }

export function isNormalExpressionNode(value: Node): value is NormalExpressionNode {
  return value[0] === NodeTypes.NormalExpression
}
export function asNormalExpressionNode(value: Node, sourceCodeInfo?: SourceCodeInfo): NormalExpressionNode {
  assertNormalExpressionNode(value, sourceCodeInfo)
  return value
}
export function assertNormalExpressionNode(
  value: Node,
  sourceCodeInfo?: SourceCodeInfo,
): asserts value is NormalExpressionNode {
  if (!isNormalExpressionNode(value))
    throw getAssertionError('NormalExpressionNode', value, sourceCodeInfo)
}

export function isNormalExpressionNodeWithName(value: Node): value is NormalExpressionNodeWithName {
  if (!isNormalExpressionNode(value)) {
    return false
  }
  return typeof value[1][0] === 'string'
}
export function asNormalExpressionNodeWithName(
  value: Node,
  sourceCodeInfo?: SourceCodeInfo,
): NormalExpressionNodeWithName {
  assertNormalExpressionNodeWithName(value, sourceCodeInfo)
  return value
}
export function assertNormalExpressionNodeWithName(
  value: Node,
  sourceCodeInfo?: SourceCodeInfo,
): asserts value is NormalExpressionNodeWithName {
  if (!isNormalExpressionNodeWithName(value))
    throw getAssertionError('NormalExpressionNodeWithName', value, sourceCodeInfo)
}

export function isExpressionNode(value: Node): value is ExpressionNode {
  return isNormalExpressionNode(value)
    || value[0] === NodeTypes.SpecialExpression
    || value[0] === NodeTypes.Number
    || value[0] === NodeTypes.String
}
export function asExpressionNode(value: Node, sourceCodeInfo?: SourceCodeInfo): ExpressionNode {
  assertExpressionNode(value, sourceCodeInfo)
  return value
}
export function assertExpressionNode(value: Node, sourceCodeInfo?: SourceCodeInfo): asserts value is ExpressionNode {
  if (!isExpressionNode(value))
    throw getAssertionError('ExpressionNode', value, sourceCodeInfo)
}

export function isSpreadNode(value: Node): value is SpreadNode {
  return value[0] === NodeTypes.Spread
}
