import type { SpecialExpressionName } from '../builtin'
import { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import { NodeTypes } from '../constants/constants'
import type {
  ExpressionNode,
  Node,
  NormalBuiltinSymbolNode,
  NormalExpressionNode,
  NormalExpressionNodeWithName,
  SpecialBuiltinSymbolNode,
  SpreadNode,
  SymbolNode,
  UserDefinedSymbolNode,
} from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { getAssertionError } from '../utils/getAssertionError'

export function isSymbolNode(node: Node): node is SymbolNode {
  const nodeType = node[0]
  return NodeTypes.UserDefinedSymbol === nodeType
    || NodeTypes.NormalBuiltinSymbol === nodeType
    || NodeTypes.SpecialBuiltinSymbol === nodeType
}
export function asSymbolNode(node: Node, sourceCodeInfo?: SourceCodeInfo): SymbolNode {
  assertSymbolNode(node, sourceCodeInfo)
  return node
}
export function assertSymbolNode(node: Node, sourceCodeInfo?: SourceCodeInfo): asserts node is SymbolNode {
  if (!isSymbolNode(node))
    throw getAssertionError('SymbolNode', node, sourceCodeInfo)
}

export function isUserDefinedSymbolNode(node: Node): node is UserDefinedSymbolNode {
  return NodeTypes.UserDefinedSymbol === node[0]
}
export function asUserDefinedSymbolNode(node: Node, sourceCodeInfo?: SourceCodeInfo): UserDefinedSymbolNode {
  assertUserDefinedSymbolNode(node, sourceCodeInfo)
  return node
}
export function assertUserDefinedSymbolNode(node: Node, sourceCodeInfo?: SourceCodeInfo): asserts node is UserDefinedSymbolNode {
  if (!isUserDefinedSymbolNode(node))
    throw getAssertionError('UserDefinedSymbolNode', node, sourceCodeInfo)
}

export function isNormalBuiltinSymbolNode(node: Node): node is NormalBuiltinSymbolNode {
  return NodeTypes.NormalBuiltinSymbol === node[0]
}
export function asNormalBuiltinSymbolNode(node: Node, sourceCodeInfo?: SourceCodeInfo): NormalBuiltinSymbolNode {
  assertNormalBuiltinSymbolNode(node, sourceCodeInfo)
  return node
}
export function assertNormalBuiltinSymbolNode(node: Node, sourceCodeInfo?: SourceCodeInfo): asserts node is NormalBuiltinSymbolNode {
  if (!isNormalBuiltinSymbolNode(node))
    throw getAssertionError('NormalBuiltinSymbolNode', node, sourceCodeInfo)
}

export function isSpecialBuiltinSymbolNode(node: Node, name?: SpecialExpressionName): node is SpecialBuiltinSymbolNode {
  if (NodeTypes.SpecialBuiltinSymbol !== node[0]) {
    return false
  }
  if (name === undefined) {
    return true
  }
  const specialExpressionNode = node as SpecialBuiltinSymbolNode
  return specialExpressionNode[1] === specialExpressionTypes[name]
}
export function asSpecialBuiltinSymbolNode(node: Node, sourceCodeInfo?: SourceCodeInfo): SpecialBuiltinSymbolNode {
  assertSpecialBuiltinSymbolNode(node, sourceCodeInfo)
  return node
}
export function assertSpecialBuiltinSymbolNode(node: Node, sourceCodeInfo?: SourceCodeInfo): asserts node is SpecialBuiltinSymbolNode {
  if (!isSpecialBuiltinSymbolNode(node))
    throw getAssertionError('SpecialBuiltinSymbolNode', node, sourceCodeInfo)
}

// export function isNumberNode(node: Node): node is NumberNode {
//   return node[0] === NodeTypes.Number
// }
// export function asNumberNode(node: Node, sourceCodeInfo?: SourceCodeInfo): NumberNode {
//   assertNumberNode(node, sourceCodeInfo)
//   return node
// }
// export function assertNumberNode(node: Node, sourceCodeInfo?: SourceCodeInfo): asserts node is NumberNode {
//   if (!isNumberNode(node))
//     throw getAssertionError('NumberNode', node, sourceCodeInfo)
// }

export function isNormalExpressionNode(node: Node): node is NormalExpressionNode {
  return node[0] === NodeTypes.NormalExpression
}
export function asNormalExpressionNode(node: Node, sourceCodeInfo?: SourceCodeInfo): NormalExpressionNode {
  assertNormalExpressionNode(node, sourceCodeInfo)
  return node
}
export function assertNormalExpressionNode(
  node: Node,
  sourceCodeInfo?: SourceCodeInfo,
): asserts node is NormalExpressionNode {
  if (!isNormalExpressionNode(node))
    throw getAssertionError('NormalExpressionNode', node, sourceCodeInfo)
}

export function isNormalExpressionNodeWithName(node: Node): node is NormalExpressionNodeWithName {
  if (!isNormalExpressionNode(node)) {
    return false
  }
  return isSymbolNode(node[1][0])
}
export function asNormalExpressionNodeWithName(
  node: Node,
  sourceCodeInfo?: SourceCodeInfo,
): NormalExpressionNodeWithName {
  assertNormalExpressionNodeWithName(node, sourceCodeInfo)
  return node
}
export function assertNormalExpressionNodeWithName(
  node: Node,
  sourceCodeInfo?: SourceCodeInfo,
): asserts node is NormalExpressionNodeWithName {
  if (!isNormalExpressionNodeWithName(node))
    throw getAssertionError('NormalExpressionNodeWithName', node, sourceCodeInfo)
}

export function isExpressionNode(node: Node): node is ExpressionNode {
  return isNormalExpressionNode(node)
    || node[0] === NodeTypes.SpecialExpression
    || node[0] === NodeTypes.Number
    || node[0] === NodeTypes.String
}
export function asExpressionNode(node: Node, sourceCodeInfo?: SourceCodeInfo): ExpressionNode {
  assertExpressionNode(node, sourceCodeInfo)
  return node
}
export function assertExpressionNode(node: Node, sourceCodeInfo?: SourceCodeInfo): asserts node is ExpressionNode {
  if (!isExpressionNode(node))
    throw getAssertionError('ExpressionNode', node, sourceCodeInfo)
}

export function isSpreadNode(node: Node): node is SpreadNode {
  return node[0] === NodeTypes.Spread
}
