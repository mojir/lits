import { AstNode, NodeType } from '../parser/interface'

const astTypes: Record<NodeType, true> = {
  Number: true,
  String: true,
  NormalExpression: true,
  SpecialExpression: true,
  Name: true,
  Modifier: true,
  ReservedName: true,
  Binding: true,
  Argument: true,
  Partial: true,
}

export function isAstNode(value: unknown): value is AstNode {
  if (value === null || typeof value !== `object`) {
    return false
  }
  if (!(value as AstNode).token) {
    return false
  }
  if (!astTypes[(value as AstNode).type]) {
    return false
  }
  return true
}
