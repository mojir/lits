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

// function assert(
//   value: unknown,
//   sourceCodeInfo: SourceCodeInfo,
//   options: AstNodeOptions = {},
// ): asserts value is AstNode {
//   if (!is(value, options)) {
//     throw new LitsError(`Expected AstNode${options.type ? ` (${options.type})` : ``}, got ${value}`, sourceCodeInfo)
//   }
// }

// function as(value: unknown, sourceCodeInfo: SourceCodeInfo, options: NumberOptions = {}): number {
//   assert(value, sourceCodeInfo, options)
//   return value
// }

// export const number: {
//   is: (value: unknown, options?: NumberOptions) => value is number
//   as: (value: unknown, sourceCodeInfo: SourceCodeInfo, options?: NumberOptions) => number
//   assert(value: unknown, sourceCodeInfo: SourceCodeInfo, options?: NumberOptions): asserts value is number
// } = {
//   is,
//   as,
//   assert,
// }
