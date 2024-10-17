import { AstNodeType } from '../constants/constants'
import type { Ast, AstNode, NormalExpressionNode } from '../parser/interface'
import { UnparseOptions } from './UnparseOptions'
import { unparseNormalExpressionNode } from './unparseNormalExpression'
import { applyMetaTokens, ensureNewlineSeparator } from './utils'
import { unparseSpecialExpression } from './unparseSpecialExpression'

export type ExpressionWithParamsNode = Pick<NormalExpressionNode, 'debugData' | 'n' | 'p'>
export type Unparse = (node: AstNode, options: UnparseOptions) => string

const unparse: Unparse = (node: AstNode, options: UnparseOptions) => {
  switch (node.t) {
    case AstNodeType.String:
      return node.debugData?.token.o?.s
        ? applyMetaTokens(`:${node.v}`, node.debugData?.token.debugData?.metaTokens, options) // Keyword
        : applyMetaTokens(`"${node.v}"`, node.debugData?.token.debugData?.metaTokens, options)
    case AstNodeType.Number:
    case AstNodeType.Name:
    case AstNodeType.Modifier:
    case AstNodeType.ReservedName:
      return applyMetaTokens(node.v, node.debugData?.token.debugData?.metaTokens, options)
    case AstNodeType.Comment:
      return `${applyMetaTokens(node.v, node.debugData?.token.debugData?.metaTokens, options)}\n`
    case AstNodeType.NormalExpression: {
      return unparseNormalExpressionNode(node, options)
    }
    case AstNodeType.SpecialExpression:
      return unparseSpecialExpression(node, options)
  }
}

export function unparseAst(ast: Ast, lineLength: number): string {
  const options = new UnparseOptions(unparse, lineLength || Number.MAX_SAFE_INTEGER)
  const result = ast.b.reduce<string>((acc, node) => {
    const nodeUnparsed = unparse(node, options)
    return ensureNewlineSeparator(acc, nodeUnparsed)
  }, '')
  return result.trim()
}
