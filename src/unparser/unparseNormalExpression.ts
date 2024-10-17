import { TokenType } from '../constants/constants'
import { isNormalExpressionNode } from '../typeGuards/astNode'
import type { UnparseOptions } from './UnparseOptions'
import type { ExpressionWithParamsNode } from './unparse'
import { unparseArrayLiteral } from './unparseArrayLiteral'
import { unparseObjectLiteral } from './unparseObjectLiteral'
import { unparseParams } from './unparseParams'
import { applyMetaTokens } from './utils'

export function unparseNormalExpressionNode(node: ExpressionWithParamsNode, options: UnparseOptions): string {
  if (isNormalExpressionNode(node)) {
    if (node.debugData?.token.t === TokenType.Bracket && node.debugData.token.v === '[')
      return unparseArrayLiteral(node, options)
    else if (node.debugData?.token.t === TokenType.Bracket && node.debugData.token.v === '{')
      return unparseObjectLiteral(node, options)
  }

  const startBracket = applyMetaTokens('(', node.debugData?.token.debugData?.metaTokens, options)
  const endBracket = applyMetaTokens(')', node.debugData?.lastToken?.debugData?.metaTokens, options.inline())

  // if expression node e.g. ("Albert" 2), first parameter is the name ("Albert")
  const nameOptions = startBracket.endsWith('\n') ? options.noInline().inc() : options.inline().inc()
  const name = node.n
    ? applyMetaTokens(node.n, node.debugData?.nameToken?.debugData?.metaTokens, nameOptions)
    : options.unparse(node.p[0]!, nameOptions)

  const params = node.n ? node.p : node.p.slice(1)

  const prefix = startBracket + name
  const inline = !name.includes('\n')
  return unparseParams({ params, options, prefix, inline, name, endBracket })
}
