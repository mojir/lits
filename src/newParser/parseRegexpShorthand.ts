import { normalExpressionTypes } from '../builtin/normalExpressions'
import { NodeTypes } from '../constants/constants'
import type { NormalExpressionNodeWithName, StringNode } from '../parser/types'
import { withSourceCodeInfo } from './helpers'
import type { ParserContext } from './ParserContext'

export function parseRegexpShorthand(ctx: ParserContext): NormalExpressionNodeWithName {
  const token = ctx.peek()
  ctx.advance()

  const endStringPosition = token[1].lastIndexOf('"')
  const regexpString = token[1].substring(2, endStringPosition)
  const optionsString = token[1].substring(endStringPosition + 1)
  const stringNode: StringNode = withSourceCodeInfo([NodeTypes.String, regexpString], token[2]) satisfies StringNode

  const optionsNode: StringNode = withSourceCodeInfo([NodeTypes.String, optionsString], token[2]) satisfies StringNode

  const node: NormalExpressionNodeWithName = withSourceCodeInfo([
    NodeTypes.NormalExpression,
    [
      withSourceCodeInfo([NodeTypes.NormalBuiltinSymbol, normalExpressionTypes.regexp as number], token[2]),
      [stringNode, optionsNode],
    ],
  ], token[2])

  return node
}
