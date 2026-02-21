import type { IfNode } from '../../builtin/specialExpressions/if'
import type { UnlessNode } from '../../builtin/specialExpressions/unless'
import { specialExpressionTypes } from '../../builtin/specialExpressionTypes'
import { NodeTypes } from '../../constants/constants'
import type { SymbolToken } from '../../tokenizer/token'
import { assertReservedSymbolToken, isReservedSymbolToken } from '../../tokenizer/token'
import type { AstNode } from '../types'
import { withSourceCodeInfo } from '../helpers'
import type { ParserContext } from '../ParserContext'
import { parseImplicitBlock } from './parseImplicitBlock'

export function parseIfOrUnless(ctx: ParserContext, token: SymbolToken): IfNode | UnlessNode {
  const isUnless = token[1] === 'unless'
  ctx.advance()
  const condition = ctx.parseExpression()
  assertReservedSymbolToken(ctx.tryPeek(), 'then')
  ctx.advance()
  const thenExpression = parseImplicitBlock(ctx, ['else', 'end'])

  let elseExpression: AstNode | undefined
  if (isReservedSymbolToken(ctx.tryPeek(), 'else')) {
    ctx.advance()
    elseExpression = parseImplicitBlock(ctx, ['end'])
  }

  ctx.advance()

  return isUnless
    ? withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.unless, [condition, thenExpression, elseExpression]]], token[2]) satisfies UnlessNode
    : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.if, [condition, thenExpression, elseExpression]]], token[2]) satisfies IfNode
}
