import type { MatchCase, MatchNode } from '../../builtin/specialExpressions/match'
import { type SymbolToken, assertReservedSymbolToken, isReservedSymbolToken } from '../../tokenizer/token'
import { NodeTypes } from '../../constants/constants'
import { specialExpressionTypes } from '../../builtin/specialExpressionTypes'
import type { ParserContext } from '../ParserContext'
import { withSourceCodeInfo } from '../helpers'
import { parseImplicitBlock } from './parseImplicitBlock'
import { parseBindingTarget } from './parseBindingTarget'

export function parseMatch(ctx: ParserContext, token: SymbolToken): MatchNode {
  ctx.advance()
  const valueExpression = ctx.parseExpression()
  const params: MatchCase[] = []

  while (!ctx.isAtEnd() && !isReservedSymbolToken(ctx.tryPeek(), 'end')) {
    assertReservedSymbolToken(ctx.tryPeek(), 'case')
    ctx.advance()

    // Parse pattern instead of expression
    const pattern = parseBindingTarget(ctx, { allowLiteralPatterns: true })

    // Check for optional guard: `when <expression>`
    let guard
    if (isReservedSymbolToken(ctx.tryPeek(), 'when')) {
      ctx.advance()
      guard = ctx.parseExpression()
    }

    assertReservedSymbolToken(ctx.tryPeek(), 'then')
    ctx.advance()
    const thenExpression = parseImplicitBlock(ctx, ['case', 'end'])

    params.push([pattern, thenExpression, guard])
    if (isReservedSymbolToken(ctx.tryPeek(), 'end')) {
      break
    }
  }

  assertReservedSymbolToken(ctx.tryPeek(), 'end')
  ctx.advance()

  return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.match, valueExpression, params]], token[2]) satisfies MatchNode
}
