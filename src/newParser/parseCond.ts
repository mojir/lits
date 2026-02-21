import type { CondNode } from '../builtin/specialExpressions/cond'
import { type SymbolToken, assertReservedSymbolToken, isReservedSymbolToken } from '../tokenizer/token'
import type { Node } from '../parser/types'
import { NodeTypes } from '../constants/constants'
import { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import { parseExpression } from './parseExpression'
import { parseImplicitBlock } from './parseImplicitBlock'
import type { ParserContext } from './ParserContext'
import { withSourceCodeInfo } from './helpers'

export function parseCond(ctx: ParserContext, token: SymbolToken): CondNode {
  ctx.advance()

  const params: [Node, Node][] = []

  while (!ctx.isAtEnd() && !isReservedSymbolToken(ctx.tryPeek(), 'end')) {
    assertReservedSymbolToken(ctx.tryPeek(), 'case')
    ctx.advance()
    const caseExpression = parseExpression(ctx)
    assertReservedSymbolToken(ctx.tryPeek(), 'then')
    ctx.advance()
    const thenExpression = parseImplicitBlock(ctx, ['case', 'end'])

    params.push([caseExpression, thenExpression])
    if (isReservedSymbolToken(ctx.tryPeek(), 'end')) {
      break
    }
  }

  assertReservedSymbolToken(ctx.tryPeek())
  ctx.advance()

  return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.cond, params]], token[2]) satisfies CondNode
}
