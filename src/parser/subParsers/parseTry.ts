import type { TryNode, WithHandler } from '../../builtin/specialExpressions/try'
import { specialExpressionTypes } from '../../builtin/specialExpressionTypes'
import { NodeTypes } from '../../constants/constants'
import type { AstNode, SymbolNode } from '../types'
import type { SymbolToken } from '../../tokenizer/token'
import { assertRParenToken, assertReservedSymbolToken, isLParenToken, isReservedSymbolToken } from '../../tokenizer/token'
import { withSourceCodeInfo } from '../helpers'
import type { ParserContext } from '../ParserContext'
import { parseImplicitBlock } from './parseImplicitBlock'
import { parseSymbol } from './parseSymbol'

export function parseTry(ctx: ParserContext, token: SymbolToken): TryNode {
  ctx.advance()

  // Parse the try body — it ends at 'with', 'catch', or 'end' (only 'end' when neither with nor catch)
  const tryExpression = parseImplicitBlock(ctx, ['with', 'catch'])

  // Parse optional with-handlers
  const withHandlers: WithHandler[] = []
  if (isReservedSymbolToken(ctx.tryPeek(), 'with')) {
    ctx.advance()
    while (!ctx.isAtEnd() && !isReservedSymbolToken(ctx.tryPeek(), 'end') && !isReservedSymbolToken(ctx.tryPeek(), 'catch')) {
      assertReservedSymbolToken(ctx.tryPeek(), 'case')
      ctx.advance()
      const effectExpr = ctx.parseExpression()
      assertReservedSymbolToken(ctx.tryPeek(), 'then')
      ctx.advance()
      const handlerFn = parseImplicitBlock(ctx, ['case', 'catch', 'end'])
      withHandlers.push([effectExpr, handlerFn])
    }
  }

  // Parse optional catch clause
  let errorSymbol: SymbolNode | undefined
  let catchExpression: AstNode | undefined
  if (isReservedSymbolToken(ctx.tryPeek(), 'catch')) {
    ctx.advance()

    if (isLParenToken(ctx.tryPeek())) {
      ctx.advance()
      errorSymbol = parseSymbol(ctx)
      assertRParenToken(ctx.tryPeek())
      ctx.advance()
    }

    catchExpression = parseImplicitBlock(ctx, ['end'])
  }

  assertReservedSymbolToken(ctx.tryPeek(), 'end')
  ctx.advance()

  return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.try, tryExpression, errorSymbol, catchExpression, withHandlers]], token[2]) satisfies TryNode
}
