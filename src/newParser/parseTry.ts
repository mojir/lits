import type { TryNode } from '../builtin/specialExpressions/try'
import { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import { NodeTypes } from '../constants/constants'
import type { SymbolNode } from '../parser/types'
import type { SymbolToken } from '../tokenizer/token'
import { assertRParenToken, isLParenToken } from '../tokenizer/token'
import { withSourceCodeInfo } from './helpers'
import { parseImplicitBlock } from './parseImplicitBlock'
import type { ParserContext } from './ParserContext'
import { parseSymbol } from './parseSymbol'

export function parseTry(ctx: ParserContext, token: SymbolToken): TryNode {
  ctx.advance()
  const tryExpression = parseImplicitBlock(ctx, ['catch'])

  ctx.advance()

  let errorSymbol: SymbolNode | undefined
  if (isLParenToken(ctx.tryPeek())) {
    ctx.advance()
    errorSymbol = parseSymbol(ctx)
    assertRParenToken(ctx.tryPeek())
    ctx.advance()
  }

  const catchExpression = parseImplicitBlock(ctx, ['end'])

  ctx.advance()

  return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.try, tryExpression, errorSymbol, catchExpression]], token[2]) satisfies TryNode
}
