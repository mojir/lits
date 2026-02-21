import type { ArrayNode } from '../../builtin/specialExpressions/array'
import { specialExpressionTypes } from '../../builtin/specialExpressionTypes'
import { NodeTypes } from '../../constants/constants'
import { LitsError } from '../../errors'
import { asLBracketToken, assertRBracketToken, isOperatorToken, isRBracketToken } from '../../tokenizer/token'
import type { AstNode } from '../types'
import { withSourceCodeInfo } from '../helpers'
import type { ParserContext } from '../ParserContext'

export function parseArray(ctx: ParserContext): ArrayNode {
  const firstToken = asLBracketToken(ctx.tryPeek())
  ctx.advance()
  const params: AstNode[] = []
  while (!ctx.isAtEnd() && !isRBracketToken(ctx.tryPeek())) {
    if (isOperatorToken(ctx.tryPeek(), '...')) {
      ctx.advance()
      params.push(withSourceCodeInfo([NodeTypes.Spread, ctx.parseExpression()], ctx.peekSourceCodeInfo()))
    }
    else {
      params.push(ctx.parseExpression())
    }
    const nextToken = ctx.tryPeek()
    if (!isOperatorToken(nextToken, ',') && !isRBracketToken(nextToken)) {
      throw new LitsError('Expected comma or closing parenthesis', ctx.peekSourceCodeInfo())
    }
    if (isOperatorToken(nextToken, ',')) {
      ctx.advance()
    }
  }

  assertRBracketToken(ctx.tryPeek())
  ctx.advance()

  return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.array, params]], firstToken[2])
}
