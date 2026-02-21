import { isOperatorToken, isReservedSymbolToken } from '../../tokenizer/token'
import type { AstNode } from '../types'
import { LitsError } from '../../errors'
import { NodeTypes } from '../../constants/constants'
import { specialExpressionTypes } from '../../builtin/specialExpressionTypes'
import type { DoNode } from '../../builtin/specialExpressions/block'
import type { ParserContext } from '../ParserContext'
import { withSourceCodeInfo } from '../helpers'

type ImplicitBlockEnd = 'end' | 'else' | 'catch' | 'case'

export function parseImplicitBlock(ctx: ParserContext, ends: ImplicitBlockEnd[]): AstNode {
  const nodes: AstNode[] = []
  while (!ctx.isAtEnd() && !isImplicitBlockEnd(ctx, ends)) {
    if (isOperatorToken(ctx.tryPeek(), ';')) {
      ctx.advance()
    }
    else {
      nodes.push(ctx.parseExpression())
    }
  }
  assertImplicitBlockEnd(ctx, ends)

  if (nodes.length === 0) {
    throw new LitsError('Expected expression', ctx.peekSourceCodeInfo())
  }

  return nodes.length === 1
    ? nodes[0]!
    : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.block, nodes]], ctx.peekSourceCodeInfo()) satisfies DoNode
}

function assertImplicitBlockEnd(ctx: ParserContext, ends: ImplicitBlockEnd[]): void {
  if (!isImplicitBlockEnd(ctx, ends)) {
    throw new LitsError(`Expected ${ends.map(e => e[1]).join(' or ')}`, ctx.peekSourceCodeInfo())
  }
}

function isImplicitBlockEnd(ctx: ParserContext, ends: ImplicitBlockEnd[]): boolean {
  for (const end of ends) {
    if (isReservedSymbolToken(ctx.tryPeek(), end)) {
      return true
    }
  }
  return false
}
