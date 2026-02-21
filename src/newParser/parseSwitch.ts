import type { SwitchNode } from '../builtin/specialExpressions/switch'
import { type SymbolToken, assertReservedSymbolToken, isReservedSymbolToken } from '../tokenizer/token'
import type { Node } from '../parser/types'
import { NodeTypes } from '../constants/constants'
import { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import type { ParserContext } from './ParserContext'
import { parseImplicitBlock } from './parseImplicitBlock'
import { withSourceCodeInfo } from './helpers'

export function parseSwitch(ctx: ParserContext, token: SymbolToken): SwitchNode {
  ctx.advance()
  const valueExpression = ctx.parseExpression()
  const params: [Node, Node][] = []

  while (!ctx.isAtEnd() && !isReservedSymbolToken(ctx.tryPeek(), 'end')) {
    assertReservedSymbolToken(ctx.tryPeek(), 'case')
    ctx.advance()
    const caseExpression = ctx.parseExpression()
    assertReservedSymbolToken(ctx.tryPeek(), 'then')
    ctx.advance()
    const thenExpression = parseImplicitBlock(ctx, ['case', 'end'])

    params.push([caseExpression, thenExpression])
    if (isReservedSymbolToken(ctx.tryPeek(), 'end')) {
      break
    }
  }

  assertReservedSymbolToken(ctx.tryPeek(), 'end')
  ctx.advance()

  return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.switch, valueExpression, params]], token[2]) satisfies SwitchNode
}
