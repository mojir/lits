import type { LoopNode } from '../../builtin/specialExpressions/loop'
import { specialExpressionTypes } from '../../builtin/specialExpressionTypes'
import { NodeTypes } from '../../constants/constants'
import { LitsError } from '../../errors'
import type { BindingNode } from '../types'
import type { SymbolToken } from '../../tokenizer/token'
import { assertLParenToken, assertOperatorToken, assertRParenToken, isOperatorToken, isRParenToken } from '../../tokenizer/token'
import { withSourceCodeInfo } from '../helpers'
import type { ParserContext } from '../ParserContext'
import { parseBindingTarget } from './parseBindingTarget'

export function parseLoop(ctx: ParserContext, firstToken: SymbolToken): LoopNode {
  ctx.advance()

  assertLParenToken(ctx.tryPeek())
  ctx.advance()

  const bindingNodes: BindingNode[] = []
  let token = ctx.tryPeek()
  while (!ctx.isAtEnd() && !isRParenToken(token)) {
    const target = parseBindingTarget(ctx, { requireDefaultValue: true, noRest: true })
    const value = target[1][1]!
    target[1][1] = undefined

    bindingNodes.push(withSourceCodeInfo([NodeTypes.Binding, [target, value]], target[2]) satisfies BindingNode)

    if (isOperatorToken(ctx.tryPeek(), ',')) {
      ctx.advance()
    }
    token = ctx.tryPeek()
  }
  if (bindingNodes.length === 0) {
    throw new LitsError('Expected binding', ctx.peekSourceCodeInfo())
  }

  assertRParenToken(token)
  ctx.advance()

  assertOperatorToken(ctx.tryPeek(), '->')
  ctx.advance()

  const expression = ctx.parseExpression()

  return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.loop, bindingNodes, expression]], firstToken[2]) satisfies LoopNode
}
