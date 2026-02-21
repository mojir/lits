import type { LetNode } from '../../builtin/specialExpressions/let'
import { specialExpressionTypes } from '../../builtin/specialExpressionTypes'
import { NodeTypes } from '../../constants/constants'
import type { BindingNode } from '../types'
import type { SymbolToken } from '../../tokenizer/token'
import { withSourceCodeInfo } from '../helpers'
import type { ParserContext } from '../ParserContext'
import { parseBindingTarget } from './parseBindingTarget'

export function parseLet(ctx: ParserContext, token: SymbolToken): LetNode {
  ctx.advance()

  const target = parseBindingTarget(ctx, { requireDefaultValue: true, noRest: true })

  const value = target[1][1]!
  target[1][1] = undefined

  const bindingTarget: BindingNode = withSourceCodeInfo([NodeTypes.Binding, [target, value]], token[2])
  return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.let, bindingTarget]], token[2]) satisfies LetNode
}
