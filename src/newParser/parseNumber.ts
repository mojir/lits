import { NodeTypes } from '../constants/constants'
import type { NumberNode } from '../parser/types'
import { withSourceCodeInfo } from './helpers'
import type { ParserContext } from './ParserContext'

export function parseNumber(ctx: ParserContext): NumberNode {
  const token = ctx.peek()
  ctx.advance()

  const value = token[1]
  const negative = value[0] === '-'
  const numberString = (negative ? value.substring(1) : value).replace(/_/g, '')
  return withSourceCodeInfo([NodeTypes.Number, negative ? -Number(numberString) : Number(numberString)], token[2]) satisfies NumberNode
}
