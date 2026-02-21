import { NodeTypes } from '../constants/constants'
import type { NumberNode, ReservedSymbolNode } from '../parser/types'
import { isNumberReservedSymbol, numberReservedSymbolRecord } from '../tokenizer/reservedNames'
import { asReservedSymbolToken } from '../tokenizer/token'
import { withSourceCodeInfo } from './helpers'
import type { ParserContext } from './ParserContext'

export function parseReservedSymbol(ctx: ParserContext): ReservedSymbolNode | NumberNode {
  const token = asReservedSymbolToken(ctx.tryPeek())
  ctx.advance()

  const symbol = token[1]
  if (isNumberReservedSymbol(symbol)) {
    return withSourceCodeInfo([NodeTypes.Number, numberReservedSymbolRecord[symbol]], token[2]) satisfies NumberNode
  }
  return withSourceCodeInfo([NodeTypes.ReservedSymbol, token[1]], token[2]) satisfies ReservedSymbolNode
}
