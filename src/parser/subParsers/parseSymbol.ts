import { LitsError } from '../../errors'
import type { NormalBuiltinSymbolNode, SpecialBuiltinSymbolNode, SymbolNode } from '../types'
import { isSymbolToken } from '../../tokenizer/token'
import { stringFromQuotedSymbol, stringToSymbolNode } from '../helpers'
import type { ParserContext } from '../ParserContext'

export function parseSymbol(ctx: ParserContext): SymbolNode | NormalBuiltinSymbolNode | SpecialBuiltinSymbolNode {
  const token = ctx.peek()
  ctx.advance()
  if (!isSymbolToken(token)) {
    throw new LitsError(`Expected symbol token, got ${token[0]}`, token[2])
  }
  if (token[1][0] === '\'') {
    return stringToSymbolNode(stringFromQuotedSymbol(token[1]), token[2])
  }
  else {
    return stringToSymbolNode(token[1], token[2])
  }
}
