import type { TokenStream } from '../tokenizer/tokenize'
import type { Node } from '../parser/types'

import { LitsError } from '../errors'
import { isOperatorToken } from '../tokenizer/token'
import { ParserContext } from './ParserContext'
import { parseExpression } from './parseExpression'

// Main exported parse function (IIFE pattern)
export function parse(tokenStream: TokenStream): Node[] {
  tokenStream.tokens.forEach((token) => {
    if (token[0] === 'Error') {
      throw new LitsError(token[3], token[2])
    }
  })

  const nodes: Node[] = []

  const ctx = new ParserContext(tokenStream)

  while (!ctx.isAtEnd()) {
    nodes.push(parseExpression(ctx, 0, true))
    if (isOperatorToken(ctx.tryPeek(), ';')) {
      ctx.advance()
    }
    else {
      if (!ctx.isAtEnd()) {
        throw new LitsError('Expected ;', ctx.peekSourceCodeInfo())
      }
    }
  }

  return nodes
}
