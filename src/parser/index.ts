import type { TokenStream } from '../tokenizer/tokenize'
import { LitsError } from '../errors'
import { isOperatorToken } from '../tokenizer/token'
import type { AstNode } from './types'
import { createParserContext, parseExpression } from './subParsers/parseExpression'

export { createParserContext, parseExpression }

export function parse(tokenStream: TokenStream): AstNode[] {
  tokenStream.tokens.forEach((token) => {
    if (token[0] === 'Error') {
      throw new LitsError(token[3], token[2])
    }
  })

  const nodes: AstNode[] = []

  const ctx = createParserContext(tokenStream)

  while (!ctx.isAtEnd()) {
    nodes.push(parseExpression(ctx, 0))
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
