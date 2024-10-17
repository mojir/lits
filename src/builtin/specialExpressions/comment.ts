import { AstNodeType, TokenType } from '../../constants/constants'
import type { AstNode, SpecialExpressionNode } from '../../parser/interface'
import { asToken, isToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'

export const commentSpecialExpression: BuiltinSpecialExpression<null> = {
  parse: (tokenStream, position, { parseToken }) => {
    let tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)

    const node: SpecialExpressionNode = {
      t: AstNodeType.SpecialExpression,
      n: 'comment',
      p: [],
      tkn: tkn.sourceCodeInfo ? tkn : undefined,
    }

    while (!isToken(tkn, { type: TokenType.Bracket, value: ')' })) {
      let bodyNode: AstNode
      ;[position, bodyNode] = parseToken(tokenStream, position)
      node.p.push(bodyNode)
      tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
    }
    return [position + 1, node]
  },
  evaluate: () => null,
  analyze: () => ({ undefinedSymbols: new Set() }),
}
