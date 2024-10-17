import { AstNodeType, TokenType } from '../../constants/constants'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { AstNode, CommonSpecialExpressionNode } from '../../parser/interface'
import { asToken, isToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'

export interface DoNode extends CommonSpecialExpressionNode<'do'> {}

export const doSpecialExpression: BuiltinSpecialExpression<Any, DoNode> = {
  parse: (tokenStream, position, firstToken, { parseToken }) => {
    const node: DoNode = {
      t: AstNodeType.SpecialExpression,
      n: 'do',
      p: [],
      debugData: undefined,
    }

    let tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
    while (!isToken(tkn, { type: TokenType.Bracket, value: ')' })) {
      let bodyNode: AstNode
      ;[position, bodyNode] = parseToken(tokenStream, position)
      node.p.push(bodyNode)
      tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
    }

    node.debugData = firstToken.debugData
      ? {
          token: firstToken,
          lastToken: tkn,
        }
      : undefined

    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const newContext: Context = {}

    const newContextStack = contextStack.create(newContext)
    let result: Any = null
    for (const form of node.p)
      result = evaluateAstNode(form, newContextStack)

    return result
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
