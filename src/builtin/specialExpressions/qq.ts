import { AstNodeType, TokenType } from '../../constants/constants'
import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { assertNumberOfParams } from '../../typeGuards'
import { isNameNode } from '../../typeGuards/astNode'
import { assertAny } from '../../typeGuards/lits'
import { asToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'

export interface QqNode extends CommonSpecialExpressionNode<'??'> {}

export const qqSpecialExpression: BuiltinSpecialExpression<Any, QqNode> = {
  parse: (tokenStream, position, firstToken, { parseTokensUntilClosingBracket }) => {
    const [newPosition, params] = parseTokensUntilClosingBracket(tokenStream, position)
    const lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' })

    const node: QqNode = {
      t: AstNodeType.SpecialExpression,
      n: '??',
      p: params,
      debugData: firstToken.debugData && {
        token: firstToken,
        lastToken,
      },
    }

    assertNumberOfParams({ min: 1, max: 2 }, node)

    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const [firstNode, secondNode] = node.p

    if (isNameNode(firstNode)) {
      if (contextStack.lookUp(firstNode) === null)
        return secondNode ? evaluateAstNode(secondNode, contextStack) : null
    }
    assertAny(firstNode, node.debugData?.token.debugData?.sourceCodeInfo)
    const firstResult = evaluateAstNode(firstNode, contextStack)
    return firstResult ?? (secondNode ? evaluateAstNode(secondNode, contextStack) : null)
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
