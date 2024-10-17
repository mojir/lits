import { AstNodeType, TokenType } from '../../constants/constants'
import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { assertNumberOfParams } from '../../typeGuards'
import { asAstNode } from '../../typeGuards/astNode'
import { asToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'

export interface IfNode extends CommonSpecialExpressionNode<'if'> {}

export const ifSpecialExpression: BuiltinSpecialExpression<Any, IfNode> = {
  parse: (tokenStream, position, firstToken, { parseTokensUntilClosingBracket }) => {
    const [newPosition, params] = parseTokensUntilClosingBracket(tokenStream, position)
    const lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' })

    const node: IfNode = {
      t: AstNodeType.SpecialExpression,
      n: 'if',
      p: params,
      debugData: firstToken.debugData && {
        token: firstToken,
        lastToken,
      },
    }

    assertNumberOfParams({ min: 2, max: 3 }, node)

    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const sourceCodeInfo = node.debugData?.token.debugData?.sourceCodeInfo

    const [conditionNode, trueNode, falseNode] = node.p
    if (evaluateAstNode(asAstNode(conditionNode, sourceCodeInfo), contextStack)) {
      return evaluateAstNode(asAstNode(trueNode, sourceCodeInfo), contextStack)
    }
    else {
      if (node.p.length === 3)
        return evaluateAstNode(asAstNode(falseNode, sourceCodeInfo), contextStack)
      else
        return null
    }
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
