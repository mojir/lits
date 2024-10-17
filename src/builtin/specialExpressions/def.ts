import { AstNodeType, TokenType } from '../../constants/constants'
import type { CommonSpecialExpressionNode, NameNode } from '../../parser/interface'
import { assertNumberOfParams } from '../../typeGuards'
import { asAstNode, asNameNode, assertNameNode } from '../../typeGuards/astNode'
import { asToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'
import { assertNameNotDefined } from '../utils'

export interface DefNode extends CommonSpecialExpressionNode<'def'> {}

export const defSpecialExpression: BuiltinSpecialExpression<null, DefNode> = {
  parse: (tokenStream, position, firstToken, { parseTokensUntilClosingBracket }) => {
    const [newPosition, params] = parseTokensUntilClosingBracket(tokenStream, position)
    const lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' })

    const node: DefNode = {
      t: AstNodeType.SpecialExpression,
      n: 'def',
      p: params,
      debugData: firstToken.debugData && {
        token: firstToken,
        lastToken,
      },
    }

    assertNameNode(node.p[0], node.debugData?.token.debugData?.sourceCodeInfo)
    assertNumberOfParams(2, node)

    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode, builtin }) => {
    const sourceCodeInfo = node.debugData?.token.debugData?.sourceCodeInfo
    const name = (node.p[0] as NameNode).v

    assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo)

    contextStack.globalContext[name] = {
      value: evaluateAstNode(node.p[1]!, contextStack),
    }

    return null
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => {
    const sourceCodeInfo = node.debugData?.token.debugData?.sourceCodeInfo
    const subNode = asAstNode(node.p[1])
    const result = findUnresolvedIdentifiers([subNode], contextStack, builtin)
    const name = asNameNode(node.p[0]).v
    assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo)
    contextStack.globalContext[name] = { value: true }
    return result
  },
}
