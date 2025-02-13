import { AstNodeType } from '../../constants/constants'
import { UserDefinedError } from '../../errors'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { assertNumberOfParams } from '../../typeGuards'
import { asString } from '../../typeGuards/string'
import { asToken, assertToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'

export interface ThrowNode extends CommonSpecialExpressionNode<'throw'> {}

export const throwSpecialExpression: BuiltinSpecialExpression<null, ThrowNode> = {
  parse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
    const params = parseTokensUntilClosingBracket(tokenStream, parseState)

    assertToken(tokenStream.tokens[parseState.position], tokenStream.filePath, { type: 'Bracket', value: ')' })
    const lastToken = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath, { type: 'Bracket', value: ')' })

    const node: ThrowNode = {
      t: AstNodeType.SpecialExpression,
      n: 'throw',
      p: params,
      debugData: firstToken.debugData && {
        token: firstToken,
        lastToken,
      },
    }

    assertNumberOfParams(1, node)

    return node
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const message = asString(evaluateAstNode(node.p[0]!, contextStack), node.debugData?.token.debugData?.sourceCodeInfo, {
      nonEmpty: true,
    })
    throw new UserDefinedError(message, node.debugData?.token.debugData?.sourceCodeInfo)
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
