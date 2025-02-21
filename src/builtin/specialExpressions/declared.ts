import { AstNodeType } from '../../constants/constants'
import type { CommonSpecialExpressionNode, NameNode } from '../../parser/interface'
import { asRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertNumberOfParams } from '../../typeGuards'
import type { BuiltinSpecialExpression } from '../interface'

export interface DeclaredNode extends CommonSpecialExpressionNode<'declared?'> {}

export const declaredSpecialExpression: BuiltinSpecialExpression<boolean, DeclaredNode> = {
  parse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    const lastToken = asRParenToken(tokenStream.tokens[parseState.position++])

    const node: DeclaredNode = {
      t: AstNodeType.SpecialExpression,
      n: 'declared?',
      p: params,
      debugData: getTokenDebugData(firstToken) && {
        token: firstToken,
        lastToken,
      },
    }

    assertNumberOfParams(1, node)

    return node
  },
  evaluate: (node, contextStack) => {
    const lookUpResult = contextStack.lookUp(node.p[0] as NameNode)
    return lookUpResult !== null
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
