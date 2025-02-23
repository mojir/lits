import { AstNodeType } from '../../constants/constants'
import type { CommonSpecialExpressionNode, SymbolNode } from '../../parser/interface'
import { assertRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertNumberOfParams } from '../../typeGuards'
import type { BuiltinSpecialExpression } from '../interface'

export interface DeclaredNode extends CommonSpecialExpressionNode<'declared?'> {}

export const declaredSpecialExpression: BuiltinSpecialExpression<boolean, DeclaredNode> = {
  parse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: DeclaredNode = {
      t: AstNodeType.SpecialExpression,
      n: 'declared?',
      p: params,
      token: getTokenDebugData(firstToken) && firstToken,
    }

    assertNumberOfParams(1, node)

    return node
  },
  evaluate: (node, contextStack) => {
    const lookUpResult = contextStack.lookUp(node.p[0] as SymbolNode)
    return lookUpResult !== null
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
