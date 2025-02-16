import { AstNodeType } from '../../constants/constants'
import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { asRParenToken, getTokenDebugData } from '../../tokenizer/Token'
import { assertNumberOfParams } from '../../typeGuards'
import type { BuiltinSpecialExpression } from '../interface'

export interface TimeNode extends CommonSpecialExpressionNode<'time!'> {}

export const timeSpecialExpression: BuiltinSpecialExpression<Any, TimeNode> = {
  parse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    const lastToken = asRParenToken(tokenStream.tokens[parseState.position++])

    const node: TimeNode = {
      t: AstNodeType.SpecialExpression,
      n: 'time!',
      p: params,
      debugData: getTokenDebugData(firstToken) && {
        token: firstToken,
        lastToken,
      },
    }

    assertNumberOfParams(1, node)

    return node
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const param = node.p[0]!
    const startTime = Date.now()
    const result = evaluateAstNode(param, contextStack)
    const totalTime = Date.now() - startTime
    // eslint-disable-next-line no-console
    console.log(`Elapsed time: ${totalTime} ms`)

    return result
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
