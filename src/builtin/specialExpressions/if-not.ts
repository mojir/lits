import { AstNodeType } from '../../constants/constants'
import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { assertRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertNumberOfParams } from '../../typeGuards'
import { asAstNode } from '../../typeGuards/astNode'
import type { BuiltinSpecialExpression } from '../interface'

export interface IfNotNode extends CommonSpecialExpressionNode<'if-not'> {}

export const ifNotSpecialExpression: BuiltinSpecialExpression<Any, IfNotNode> = {
  parse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: IfNotNode = {
      t: AstNodeType.SpecialExpression,
      n: 'if-not',
      p: params,
      token: getTokenDebugData(firstToken) && firstToken,
    }

    assertNumberOfParams({ min: 2, max: 3 }, node)

    return node
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const sourceCodeInfo = getTokenDebugData(node.token)?.sourceCodeInfo

    const [conditionNode, trueNode, falseNode] = node.p
    if (!evaluateAstNode(asAstNode(conditionNode, sourceCodeInfo), contextStack)) {
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
