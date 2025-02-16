import { AstNodeType } from '../../constants/constants'
import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { asRParenToken, getTokenDebugData } from '../../tokenizer/Token'
import { assertNumberOfParams } from '../../typeGuards'
import { asAstNode } from '../../typeGuards/astNode'
import type { BuiltinSpecialExpression } from '../interface'

export interface IfNode extends CommonSpecialExpressionNode<'if'> {}

export const ifSpecialExpression: BuiltinSpecialExpression<Any, IfNode> = {
  parse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    const lastToken = asRParenToken(tokenStream.tokens[parseState.position++])

    const node: IfNode = {
      t: AstNodeType.SpecialExpression,
      n: 'if',
      p: params,
      debugData: getTokenDebugData(firstToken) && {
        token: firstToken,
        lastToken,
      },
    }

    assertNumberOfParams({ min: 2, max: 3 }, node)

    return node
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const sourceCodeInfo = getTokenDebugData(node.debugData?.token)?.sourceCodeInfo

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
