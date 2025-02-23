import { AstNodeType } from '../../constants/constants'
import type { CommonSpecialExpressionNode, SymbolNode } from '../../parser/interface'
import { asRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertNumberOfParams } from '../../typeGuards'
import { asAstNode, asNameNode, assertNameNode } from '../../typeGuards/astNode'
import type { BuiltinSpecialExpression } from '../interface'
import { assertNameNotDefined } from '../utils'

export interface DefNode extends CommonSpecialExpressionNode<'def'> {}

export const defSpecialExpression: BuiltinSpecialExpression<null, DefNode> = {
  parse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    const lastToken = asRParenToken(tokenStream.tokens[parseState.position++])

    const node: DefNode = {
      t: AstNodeType.SpecialExpression,
      n: 'def',
      p: params,
      debugData: getTokenDebugData(firstToken) && {
        token: firstToken,
        lastToken,
      },
    }

    assertNameNode(node.p[0], getTokenDebugData(node.debugData?.token)?.sourceCodeInfo)
    assertNumberOfParams(2, node)

    return node
  },
  evaluate: (node, contextStack, { evaluateAstNode, builtin }) => {
    const sourceCodeInfo = getTokenDebugData(node.debugData?.token)?.sourceCodeInfo
    const name = (node.p[0] as SymbolNode).v

    assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo)

    contextStack.globalContext[name] = {
      value: evaluateAstNode(node.p[1]!, contextStack),
    }

    return null
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => {
    const sourceCodeInfo = getTokenDebugData(node.debugData?.token)?.sourceCodeInfo
    const subNode = asAstNode(node.p[1])
    const result = findUnresolvedIdentifiers([subNode], contextStack, builtin)
    const name = asNameNode(node.p[0]).v
    assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo)
    contextStack.globalContext[name] = { value: true }
    return result
  },
}
