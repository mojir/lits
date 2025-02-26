import { AstNodeType } from '../../constants/constants'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { assertRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertNumberOfParams } from '../../typeGuards'
import { assertString } from '../../typeGuards/string'
import type { BuiltinSpecialExpression } from '../interface'
import { assertNameNotDefined } from '../utils'

export interface DefsNode extends CommonSpecialExpressionNode<'defs'> {}

export const defsSpecialExpression: BuiltinSpecialExpression<null, DefsNode> = {
  parse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: DefsNode = {
      t: AstNodeType.SpecialExpression,
      n: 'defs',
      p: params,
      token: getTokenDebugData(firstToken) && firstToken,
    }

    return node
  },
  validateParameterCount: node => assertNumberOfParams(2, node),
  evaluate: (node, contextStack, { evaluateAstNode, builtin }) => {
    const sourceCodeInfo = getTokenDebugData(node.token)?.sourceCodeInfo
    const name = evaluateAstNode(node.p[0]!, contextStack)
    assertString(name, sourceCodeInfo)

    assertNameNotDefined(name, contextStack, builtin, getTokenDebugData(node.token)?.sourceCodeInfo)

    contextStack.globalContext[name] = {
      value: evaluateAstNode(node.p[1]!, contextStack),
    }

    return null
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin, evaluateAstNode }) => {
    const sourceCodeInfo = getTokenDebugData(node.token)?.sourceCodeInfo
    const subNode = node.p[1]!
    const result = findUnresolvedIdentifiers([subNode], contextStack, builtin)
    const name = evaluateAstNode(node.p[0]!, contextStack)
    assertString(name, sourceCodeInfo)
    assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo)
    contextStack.globalContext[name] = { value: true }
    return result
  },
}
