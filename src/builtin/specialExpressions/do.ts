import { AstNodeType } from '../../constants/constants'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { isRParenToken } from '../../tokenizer/common/commonTokens'
import { asToken } from '../../tokenizer/tokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import type { BuiltinSpecialExpression } from '../interface'

export interface DoNode extends CommonSpecialExpressionNode<'do'> {}

export const doSpecialExpression: BuiltinSpecialExpression<Any, DoNode> = {
  parse: (tokenStream, parseState, firstToken, { parseToken }) => {
    const node: DoNode = {
      t: AstNodeType.SpecialExpression,
      n: 'do',
      p: [],
      debugData: undefined,
    }

    let tkn = asToken(tokenStream.tokens[parseState.position])
    while (!isRParenToken(tkn)) {
      node.p.push(parseToken(tokenStream, parseState))
      tkn = asToken(tokenStream.tokens[parseState.position])
    }
    parseState.position += 1

    node.debugData = getTokenDebugData(firstToken) && {
      token: firstToken,
    }

    return node
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const newContext: Context = {}

    const newContextStack = contextStack.create(newContext)
    let result: Any = null
    for (const form of node.p)
      result = evaluateAstNode(form, newContextStack)

    return result
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
