import { AstNodeType } from '../../constants/constants'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { asToken, isToken } from '../../typeGuards/token'
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

    let tkn = asToken(tokenStream.tokens[parseState.position], tokenStream.filePath)
    while (!isToken(tkn, { type: 'Bracket', value: ')' })) {
      node.p.push(parseToken(tokenStream, parseState))
      tkn = asToken(tokenStream.tokens[parseState.position], tokenStream.filePath)
    }
    parseState.position += 1

    node.debugData = firstToken.debugData
      ? {
          token: firstToken,
          lastToken: tkn,
        }
      : undefined

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
