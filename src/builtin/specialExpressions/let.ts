import { joinAnalyzeResults } from '../../analyze/utils'
import { AstNodeType } from '../../constants/constants'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { BindingNode, CommonSpecialExpressionNode } from '../../parser/interface'
import { assertRParenToken } from '../../tokenizer/tokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import type { BuiltinSpecialExpression } from '../interface'

export interface LetNode extends CommonSpecialExpressionNode<'let'> {
  bs: BindingNode[]
}

export const letSpecialExpression: BuiltinSpecialExpression<Any, LetNode> = {
  polishParse: (tokenStream, parseState, firstToken, { parseBindings }) => {
    const bindings = parseBindings(tokenStream, parseState)

    // const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: LetNode = {
      t: AstNodeType.SpecialExpression,
      n: 'let',
      p: [],
      bs: bindings,
      token: getTokenDebugData(firstToken) && firstToken,
    }
    return node
  },
  paramCount: 0,
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    for (const binding of node.bs) {
      const bindingValueNode = binding.v
      const bindingValue = evaluateAstNode(bindingValueNode, contextStack)
      contextStack.addValue(binding.n, bindingValue)
    }
    return null
  },
  findUnresolvedSymbols: (node, contextStack, { findUnresolvedSymbols, builtin }) => {
    const newContext = node.bs
      .map(binding => binding.n)
      .reduce((context: Context, name) => {
        context[name] = { value: true }
        return context
      }, {})
    const bindingResults = node.bs.map((bindingNode) => {
      const valueNode = bindingNode.v
      const bindingsResult = findUnresolvedSymbols([valueNode], contextStack, builtin)
      contextStack.addValue(bindingNode.n, { value: true })
      return bindingsResult
    })

    const paramsResult = findUnresolvedSymbols(node.p, contextStack.create(newContext), builtin)
    return joinAnalyzeResults(...bindingResults, paramsResult)
  },
}
