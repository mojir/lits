import { joinAnalyzeResults } from '../../analyze/utils'
import { AstNodeType } from '../../constants/constants'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { BindingNode, CommonSpecialExpressionNode } from '../../parser/interface'
import { assertRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertNumberOfParams } from '../../typeGuards'
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
  validateParameterCount: node => assertNumberOfParams(0, node),
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    for (const binding of node.bs) {
      const bindingValueNode = binding.v
      const bindingValue = evaluateAstNode(bindingValueNode, contextStack)
      contextStack.addValue(binding.n, bindingValue)
    }
    return null
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => {
    const newContext = node.bs
      .map(binding => binding.n)
      .reduce((context: Context, name) => {
        context[name] = { value: true }
        return context
      }, {})
    const bindingResults = node.bs.map((bindingNode) => {
      const valueNode = bindingNode.v
      const bindingsResult = findUnresolvedIdentifiers([valueNode], contextStack, builtin)
      contextStack.addValue(bindingNode.n, { value: true })
      return bindingsResult
    })

    const paramsResult = findUnresolvedIdentifiers(node.p, contextStack.create(newContext), builtin)
    return joinAnalyzeResults(...bindingResults, paramsResult)
  },
}
