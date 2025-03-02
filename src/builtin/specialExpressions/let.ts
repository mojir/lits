import { joinAnalyzeResults } from '../../analyze/utils'
import { AstNodeType } from '../../constants/constants'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { BindingNode, CommonSpecialExpressionNode } from '../../parser/interface'
import { assertRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import type { BuiltinSpecialExpression } from '../interface'

export interface LetNode extends CommonSpecialExpressionNode<'let'> {
  bs: BindingNode[]
}

export const letSpecialExpression: BuiltinSpecialExpression<Any, LetNode> = {
  polishParse: (tokenStream, parseState, firstToken, { parseBindings, parseTokensUntilClosingBracket }) => {
    const bindings = parseBindings(tokenStream, parseState)

    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: LetNode = {
      t: AstNodeType.SpecialExpression,
      n: 'let',
      p: params,
      bs: bindings,
      token: getTokenDebugData(firstToken) && firstToken,
    }
    return node
  },
  validateParameterCount: () => undefined,
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const locals: Context = {}
    const newContextStack = contextStack.create(locals)
    for (const binding of node.bs) {
      const bindingValueNode = binding.v
      const bindingValue = evaluateAstNode(bindingValueNode, newContextStack)
      locals[binding.n] = { value: bindingValue }
    }

    let result: Any = null
    for (const astNode of node.p)
      result = evaluateAstNode(astNode, newContextStack)

    return result
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => {
    const newContext = node.bs
      .map(binding => binding.n)
      .reduce((context: Context, name) => {
        context[name] = { value: true }
        return context
      }, {})
    const bindingContext: Context = {}
    const bindingResults = node.bs.map((bindingNode) => {
      const valueNode = bindingNode.v
      const bindingsResult = findUnresolvedIdentifiers([valueNode], contextStack.create(bindingContext), builtin)
      bindingContext[bindingNode.n] = { value: true }
      return bindingsResult
    })

    const paramsResult = findUnresolvedIdentifiers(node.p, contextStack.create(newContext), builtin)
    return joinAnalyzeResults(...bindingResults, paramsResult)
  },
}
