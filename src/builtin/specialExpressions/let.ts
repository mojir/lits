import { joinAnalyzeResults } from '../../analyze/utils'
import { AstNodeType } from '../../constants/constants'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { BindingNode, CommonSpecialExpressionNode, NormalExpressionNode } from '../../parser/interface'
import { asRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { asNormalExpressionNode } from '../../typeGuards/astNode'
import type { BuiltinSpecialExpression } from '../interface'

export interface LetNode extends CommonSpecialExpressionNode<'let'> {
  bs: BindingNode[]
  debugData: CommonSpecialExpressionNode<'let'>['debugData'] & ({
    bindingArray: NormalExpressionNode
  } | undefined)
}

export const letSpecialExpression: BuiltinSpecialExpression<Any, LetNode> = {
  parse: (tokenStream, parseState, firstToken, { parseBindings, parseTokensUntilClosingBracket, parseToken }) => {
    const bindingArray = getTokenDebugData(firstToken)?.sourceCodeInfo && asNormalExpressionNode(parseToken(tokenStream, { ...parseState }))

    const bindings = parseBindings(tokenStream, parseState)

    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    const lastToken = asRParenToken(tokenStream.tokens[parseState.position++])

    const node: LetNode = {
      t: AstNodeType.SpecialExpression,
      n: 'let',
      p: params,
      bs: bindings,
      debugData: getTokenDebugData(firstToken)?.sourceCodeInfo && bindingArray && {
        token: firstToken,
        lastToken,
        bindingArray,
      },
    }
    return node
  },
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
