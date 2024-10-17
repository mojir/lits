import { joinAnalyzeResults } from '../../analyze/utils'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import { AstNodeType } from '../../constants/constants'
import type { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { asToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'

type LetNode = SpecialExpressionNode & {
  bs: BindingNode[]
}

export const letSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokenStream, position, { parseBindings, parseTokens }) => {
    const firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath)
    let bindings: BindingNode[]
    ;[position, bindings] = parseBindings(tokenStream, position)

    let params: AstNode[]
    ;[position, params] = parseTokens(tokenStream, position)

    const node: LetNode = {
      t: AstNodeType.SpecialExpression,
      n: 'let',
      p: params,
      bs: bindings,
      tkn: firstToken.sourceCodeInfo ? firstToken : undefined,
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const locals: Context = {}
    const newContextStack = contextStack.create(locals)
    for (const binding of (node as LetNode).bs) {
      const bindingValueNode = binding.v
      const bindingValue = evaluateAstNode(bindingValueNode, newContextStack)
      locals[binding.n] = { value: bindingValue }
    }

    let result: Any = null
    for (const astNode of node.p)
      result = evaluateAstNode(astNode, newContextStack)

    return result
  },
  analyze: (node, contextStack, { analyzeAst, builtin }) => {
    const newContext = (node as LetNode).bs
      .map(binding => binding.n)
      .reduce((context: Context, name) => {
        context[name] = { value: true }
        return context
      }, {})
    const bindingContext: Context = {}
    const bindingResults = (node as LetNode).bs.map((bindingNode) => {
      const valueNode = bindingNode.v
      const bindingsResult = analyzeAst(valueNode, contextStack.create(bindingContext), builtin)
      bindingContext[bindingNode.n] = { value: true }
      return bindingsResult
    })

    const paramsResult = analyzeAst(node.p, contextStack.create(newContext), builtin)
    return joinAnalyzeResults(...bindingResults, paramsResult)
  },
}
