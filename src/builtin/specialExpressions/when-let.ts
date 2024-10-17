import { joinAnalyzeResults } from '../../analyze/utils'
import { LitsError } from '../../errors'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import { AstNodeType } from '../../constants/constants'
import type { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { valueToString } from '../../utils/debug/debugTools'
import { asToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'
import { asNonUndefined, assertNumberOfParams } from '../../typeGuards'

type WhenLetNode = SpecialExpressionNode & {
  b: BindingNode
}

export const whenLetSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokenStream, position, { parseBindings, parseTokens }) => {
    const firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath)
    let bindings: BindingNode[]
    ;[position, bindings] = parseBindings(tokenStream, position)

    if (bindings.length !== 1) {
      throw new LitsError(
        `Expected exactly one binding, got ${valueToString(bindings.length)}`,
        firstToken.sourceCodeInfo,
      )
    }

    let params: AstNode[]
    ;[position, params] = parseTokens(tokenStream, position)

    const node: WhenLetNode = {
      t: AstNodeType.SpecialExpression,
      n: 'when-let',
      b: asNonUndefined(bindings[0], firstToken.sourceCodeInfo),
      p: params,
      tkn: firstToken.sourceCodeInfo ? firstToken : undefined,
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const { b: binding } = node as WhenLetNode
    const locals: Context = {}
    const bindingValue = evaluateAstNode(binding.v, contextStack)
    if (!bindingValue)
      return null

    locals[binding.n] = { value: bindingValue }
    const newContextStack = contextStack.create(locals)

    let result: Any = null
    for (const form of node.p)
      result = evaluateAstNode(form, newContextStack)

    return result
  },
  validate: node => assertNumberOfParams({ min: 0 }, node),
  analyze: (node, contextStack, { analyzeAst, builtin }) => {
    const { b: binding } = node as WhenLetNode
    const newContext: Context = { [binding.n]: { value: true } }
    const bindingResult = analyzeAst(binding.v, contextStack, builtin)
    const paramsResult = analyzeAst(node.p, contextStack.create(newContext), builtin)
    return joinAnalyzeResults(bindingResult, paramsResult)
  },
}
