import { joinAnalyzeResults } from '../../analyze/utils'
import { LitsError } from '../../errors'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import { AstNodeType } from '../../constants/constants'
import type { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { asAstNode } from '../../typeGuards/astNode'
import { valueToString } from '../../utils/debug/debugTools'
import { asToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'
import { asNonUndefined, assertNumberOfParams } from '../../typeGuards'

type IfLetNode = SpecialExpressionNode & {
  b: BindingNode
}

export const ifLetSpecialExpression: BuiltinSpecialExpression<Any> = {
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

    const node: IfLetNode = {
      t: AstNodeType.SpecialExpression,
      n: 'if-let',
      b: asNonUndefined(bindings[0], firstToken.sourceCodeInfo),
      p: params,
      tkn: firstToken.sourceCodeInfo ? firstToken : undefined,
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const sourceCodeInfo = node.tkn?.sourceCodeInfo
    const locals: Context = {}
    const bindingValue = evaluateAstNode((node as IfLetNode).b.v, contextStack)
    if (bindingValue) {
      locals[(node as IfLetNode).b.n] = { value: bindingValue }
      const newContextStack = contextStack.create(locals)
      const thenForm = asAstNode(node.p[0], sourceCodeInfo)
      return evaluateAstNode(thenForm, newContextStack)
    }
    if (node.p.length === 2) {
      const elseForm = asAstNode(node.p[1], sourceCodeInfo)
      return evaluateAstNode(elseForm, contextStack)
    }
    return null
  },
  validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  analyze: (node, contextStack, { analyzeAst, builtin }) => {
    const newContext: Context = { [(node as IfLetNode).b.n]: { value: true } }
    const bindingResult = analyzeAst((node as IfLetNode).b.v, contextStack, builtin)
    const paramsResult = analyzeAst(node.p, contextStack.create(newContext), builtin)
    return joinAnalyzeResults(bindingResult, paramsResult)
  },
}
