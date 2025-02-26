import { joinAnalyzeResults } from '../../analyze/utils'
import { AstNodeType } from '../../constants/constants'
import { LitsError } from '../../errors'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { BindingNode, CommonSpecialExpressionNode } from '../../parser/interface'
import { assertRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { asNonUndefined, assertNumberOfParams } from '../../typeGuards'
import { asAstNode } from '../../typeGuards/astNode'
import { valueToString } from '../../utils/debug/debugTools'
import type { BuiltinSpecialExpression } from '../interface'

export interface IfLetNode extends CommonSpecialExpressionNode<'if-let'> {
  b: BindingNode
}

export const ifLetSpecialExpression: BuiltinSpecialExpression<Any, IfLetNode> = {
  polishParse: (tokenStream, parseState, firstToken, { parseBindings, parseTokensUntilClosingBracket }) => {
    const bindings = parseBindings(tokenStream, parseState)

    if (bindings.length !== 1) {
      throw new LitsError(
        `Expected exactly one binding, got ${valueToString(bindings.length)}`,
        getTokenDebugData(firstToken)?.sourceCodeInfo,
      )
    }

    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: IfLetNode = {
      t: AstNodeType.SpecialExpression,
      n: 'if-let',
      b: asNonUndefined(bindings[0], getTokenDebugData(firstToken)?.sourceCodeInfo),
      p: params,
      token: getTokenDebugData(firstToken) && firstToken,
    }

    return node
  },
  validateParameterCount: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const sourceCodeInfo = getTokenDebugData(node.token)?.sourceCodeInfo
    const locals: Context = {}
    const bindingValue = evaluateAstNode(node.b.v, contextStack)
    if (bindingValue) {
      locals[node.b.n] = { value: bindingValue }
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
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => {
    const newContext: Context = { [node.b.n]: { value: true } }
    const bindingResult = findUnresolvedIdentifiers([node.b.v], contextStack, builtin)
    const paramsResult = findUnresolvedIdentifiers(node.p, contextStack.create(newContext), builtin)
    return joinAnalyzeResults(bindingResult, paramsResult)
  },
}
