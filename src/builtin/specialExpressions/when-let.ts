import { joinAnalyzeResults } from '../../analyze/utils'
import { AstNodeType } from '../../constants/constants'
import { LitsError } from '../../errors'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { BindingNode, CommonSpecialExpressionNode } from '../../parser/interface'
import { asRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { asNonUndefined } from '../../typeGuards'
import { valueToString } from '../../utils/debug/debugTools'
import type { BuiltinSpecialExpression } from '../interface'

export interface WhenLetNode extends CommonSpecialExpressionNode<'when-let'> {
  b: BindingNode
}

export const whenLetSpecialExpression: BuiltinSpecialExpression<Any, WhenLetNode> = {
  parse: (tokenStream, parseState, firstToken, { parseBindings, parseTokensUntilClosingBracket }) => {
    const bindings = parseBindings(tokenStream, parseState)

    if (bindings.length !== 1) {
      throw new LitsError(
        `Expected exactly one binding, got ${valueToString(bindings.length)}`,
        getTokenDebugData(firstToken)?.sourceCodeInfo,
      )
    }

    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    const lastToken = asRParenToken(tokenStream.tokens[parseState.position++])

    const node: WhenLetNode = {
      t: AstNodeType.SpecialExpression,
      n: 'when-let',
      b: asNonUndefined(bindings[0], getTokenDebugData(firstToken)?.sourceCodeInfo),
      p: params,
      debugData: getTokenDebugData(firstToken) && {
        token: firstToken,
        lastToken,
      },
    }
    return node
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const { b: binding } = node
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
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => {
    const { b: binding } = node
    const newContext: Context = { [binding.n]: { value: true } }
    const bindingResult = findUnresolvedIdentifiers([binding.v], contextStack, builtin)
    const paramsResult = findUnresolvedIdentifiers(node.p, contextStack.create(newContext), builtin)
    return joinAnalyzeResults(bindingResult, paramsResult)
  },
}
