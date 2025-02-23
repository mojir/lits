import { joinAnalyzeResults } from '../../analyze/utils'
import { AstNodeType } from '../../constants/constants'
import { LitsError } from '../../errors'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { BindingNode, CommonSpecialExpressionNode } from '../../parser/interface'
import { assertRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { asNonUndefined } from '../../typeGuards'
import { isSeq } from '../../typeGuards/lits'
import { toAny } from '../../utils'
import { valueToString } from '../../utils/debug/debugTools'
import type { BuiltinSpecialExpression } from '../interface'

export interface WhenFirstNode extends CommonSpecialExpressionNode<'when-first'> {
  b: BindingNode
}

export const whenFirstSpecialExpression: BuiltinSpecialExpression<Any, WhenFirstNode> = {
  parse: (tokenStream, parseState, firstToken, { parseBindings, parseTokensUntilClosingBracket }) => {
    const bindings = parseBindings(tokenStream, parseState)

    if (bindings.length !== 1) {
      throw new LitsError(
        `Expected exactly one binding, got ${valueToString(bindings.length)}`,
        getTokenDebugData(firstToken)?.sourceCodeInfo,
      )
    }

    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: WhenFirstNode = {
      t: AstNodeType.SpecialExpression,
      n: 'when-first',
      b: asNonUndefined(bindings[0], getTokenDebugData(firstToken)?.sourceCodeInfo),
      p: params,
      debugData: getTokenDebugData(firstToken) && {
        token: firstToken,
      },
    }
    return node
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const locals: Context = {}
    const { b: binding } = node
    const evaluatedBindingForm = evaluateAstNode(binding.v, contextStack)
    if (!isSeq(evaluatedBindingForm)) {
      throw new LitsError(
        `Expected undefined or a sequence, got ${valueToString(evaluatedBindingForm)}`,
        getTokenDebugData(node.debugData?.token)?.sourceCodeInfo,
      )
    }

    if (evaluatedBindingForm.length === 0)
      return null

    const bindingValue = toAny(evaluatedBindingForm[0])
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
