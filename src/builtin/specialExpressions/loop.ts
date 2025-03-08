import { joinAnalyzeResults } from '../../analyze/utils'
import { AstNodeType } from '../../constants/constants'
import { LitsError, RecurSignal } from '../../errors'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { BindingNode, CommonSpecialExpressionNode } from '../../parser/interface'
import { assertRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { asNonUndefined } from '../../typeGuards'
import { asAny } from '../../typeGuards/lits'
import { valueToString } from '../../utils/debug/debugTools'
import type { BuiltinSpecialExpression } from '../interface'

export interface LoopNode extends CommonSpecialExpressionNode<'loop'> {
  bs: BindingNode[]
}

export const loopSpecialExpression: BuiltinSpecialExpression<Any, LoopNode> = {
  polishParse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket, parseBindings }) => {
    const bindings = parseBindings(tokenStream, parseState)

    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: LoopNode = {
      t: AstNodeType.SpecialExpression,
      n: 'loop',
      p: params,
      bs: bindings,
      token: getTokenDebugData(firstToken) && firstToken,
    }
    return node
  },
  paramCount: {},
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const sourceCodeInfo = getTokenDebugData(node.token)?.sourceCodeInfo
    const bindingContext: Context = node.bs.reduce((result: Context, binding) => {
      result[binding.n] = { value: evaluateAstNode(binding.v, contextStack) }
      return result
    }, {})
    const newContextStack = contextStack.create(bindingContext)

    for (;;) {
      let result: Any = null
      try {
        for (const form of node.p)
          result = evaluateAstNode(form, newContextStack)
      }
      catch (error) {
        if (error instanceof RecurSignal) {
          const params = error.params
          if (params.length !== node.bs.length) {
            throw new LitsError(
              `recur expected ${node.bs.length} parameters, got ${valueToString(params.length)}`,
              sourceCodeInfo,
            )
          }
          ;node.bs.forEach((binding, index) => {
            asNonUndefined(bindingContext[binding.n], sourceCodeInfo).value = asAny(params[index], sourceCodeInfo)
          })
          continue
        }
        throw error
      }
      return result
    }
  },
  findUnresolvedSymbols: (node, contextStack, { findUnresolvedSymbols, builtin }) => {
    const newContext = node.bs
      .map(binding => binding.n)
      .reduce((context: Context, name) => {
        context[name] = { value: true }
        return context
      }, {})

    const bindingValueNodes = node.bs.map(binding => binding.v)
    const bindingsResult = findUnresolvedSymbols(bindingValueNodes, contextStack, builtin)
    const paramsResult = findUnresolvedSymbols(node.p, contextStack.create(newContext), builtin)
    return joinAnalyzeResults(bindingsResult, paramsResult)
  },
}
