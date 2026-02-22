import type { Any } from '../../interface'
import type { AstNode, SpecialExpressionNode } from '../../parser/types'
import type { MaybePromise } from '../../utils/maybePromise'
import { chain } from '../../utils/maybePromise'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type CondNode = SpecialExpressionNode<[typeof specialExpressionTypes['cond'], [AstNode, AstNode][]]>

const docs: CustomDocs = {
  category: 'special-expression',
  customVariants: ['cond cond-branch cond-branch ... end'],
  details: [
    ['cond-branch', 'case test then body', 'A branch of the cond expression.'],
    ['test', 'expression', 'The condition to test.'],
    ['body', 'expressions', 'The expressions to evaluate if the test is truthy.'],
  ],
  description: 'Used for branching. `cond-branches` are tested sequentially from the top. If no branch is tested truthy, `null` is returned.',
  examples: [
    `
cond
  case false then write!("FALSE")
  case true then write!("TRUE")
end`,
    `
cond
  case false then write!("FALSE")
  case null then write!("null")
end ?? write!("TRUE")`,
    `
cond
  case false then write!("FALSE")
  case null then write!("null")
end ?? write!("TRUE")`,
  ],
}

export const condSpecialExpression: BuiltinSpecialExpression<Any, CondNode> = {
  arity: {},
  docs,
  evaluate: (node, contextStack, { evaluateNode }) => {
    const params = node[1][1]
    function processCase(index: number): MaybePromise<Any> {
      if (index >= params.length)
        return null
      const [test, form] = params[index]!
      return chain(evaluateNode(test, contextStack), (value) => {
        if (!value)
          return processCase(index + 1)
        return evaluateNode(form, contextStack)
      })
    }
    return processCase(0)
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols(node[1][1].flat(), contextStack, builtin, evaluateNode),
}
