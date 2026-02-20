import type { Any } from '../../interface'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type IfNode = SpecialExpressionNode<[typeof specialExpressionTypes['if'], [Node, Node, Node?]]>

const docs: CustomDocs = {
  category: 'Special-Expression',
  customVariants: ['if test then true-expr else false-expr', 'if test then true-expr'],
  details: [
    ['test', 'expression', 'The condition to test.'],
    ['true-expr', 'expression', 'The expression to evaluate if the test is truthy.'],
    ['false-expr', 'expression', 'The expression to evaluate if the test is falsy.'],
  ],
  description: 'Either `true-expr` or `false-expr` branch is taken. `true-expr` is selected when $test is truthy. If $test is falsy `false-expr` is executed, if no `false-expr` exists, `null` is returned.',
  examples: [
    `
if true then
  write!("TRUE")
else
  write!("FALSE")
end`,
    'if false then write!("TRUE") else write!("FALSE") end',
    'if true then write!("TRUE") end',
    'if false then write!("TRUE") end',
  ],
}

export const ifSpecialExpression: BuiltinSpecialExpression<Any, IfNode> = {
  arity: { min: 2, max: 3 },
  docs,
  evaluate: (node, contextStack, { evaluateNode }) => {
    const [conditionNode, trueNode, falseNode] = node[1][1]
    if (evaluateNode(conditionNode, contextStack)) {
      return evaluateNode(trueNode, contextStack)
    }
    else if (falseNode) {
      return evaluateNode(falseNode, contextStack)
    }
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) =>
    getUndefinedSymbols(node[1][1].filter(n => !!n), contextStack, builtin, evaluateNode),
}
