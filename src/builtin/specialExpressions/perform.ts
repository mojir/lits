import type { Any } from '../../interface'
import type { AstNode, SpecialExpressionNode } from '../../parser/types'
import { joinSets } from '../../utils'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type PerformNode = SpecialExpressionNode<[typeof specialExpressionTypes['perform'], AstNode, AstNode[]]>

const docs: CustomDocs = {
  category: 'special-expression',
  customVariants: [
    'perform(eff)',
    'perform(eff, arg1)',
    'perform(eff, arg1, arg2, ...)',
  ],
  details: [
    ['eff', 'expression', 'An expression evaluating to an effect value (from `effect(name)`).'],
    ['arg1, arg2, ...', 'expressions', 'Arguments passed to the effect handler.'],
  ],
  description: 'Invokes an effect. The nearest enclosing `try/with` handler matching the effect '
    + 'intercepts the call. The handler receives the arguments as an array and its return value '
    + 'becomes the result of `perform`. If no local handler matches, the effect is dispatched '
    + 'to the host.',
  examples: [
    `
try
  perform(effect(lits.log), "hello")
with
  case effect(lits.log) then ([msg]) -> msg
end
`,
  ],
}

export const performSpecialExpression: BuiltinSpecialExpression<Any, PerformNode> = {
  arity: { min: 1 },
  docs,
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const [, effectExpr, argExprs] = node[1]
    const effectResult = getUndefinedSymbols([effectExpr], contextStack, builtin, evaluateNode)
    const argsResult = getUndefinedSymbols(argExprs, contextStack, builtin, evaluateNode)
    return joinSets(effectResult, argsResult)
  },
}
