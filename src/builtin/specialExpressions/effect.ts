import type { EffectRef, SpecialExpressionNode } from '../../parser/types'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type EffectNode = SpecialExpressionNode<[typeof specialExpressionTypes['effect'], string]>

const docs: CustomDocs = {
  category: 'special-expression',
  customVariants: [
    'effect(name)',
  ],
  details: [
    ['name', 'dotted identifier', 'The effect name, e.g. `llm.complete` or `lits.log`.'],
  ],
  description: 'Returns the unique effect reference for the given name. '
    + 'Calling `effect` with the same name always returns the same reference. '
    + 'Effect references are first-class values that can be stored, passed, and compared with `==`.',
  examples: [
    'effect(lits.log)',
    '==(effect(llm.complete), effect(llm.complete))',
  ],
}

export const effectSpecialExpression: BuiltinSpecialExpression<EffectRef, EffectNode> = {
  arity: {},
  docs,
  getUndefinedSymbols: () => new Set<string>(),
}
