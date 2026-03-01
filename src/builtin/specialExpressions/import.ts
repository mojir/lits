import type { Any } from '../../interface'
import type { SpecialExpressionNode } from '../../parser/types'
import { toFixedArity } from '../../utils/arity'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type ImportNode = SpecialExpressionNode<[typeof specialExpressionTypes['import'], string]>

const docs: CustomDocs = {
  category: 'special-expression',
  description: 'Imports all functions from a module as an object. Use destructuring to pick specific functions.',
  customVariants: [
    'import(module-name)',
  ],
  returns: { type: 'object' },
  examples: [
    'let v = import(vector); v.stdev([1, 2, 3, 4])',
    'let { linspace } = import(vector); linspace(0, 10, 5)',
    'let g = import(grid); g.row([[1, 2], [3, 4]], 0)',
  ],
}

export const importSpecialExpression: BuiltinSpecialExpression<Any, ImportNode> = {
  arity: toFixedArity(1),
  docs,
  getUndefinedSymbols: () => new Set(),
}
