import type { SpecialExpressionNode, SymbolNode } from '../../parser/types'
import { toFixedArity } from '../../utils/arity'
import type { BuiltinSpecialExpression, FunctionDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type DefinedNode = SpecialExpressionNode<[typeof specialExpressionTypes['defined?'], SymbolNode]>

const docs: FunctionDocs = {
  category: 'special-expression',
  returns: {
    type: 'boolean',
  },
  args: {
    symbol: { type: 'any' },
  },
  variants: [
    { argumentNames: ['symbol'] },
  ],
  description: `Returns \`true\` if \`symbol\` is defined, \`false\` otherwise.

Built-in symbols are always considered defined. For user-defined symbols, checks if the symbol exists in the current scope.`,
  examples: [
    'let x = 42; defined?(x)',
    'defined?(x)',
    'defined?(+)',
  ],
}

export const definedSpecialExpression: BuiltinSpecialExpression<boolean, DefinedNode> = {
  arity: toFixedArity(1),
  docs,
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols([node[1][1]], contextStack, builtin, evaluateNode),
}
