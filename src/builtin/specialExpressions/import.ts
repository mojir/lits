import { LitsError } from '../../errors'
import type { Any } from '../../interface'
import type { ModuleFunction, SpecialExpressionNode } from '../../parser/types'
import { toFixedArity } from '../../utils/arity'
import { FUNCTION_SYMBOL } from '../../utils/symbols'
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
  evaluate: (node, contextStack) => {
    const moduleName = node[1][1]
    const sourceCodeInfo = node[2]

    // Check for value modules first (file modules from bundles)
    const valueModule = contextStack.getValueModule(moduleName)
    if (valueModule.found) {
      return valueModule.value as Any
    }

    // Fall back to builtin modules
    const module = contextStack.getModule(moduleName)
    if (!module) {
      throw new LitsError(`Unknown module: '${moduleName}'`, sourceCodeInfo)
    }

    // Create an object where each key is a function name and value is a ModuleFunction
    const result: Record<string, ModuleFunction> = {}
    for (const [functionName, expression] of Object.entries(module.functions)) {
      result[functionName] = {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Module',
        moduleName,
        functionName,
        arity: expression.arity,
      }
    }
    return result
  },
  getUndefinedSymbols: () => new Set(),
}
