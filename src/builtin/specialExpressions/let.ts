import type { Any } from '../../interface'
import type { BindingNode, SpecialExpressionNode } from '../../parser/types'
import { addToSet } from '../../utils'
import { toFixedArity } from '../../utils/arity'
import { getAllBindingTargetNames, walkDefaults } from '../bindingNode'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type LetNode = SpecialExpressionNode<[typeof specialExpressionTypes['let'], BindingNode]>

const docs: CustomDocs = {
  category: 'special-expression',
  customVariants: ['let s = value;'],
  details: [
    ['s', 'symbol', 'The name of the variable to bind.'],
    ['value', 'any', 'The value to bind to the variable.'],
  ],
  description: `
  Binds local variables s to \`value\`. \`value\` can be any expression. The scope of the variables is the body of the let expression.`,
  examples: [`
let a = 1 + 2 + 3 + 4;
let b = -> $ * ( $ + 1 );
write!("a", a, "b", b)`],
}

export const letSpecialExpression: BuiltinSpecialExpression<Any, LetNode> = {
  arity: toFixedArity(0),
  docs,
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const bindingNode = node[1][1]
    const target = bindingNode[1][0]
    const value = bindingNode[1][1]
    const bindingResult = getUndefinedSymbols([value], contextStack, builtin, evaluateNode)
    walkDefaults(target, (defaultNode) => {
      addToSet(bindingResult, getUndefinedSymbols([defaultNode], contextStack, builtin, evaluateNode))
    })
    contextStack.addValues(getAllBindingTargetNames(target), target[2])
    return bindingResult
  },
}
