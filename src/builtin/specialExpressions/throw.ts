import { UserDefinedError } from '../../errors'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import { asString } from '../../typeGuards/string'
import { toFixedArity } from '../../utils/arity'
import type { BuiltinSpecialExpression, FunctionDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type ThrowNode = SpecialExpressionNode<[typeof specialExpressionTypes['throw'], Node]>

const docs: FunctionDocs = {
  category: 'Special expression',
  returns: {
    type: 'never',
  },
  args: {
    expr: {
      type: 'any',
    },
  },
  variants: [
    { argumentNames: ['expr'] },
  ],
  description: 'Throws `UserDefinedError` with message set to $expr evaluated. $expr must evaluate to a string.',
  examples: [
    'try throw("You shall not pass!") catch(error) "Error: " ++ error.message end',
    'try throw(slice("You shall not pass!", 0, 3)) catch(error) "Error: " ++ error.message end',
  ],
}

export const throwSpecialExpression: BuiltinSpecialExpression<null, ThrowNode> = {
  arity: toFixedArity(1),
  docs,
  evaluate: (node, contextStack, { evaluateNode }) => {
    const message = asString(evaluateNode(node[1][1], contextStack), node[2], {
      nonEmpty: true,
    })
    throw new UserDefinedError(message, node[2])
  },
  evaluateAsNormalExpression: (params, sourceCodeInfo) => {
    const message = asString(params[0], sourceCodeInfo, {
      nonEmpty: true,
    })
    throw new UserDefinedError(message, undefined)
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols([node[1][1]], contextStack, builtin, evaluateNode),
}
