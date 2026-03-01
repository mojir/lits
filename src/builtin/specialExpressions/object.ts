import type { Any, Obj } from '../../interface'
import type { AstNode, SpecialExpressionNode } from '../../parser/types'
import { assertString } from '../../typeGuards/string'
import type { BuiltinSpecialExpression, FunctionDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type ObjectNode = SpecialExpressionNode<[typeof specialExpressionTypes['object'], AstNode[]]>

const docs: FunctionDocs = {
  category: 'special-expression',
  returns: {
    type: 'object',
  },
  args: {
    kvps: {
      type: 'any',
      rest: true,
      description: 'key - value pairs, where key is a string',
    },
  },
  variants: [
    { argumentNames: ['kvps'] },
  ],
  description: 'Constructs a new object. Object members are created from the $kvps key-value pairs. Requires an even number of arguments.',
  examples: [
    'object()',
    `
let default = {
  type: "Person",
  name: "John Doe",
  age: 42
};

{
  ...default,
  name: "Lisa"
}`,
    'object("x", 10, "y", true, "z", "A string")',
    '{}',
    '{ a: 1, b: 2 }',
  ],
  hideOperatorForm: true,
}

export const objectSpecialExpression: BuiltinSpecialExpression<Any, ObjectNode> = {
  arity: {},
  docs,
  evaluateAsNormalExpression: (params, sourceCodeInfo) => {
    const result: Obj = {}

    for (let i = 0; i < params.length; i += 2) {
      const key = params[i]
      const value = params[i + 1]
      assertString(key, sourceCodeInfo)
      result[key] = value ?? null
    }

    return result
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode),
}
