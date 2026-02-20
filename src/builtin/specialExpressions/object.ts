import { LitsError } from '../../errors'
import type { Any, Obj } from '../../interface'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import { isUnknownRecord } from '../../typeGuards'
import { isSpreadNode } from '../../typeGuards/astNode'
import { assertString } from '../../typeGuards/string'
import type { BuiltinSpecialExpression, FunctionDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type ObjectNode = SpecialExpressionNode<[typeof specialExpressionTypes['object'], Node[]]>

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
  evaluate: (node, contextStack, { evaluateNode }) => {
    const result: Obj = {}

    const params = node[1][1]
    for (let i = 0; i < params.length; i += 2) {
      const keyNode = params[i]!
      if (isSpreadNode(keyNode)) {
        const spreadObject = evaluateNode(keyNode[1], contextStack)
        if (!isUnknownRecord(spreadObject)) {
          throw new LitsError('Spread value is not an object', keyNode[2])
        }
        Object.assign(result, spreadObject)
        i -= 1
      }
      else {
        const key = evaluateNode(keyNode, contextStack)
        const valueNode = params[i + 1]
        if (valueNode === undefined) {
          throw new LitsError('Missing value for key', keyNode[2])
        }
        const value = evaluateNode(valueNode, contextStack)
        assertString(key, keyNode[2])
        result[key] = value
      }
    }
    return result
  },
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
