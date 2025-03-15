import { describe, it } from 'vitest'
import { testTypeGuars } from '../../__tests__/testUtils'
import type { QqNode } from '../builtin/specialExpressions/qq'
import type {
  AstNode,
  ExpressionNode,
  NormalExpressionNode,
  NormalExpressionNodeWithName,
  NumberNode,
  StringNode,
  SymbolNode,
} from '../parser/types'
import type { Token } from '../tokenizer/token'
import {
  asAstNode,
  asExpressionNode,
  asNormalExpressionNode,
  asNormalExpressionNodeWithName,
  asSymbolNode,
  assertAstNode,
  assertExpressionNode,
  assertNormalExpressionNode,
  assertNormalExpressionNodeWithName,
  assertSymbolNode,
  isAstNode,
  isExpressionNode,
  isNormalExpressionNode,
  isNormalExpressionNodeWithName,
  isSymbolNode,
} from './astNode'

describe('astNode type guards', () => {
  const tkn: Token = ['Symbol', 'X']
  const invalidAstNodes: unknown[] = [
    {
      tkn,
      value: 'A name',
    },
    {
      type: 999,
      tkn,
      value: 'A name',
    },
    {},
    null,
    0,
    1,
    true,
    false,
    null,
    [],
  ]
  const specialExpressionNode: QqNode = {
    type: 'SpecialExpression',
    name: '??',
    params: [{
      type: 'ReservedSymbol',
      value: 'null',
      token: undefined,
      params: [],
      name: undefined,
    }, {
      type: 'ReservedSymbol',
      value: 'null',
      token: undefined,
      params: [],
      name: undefined,
    }],
    token: undefined,
  }
  const nameNode: SymbolNode = {
    type: 'Symbol',
    token: tkn,
    value: 'A name',
    params: [],
    name: undefined,
  }
  const numberNode: NumberNode = {
    type: 'Number',
    value: 12,
    token: tkn,
    params: [],
    name: undefined,
  }
  const stringNode: StringNode = {
    type: 'String',
    value: 'foo',
    token: ['Symbol', 'X'],
    params: [],
    name: undefined,
  }
  const normalExpressionNodeWithName: NormalExpressionNodeWithName = {
    type: 'NormalExpression',
    params: [],
    name: 'object',
    token: ['Symbol', 'X'],
  }
  const normalExpressionNodeWithoutName: NormalExpressionNode = {
    type: 'NormalExpression',
    name: undefined,
    params: [{
      type: 'NormalExpression',
      name: '+',
      params: [
        {
          type: 'Number',
          value: 2,
          token: ['Symbol', 'X'],
          params: [],
          name: undefined,
        },
      ],
      token: undefined,
    }],
    token: undefined,
  }

  const expressionNodes: ExpressionNode[] = [
    normalExpressionNodeWithName,
    normalExpressionNodeWithoutName,
    specialExpressionNode,
    numberNode,
    stringNode,
  ]

  const validAstNodes: AstNode[] = [nameNode, ...expressionNodes]

  it('astNode', () => {
    testTypeGuars(
      {
        valid: [...validAstNodes],
        invalid: [...invalidAstNodes],
      },
      { is: isAstNode, as: asAstNode, assert: assertAstNode },
    )
  })

  it('nameNode', () => {
    testTypeGuars(
      {
        valid: [nameNode],
        invalid: [...invalidAstNodes, ...validAstNodes.filter(node => node !== nameNode)],
      },
      { is: isSymbolNode, as: asSymbolNode, assert: assertSymbolNode },
    )
  })

  it('isNormalExpressionNodeWithName', () => {
    testTypeGuars(
      {
        valid: [normalExpressionNodeWithName],
        invalid: [...invalidAstNodes, ...validAstNodes.filter(node => node !== normalExpressionNodeWithName)],
      },
      {
        is: isNormalExpressionNodeWithName,
        as: asNormalExpressionNodeWithName,
        assert: assertNormalExpressionNodeWithName,
      },
    )
  })

  it('isNormalExpressionNode', () => {
    testTypeGuars(
      {
        valid: [normalExpressionNodeWithName, normalExpressionNodeWithoutName],
        invalid: [
          ...invalidAstNodes,
          ...validAstNodes.filter(
            node => node !== normalExpressionNodeWithName && node !== normalExpressionNodeWithoutName,
          ),
        ],
      },
      { is: isNormalExpressionNode, as: asNormalExpressionNode, assert: assertNormalExpressionNode },
    )
  })

  it('expressionNode', () => {
    testTypeGuars(
      {
        valid: [...expressionNodes],
        invalid: [...invalidAstNodes, ...validAstNodes.filter(node => !(expressionNodes as unknown[]).includes(node))],
      },
      { is: isExpressionNode, as: asExpressionNode, assert: assertExpressionNode },
    )
  })
})
