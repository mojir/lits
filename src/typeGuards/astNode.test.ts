import { describe, it } from 'vitest'
import { testTypeGuars } from '../../__tests__/testUtils'
import type { QqNode } from '../builtin/specialExpressions/qq'
import { AstNodeType } from '../constants/constants'
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
      v: 'A name',
    },
    {
      t: 999,
      tkn,
      v: 'A name',
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
    t: AstNodeType.SpecialExpression,
    n: '??',
    p: [{
      t: AstNodeType.ReservedSymbol,
      v: 'null',
      token: undefined,
      p: [],
      n: undefined,
    }, {
      t: AstNodeType.ReservedSymbol,
      v: 'null',
      token: undefined,
      p: [],
      n: undefined,
    }],
    token: undefined,
  }
  const nameNode: SymbolNode = {
    t: AstNodeType.Symbol,
    token: tkn,
    v: 'A name',
    p: [],
    n: undefined,
  }
  const numberNode: NumberNode = {
    t: AstNodeType.Number,
    v: 12,
    token: tkn,
    p: [],
    n: undefined,
  }
  const stringNode: StringNode = {
    t: AstNodeType.String,
    v: 'foo',
    token: ['Symbol', 'X'],
    p: [],
    n: undefined,
  }
  const normalExpressionNodeWithName: NormalExpressionNodeWithName = {
    t: AstNodeType.NormalExpression,
    p: [],
    n: 'object',
    token: ['Symbol', 'X'],
  }
  const normalExpressionNodeWithoutName: NormalExpressionNode = {
    t: AstNodeType.NormalExpression,
    n: undefined,
    p: [{
      t: AstNodeType.NormalExpression,
      n: '+',
      p: [
        {
          t: AstNodeType.Number,
          v: 2,
          token: ['Symbol', 'X'],
          p: [],
          n: undefined,
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
