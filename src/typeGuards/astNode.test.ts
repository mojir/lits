import { describe, it } from 'vitest'
import { testTypeGuars } from '../../__tests__/testUtils'
import { AstNodeType, TokenType } from '../constants/constants'
import type {
  AstNode,
  ExpressionNode,
  NameNode,
  NormalExpressionNode,
  NormalExpressionNodeWithName,
  NumberNode,
  SpecialExpressionNode,
  StringNode,
} from '../parser/interface'
import type { Token } from '../tokenizer/interface'
import {
  asAstNode,
  asExpressionNode,
  asNameNode,
  asNormalExpressionNode,
  asNormalExpressionNodeWithName,
  assertAstNode,
  assertExpressionNode,
  assertNameNode,
  assertNormalExpressionNode,
  assertNormalExpressionNodeWithName,
  isAstNode,
  isExpressionNode,
  isNameNode,
  isNormalExpressionNode,
  isNormalExpressionNodeWithName,
} from './astNode'

describe('astNode type guards', () => {
  const tkn: Token = { t: TokenType.Name, v: 'X' }
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
  const specialExpressionNode: SpecialExpressionNode = {
    t: AstNodeType.SpecialExpression,
    n: '??',
    p: [],
  }
  const nameNode: NameNode = {
    t: AstNodeType.Name,
    tkn,
    v: 'A name',
  }
  const numberNode: NumberNode = {
    t: AstNodeType.Number,
    v: 12,
    tkn,
  }
  const stringNode: StringNode = {
    t: AstNodeType.String,
    v: 'foo',
    tkn: { t: TokenType.Name, v: 'X' },
  }
  const normalExpressionNodeWithName: NormalExpressionNodeWithName = {
    t: AstNodeType.NormalExpression,
    p: [],
    n: 'object',
    tkn: { t: TokenType.Name, v: 'X' },
  }
  const normalExpressionNodeWithoutName: NormalExpressionNode = {
    t: AstNodeType.NormalExpression,
    p: [],
    e: {
      t: AstNodeType.NormalExpression,
      n: '+',
      p: [
        {
          t: AstNodeType.Number,
          v: 2,
          tkn: { t: TokenType.Name, v: 'X' },
        },
      ],
    },
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
      { is: isNameNode, as: asNameNode, assert: assertNameNode },
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
