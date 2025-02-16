import { describe, it } from 'vitest'
import { testTypeGuars } from '../../__tests__/testUtils'
import type { QqNode } from '../builtin/specialExpressions/qq'
import { AstNodeType } from '../constants/constants'
import type {
  AstNode,
  ExpressionNode,
  NameNode,
  NormalExpressionNode,
  NormalExpressionNodeWithName,
  NumberNode,
  StringNode,
} from '../parser/interface'
import type { Token } from '../tokenizer/Token'
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
      t: AstNodeType.ReservedName,
      v: 'nil',
      debugData: undefined,
      p: [],
      n: undefined,
    }, {
      t: AstNodeType.ReservedName,
      v: 'nil',
      debugData: undefined,
      p: [],
      n: undefined,
    }],
    debugData: undefined,
  }
  const nameNode: NameNode = {
    t: AstNodeType.Name,
    debugData: { token: tkn, lastToken: tkn },
    v: 'A name',
    p: [],
    n: undefined,
  }
  const numberNode: NumberNode = {
    t: AstNodeType.Number,
    v: 12,
    debugData: { token: tkn, lastToken: tkn },
    p: [],
    n: undefined,
  }
  const stringNode: StringNode = {
    t: AstNodeType.String,
    v: 'foo',
    debugData: { token: ['Symbol', 'X'], lastToken: tkn },
    p: [],
    n: undefined,
  }
  const normalExpressionNodeWithName: NormalExpressionNodeWithName = {
    t: AstNodeType.NormalExpression,
    p: [],
    n: 'object',
    debugData: { token: ['Symbol', 'X'], lastToken: tkn },
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
          debugData: { token: ['Symbol', 'X'], lastToken: tkn },
          p: [],
          n: undefined,
        },
      ],
      debugData: undefined,
    }],
    debugData: undefined,
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
