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
} from '../parser/interface'
import type { Token } from '../tokenizer/tokens'
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
  const tkn: Token = ['PF_Symbol', 'X']
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
  const nameNode: SymbolNode = {
    t: AstNodeType.Name,
    debugData: { token: tkn },
    v: 'A name',
    p: [],
    n: undefined,
  }
  const numberNode: NumberNode = {
    t: AstNodeType.Number,
    v: 12,
    debugData: { token: tkn },
    p: [],
    n: undefined,
  }
  const stringNode: StringNode = {
    t: AstNodeType.String,
    v: 'foo',
    debugData: { token: ['PF_Symbol', 'X'] },
    p: [],
    n: undefined,
  }
  const normalExpressionNodeWithName: NormalExpressionNodeWithName = {
    t: AstNodeType.NormalExpression,
    p: [],
    n: 'object',
    debugData: { token: ['PF_Symbol', 'X'] },
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
          debugData: { token: ['PF_Symbol', 'X'] },
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
