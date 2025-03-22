import { describe, it } from 'vitest'
import { testTypeGuars } from '../../__tests__/testUtils'
import type { QqNode } from '../builtin/specialExpressions/qq'
import type {
  ExpressionNode,
  Node,
  NormalExpressionNodeExpression,
  NormalExpressionNodeWithName,
  NumberNode,
  StringNode,
  SymbolNode,
} from '../parser/types'
import { NodeTypes } from '../constants/constants'
import { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import {
  asExpressionNode,
  asNormalExpressionNode,
  asNormalExpressionNodeWithName,
  asSymbolNode,
  assertExpressionNode,
  assertNormalExpressionNode,
  assertNormalExpressionNodeWithName,
  assertSymbolNode,
  isExpressionNode,
  isNormalExpressionNode,
  isNormalExpressionNodeWithName,
  isSymbolNode,
} from './astNode'

describe('node type guards', () => {
  const specialExpressionNode: QqNode = [NodeTypes.SpecialExpression, [specialExpressionTypes['??'], [[NodeTypes.ReservedSymbol, null], [NodeTypes.ReservedSymbol, null]]]]
  const symbolNode: SymbolNode = [NodeTypes.Symbol, 'A name']
  const numberNode: NumberNode = [NodeTypes.Number, 12]
  const stringNode: StringNode = [NodeTypes.String, 'foo']
  const normalExpressionNodeWithName: NormalExpressionNodeWithName = [NodeTypes.NormalExpression, ['object', []]]
  const normalExpressionNodeWithoutName: NormalExpressionNodeExpression = [NodeTypes.NormalExpression, [stringNode, [numberNode]]]

  const expressionNodes: ExpressionNode[] = [
    normalExpressionNodeWithName,
    normalExpressionNodeWithoutName,
    specialExpressionNode,
    numberNode,
    stringNode,
  ]

  const validNodes: Node[] = [symbolNode, ...expressionNodes]

  it('nameNode', () => {
    testTypeGuars(
      {
        valid: [symbolNode],
        invalid: [...validNodes.filter(node => node !== symbolNode)],
      },
      { is: isSymbolNode, as: asSymbolNode, assert: assertSymbolNode },
    )
  })

  it('isNormalExpressionNodeWithName', () => {
    testTypeGuars(
      {
        valid: [normalExpressionNodeWithName],
        invalid: [...validNodes.filter(node => node !== normalExpressionNodeWithName)],
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
          ...validNodes.filter(
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
        invalid: [...validNodes.filter(node => !(expressionNodes as unknown[]).includes(node))],
      },
      { is: isExpressionNode, as: asExpressionNode, assert: assertExpressionNode },
    )
  })
})
