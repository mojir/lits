import { describe, expect, it, test } from 'vitest'
import { testTypeGuars } from '../../__tests__/testUtils'
import { AstNodeType } from '../constants/constants'
import type { AstNode, NormalExpressionNode } from '../parser/types'
import {
  asNonUndefined,
  asUnknownRecord,
  assertNonUndefined,
  assertNumberOfParams,
  assertUnknownRecord,
  canBeOperator,
  isUnknownRecord,
} from '.'

function toNormalExpressionNode(arr: number[]): NormalExpressionNode {
  const astNodes: AstNode[] = arr.map(n => ({
    t: AstNodeType.Number,
    v: n,
    token: undefined,
    p: [],
    n: undefined,
  }))
  return {
    n: 'let',
    p: astNodes,
    t: AstNodeType.NormalExpression,
    token: ['Symbol', 'X'],
  }
}

describe('typeGuards index file', () => {
  test('canBeOperator', () => {
    expect(canBeOperator({ odd: true })).toBe(false)
    expect(canBeOperator({ max: 1 })).toBe(false)
  })

  it('asNotUndefined', () => {
    expect(() => asNonUndefined(undefined)).toThrow()
    expect(asNonUndefined(null)).toBe(null)
    expect(asNonUndefined(false)).toBe(false)
    expect(asNonUndefined(true)).toBe(true)
    expect(asNonUndefined(0)).toBe(0)
    const obj = {}
    expect(asNonUndefined(obj)).toBe(obj)
  })
  it('assertNotUndefined', () => {
    expect(() => assertNonUndefined(undefined)).toThrow()
    expect(() => assertNonUndefined(undefined)).toThrow()
    expect(() => assertNonUndefined(null)).not.toThrow()
    expect(() => assertNonUndefined(false)).not.toThrow()
    expect(() => assertNonUndefined(true)).not.toThrow()
    expect(() => assertNonUndefined(0)).not.toThrow()
    expect(() => assertNonUndefined({})).not.toThrow()
  })
  it('assertLengthEven', () => {
    expect(() => assertNumberOfParams({ even: true }, toNormalExpressionNode([]))).not.toThrow()
    expect(() => assertNumberOfParams({ even: true }, toNormalExpressionNode([0]))).toThrow()
    expect(() => assertNumberOfParams({ even: true }, toNormalExpressionNode([0, 1]))).not.toThrow()
    expect(() => assertNumberOfParams({ even: true }, toNormalExpressionNode([0, 1, 2]))).toThrow()
    expect(() => assertNumberOfParams({ even: true }, toNormalExpressionNode([0, 1, 2, 3]))).not.toThrow()
    expect(() => assertNumberOfParams({ even: true }, toNormalExpressionNode([0, 1, 2, 3, 4]))).toThrow()
    expect(() => assertNumberOfParams({ even: true }, toNormalExpressionNode([0, 1, 2, 3, 4, 5]))).not.toThrow()
  })

  it('assertLengthOdd', () => {
    expect(() => assertNumberOfParams({ odd: true }, toNormalExpressionNode([]))).toThrow()
    expect(() => assertNumberOfParams({ odd: true }, toNormalExpressionNode([0]))).not.toThrow()
    expect(() => assertNumberOfParams({ odd: true }, toNormalExpressionNode([0, 1]))).toThrow()
    expect(() => assertNumberOfParams({ odd: true }, toNormalExpressionNode([0, 1, 2]))).not.toThrow()
    expect(() => assertNumberOfParams({ odd: true }, toNormalExpressionNode([0, 1, 2, 3]))).toThrow()
    expect(() => assertNumberOfParams({ odd: true }, toNormalExpressionNode([0, 1, 2, 3, 4]))).not.toThrow()
    expect(() => assertNumberOfParams({ odd: true }, toNormalExpressionNode([0, 1, 2, 3, 4, 5]))).toThrow()
  })

  it('assertLength', () => {
    expect(() => assertNumberOfParams(0, toNormalExpressionNode([]))).not.toThrow()
    expect(() => assertNumberOfParams(0, toNormalExpressionNode([1]))).toThrow()
    expect(() => assertNumberOfParams(1, toNormalExpressionNode([1]))).not.toThrow()
    expect(() => assertNumberOfParams(1, toNormalExpressionNode([]))).toThrow()
    expect(() => assertNumberOfParams(1, toNormalExpressionNode([1, 2]))).toThrow()
    expect(() => assertNumberOfParams(2, toNormalExpressionNode([1, 2]))).not.toThrow()
    expect(() => assertNumberOfParams(2, toNormalExpressionNode([1]))).toThrow()
    expect(() => assertNumberOfParams(2, toNormalExpressionNode([1, 2, 3]))).toThrow()
    expect(() => assertNumberOfParams({}, toNormalExpressionNode([]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, toNormalExpressionNode([1, 2, 3, 4, 5]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, toNormalExpressionNode([1, 2, 3, 4]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, toNormalExpressionNode([1, 2, 3]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, toNormalExpressionNode([1, 2]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, toNormalExpressionNode([1]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, toNormalExpressionNode([]))).toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, toNormalExpressionNode([1, 2, 3, 4, 5]))).toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, toNormalExpressionNode([1, 2, 3, 4]))).toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, toNormalExpressionNode([1, 2, 3]))).not.toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, toNormalExpressionNode([1, 2]))).not.toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, toNormalExpressionNode([1]))).not.toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, toNormalExpressionNode([]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, toNormalExpressionNode([]))).toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, toNormalExpressionNode([1]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, toNormalExpressionNode([1, 2]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, toNormalExpressionNode([1, 2, 3]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, toNormalExpressionNode([1, 2, 3, 4]))).toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, toNormalExpressionNode([1, 2, 3, 4, 5]))).toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, toNormalExpressionNode([1, 2, 3, 4, 5, 6]))).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, toNormalExpressionNode([]))).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, toNormalExpressionNode([1]))).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, toNormalExpressionNode([1, 2]))).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, toNormalExpressionNode([1, 2, 3]))).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, toNormalExpressionNode([1, 2, 3, 4]))).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, toNormalExpressionNode([1, 2, 3, 4, 5]))).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, toNormalExpressionNode([1, 2, 3, 4, 5, 6]))).toThrow()
  })

  it('unknownRecord', () => {
    const valid = [{}, { a: 1 }]
    const invalid = [undefined, null, 0, false, true, '', 'foo', []]
    testTypeGuars(
      {
        valid,
        invalid,
      },
      { is: isUnknownRecord, as: asUnknownRecord, assert: assertUnknownRecord },
    )
  })
})
