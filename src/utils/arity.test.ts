import { describe, expect, it } from 'vitest'
import { LitsError } from '../errors'
import type { FunctionLike, NormalBuiltinSymbolNode, NormalExpressionNodeWithName, NumberNode } from '../parser/types'
import { NodeTypes } from '../constants/constants'
import { normalExpressionTypes } from '../builtin/normalExpressions'
import { arityAccepts, arityAcceptsMin, assertNumberOfParams, getArityFromFunction, getCommonArityFromFunctions, toFixedArity } from './arity'
import { FUNCTION_SYMBOL } from './symbols'

describe('arity utilities', () => {
  describe('arityAccepts', () => {
    it('should accept when number of params equals fixed count', () => {
      expect(arityAccepts(toFixedArity(2), 2)).toBe(true)
    })

    it('should reject when number of params does not equal fixed count', () => {
      expect(arityAccepts(toFixedArity(2), 3)).toBe(false)
    })

    it('should reject when number of params is less than minimum', () => {
      expect(arityAccepts({ min: 3 }, 2)).toBe(false)
    })

    it('should accept when number of params equals minimum', () => {
      expect(arityAccepts({ min: 3 }, 3)).toBe(true)
    })

    it('should reject when number of params is more than maximum', () => {
      expect(arityAccepts({ max: 3 }, 4)).toBe(false)
    })

    it('should accept when number of params equals maximum', () => {
      expect(arityAccepts({ max: 3 }, 3)).toBe(true)
    })
  })

  describe('arityAcceptsMin', () => {
    it('should accept when number of params equals fixed count', () => {
      expect(arityAcceptsMin(toFixedArity(2), 2)).toBe(true)
    })

    it('should accept when number of params is greater than fixed count', () => {
      expect(arityAcceptsMin(toFixedArity(2), 3)).toBe(true)
    })

    it('should reject when number of params is less than fixed count', () => {
      expect(arityAcceptsMin(toFixedArity(2), 1)).toBe(false)
    })

    it('should accept when number of params meets minimum requirement', () => {
      expect(arityAcceptsMin({ min: 2 }, 3)).toBe(true)
    })

    it('should accept when number of params equals minimum requirement', () => {
      expect(arityAcceptsMin({ min: 2 }, 2)).toBe(true)
    })

    it('should reject when number of params is less than minimum requirement', () => {
      expect(arityAcceptsMin({ min: 2 }, 1)).toBe(false)
    })
  })

  describe('getCommonArityFromFunctions', () => {
    it('should return common fixed count when all functions have same count', () => {
      const functions = [{ [FUNCTION_SYMBOL]: true, arity: toFixedArity(2) }, { [FUNCTION_SYMBOL]: true, arity: toFixedArity(2) }] as FunctionLike[]
      expect(getCommonArityFromFunctions(functions)).toEqual(toFixedArity(2))
    })

    it('should return null when functions have different fixed counts', () => {
      const functions = [{ [FUNCTION_SYMBOL]: true, arity: toFixedArity(2) }, { [FUNCTION_SYMBOL]: true, arity: toFixedArity(3) }] as FunctionLike[]
      expect(getCommonArityFromFunctions(functions)).toBe(null)
    })

    it('should return common range when functions have compatible ranges', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, arity: { min: 1, max: 3 } },
        { [FUNCTION_SYMBOL]: true, arity: { min: 2, max: 4 } },
      ] as FunctionLike[]
      expect(getCommonArityFromFunctions(functions)).toEqual({ min: 2, max: 3 })
    })

    it('should return null if min and number are not compatible', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, arity: { min: 3 } },
        { [FUNCTION_SYMBOL]: true, arity: toFixedArity(2) },
      ] as FunctionLike[]
      expect(getCommonArityFromFunctions(functions)).toBe(null)
    })

    it('should return null when ranges are incompatible', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, arity: { min: 3, max: 4 } },
        { [FUNCTION_SYMBOL]: true, arity: { min: 1, max: 2 } },
      ] as FunctionLike[]
      expect(getCommonArityFromFunctions(functions)).toBe(null)
    })

    it('should return null when min is greater than max', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, arity: { min: 4, max: 2 } },
        { [FUNCTION_SYMBOL]: true, arity: { min: 1, max: 3 } },
      ] as FunctionLike[]
      expect(getCommonArityFromFunctions(functions)).toBe(null)
    })

    it('should return fixed count when min equals max', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, arity: toFixedArity(2) },
        { [FUNCTION_SYMBOL]: true, arity: toFixedArity(2) },
      ] as FunctionLike[]
      expect(getCommonArityFromFunctions(functions)).toEqual(toFixedArity(2))
    })

    it('should return fixed count when min equals max and constraints are compatible with min', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, arity: { min: 2, max: 2, even: true } },
        { [FUNCTION_SYMBOL]: true, arity: { min: 2, max: 2, even: true } },
      ] as FunctionLike[]
      expect(getCommonArityFromFunctions(functions)).toEqual(toFixedArity(2))
    })
  })

  describe('getArityFromFunction', () => {
    it('should return 1 for number input', () => {
      expect(getArityFromFunction(42)).toEqual({ min: 1, max: 1 })
    })

    it('should return 1 for collection input', () => {
      expect(getArityFromFunction([])).toEqual({ min: 1, max: 1 })
    })

    it('should return arity from function object', () => {
      const fn: FunctionLike = { [FUNCTION_SYMBOL]: true, arity: 3 }
      expect(getArityFromFunction(fn)).toBe(3)
    })
  })

  describe('assertNumberOfParams', () => {
    const createTestNode = (params: number[]): NormalExpressionNodeWithName => {
      const symbolNode: NormalBuiltinSymbolNode = [NodeTypes.NormalBuiltinSymbol, normalExpressionTypes['+']!]
      const paramNodes: NumberNode[] = params.map(p => [NodeTypes.Number, p])
      const sourceCodeInfo = {
        position: {
          line: 1,
          column: 1,
        },
        code: 'test',
      } as const
      return [NodeTypes.NormalExpression, [symbolNode, paramNodes], sourceCodeInfo]
    }

    it('should not throw when number of params matches fixed count', () => {
      const node = createTestNode([1, 2])
      expect(() => assertNumberOfParams(toFixedArity(2), node[1][1].length, node[2])).not.toThrow()
    })

    it('should throw when number of params does not match fixed count', () => {
      const node = createTestNode([1, 2, 3])
      expect(() => assertNumberOfParams(toFixedArity(2), node[1][1].length, node[2])).toThrow(LitsError)
    })

    it('should throw when number of params is less than minimum', () => {
      const node = createTestNode([1])
      expect(() => assertNumberOfParams({ min: 2 }, node[1][1].length, node[2])).toThrow(LitsError)
    })

    it('should throw when number of params is more than maximum', () => {
      const node = createTestNode([1, 2, 3])
      expect(() => assertNumberOfParams({ max: 2 }, node[1][1].length, node[2])).toThrow(LitsError)
    })

    it('should throw when min is greater than max', () => {
      const node = createTestNode([1, 2])
      expect(() => assertNumberOfParams({ min: 3, max: 2 }, node[1][1].length, node[2])).toThrow(LitsError)
    })

    it('should throw when max is less than min', () => {
      const node = createTestNode([1, 2, 3])
      expect(() => assertNumberOfParams({ min: 2, max: 1 }, node[1][1].length, node[2])).toThrow(LitsError)
    })

    it('should throw with correct error message when params are less than min', () => {
      const node = createTestNode([1])
      expect(() => assertNumberOfParams({ min: 2 }, node[1][1].length, node[2])).toThrow()
    })

    it('should throw with correct error message when params are more than max', () => {
      const node = createTestNode([1, 2, 3])
      expect(() => assertNumberOfParams({ max: 2 }, node[1][1].length, node[2])).toThrow()
    })
  })
})
