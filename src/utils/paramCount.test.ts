import { describe, expect, it } from 'vitest'
import { LitsError } from '../errors'
import type { FunctionLike, NormalBuiltinSymbolNode, NormalExpressionNodeWithName, NumberNode } from '../parser/types'
import { NodeTypes } from '../constants/constants'
import { normalExpressionTypes } from '../builtin/normalExpressions'
import { assertNumberOfParams, getCommonParamCountFromFunctions, getParamCountFromFunction, paramCountAccepts, paramCountAcceptsMin } from './paramCount'
import { FUNCTION_SYMBOL } from './symbols'

describe('paramCount utilities', () => {
  describe('paramCountAccepts', () => {
    it('should accept when number of params equals fixed count', () => {
      expect(paramCountAccepts(2, 2)).toBe(true)
    })

    it('should reject when number of params does not equal fixed count', () => {
      expect(paramCountAccepts(2, 3)).toBe(false)
    })

    it('should reject when number of params is not even', () => {
      expect(paramCountAccepts({ even: true }, 3)).toBe(false)
    })

    it('should accept when number of params is even', () => {
      expect(paramCountAccepts({ even: true }, 4)).toBe(true)
    })

    it('should reject when number of params is not odd', () => {
      expect(paramCountAccepts({ odd: true }, 4)).toBe(false)
    })

    it('should accept when number of params is odd', () => {
      expect(paramCountAccepts({ odd: true }, 3)).toBe(true)
    })

    it('should reject when number of params is less than minimum', () => {
      expect(paramCountAccepts({ min: 3 }, 2)).toBe(false)
    })

    it('should accept when number of params equals minimum', () => {
      expect(paramCountAccepts({ min: 3 }, 3)).toBe(true)
    })

    it('should reject when number of params is more than maximum', () => {
      expect(paramCountAccepts({ max: 3 }, 4)).toBe(false)
    })

    it('should accept when number of params equals maximum', () => {
      expect(paramCountAccepts({ max: 3 }, 3)).toBe(true)
    })
  })

  describe('paramCountAcceptsMin', () => {
    it('should accept when number of params equals fixed count', () => {
      expect(paramCountAcceptsMin(2, 2)).toBe(true)
    })

    it('should accept when number of params is greater than fixed count', () => {
      expect(paramCountAcceptsMin(2, 3)).toBe(true)
    })

    it('should reject when number of params is less than fixed count', () => {
      expect(paramCountAcceptsMin(2, 1)).toBe(false)
    })

    it('should accept when number of params meets minimum requirement', () => {
      expect(paramCountAcceptsMin({ min: 2 }, 3)).toBe(true)
    })

    it('should accept when number of params equals minimum requirement', () => {
      expect(paramCountAcceptsMin({ min: 2 }, 2)).toBe(true)
    })

    it('should reject when number of params is less than minimum requirement', () => {
      expect(paramCountAcceptsMin({ min: 2 }, 1)).toBe(false)
    })
  })

  describe('getCommonParamCountFromFunctions', () => {
    it('should return common fixed count when all functions have same count', () => {
      const functions = [{ [FUNCTION_SYMBOL]: true, paramCount: 2 }, { [FUNCTION_SYMBOL]: true, paramCount: 2 }] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toBe(2)
    })

    it('should return null when functions have different fixed counts', () => {
      const functions = [{ [FUNCTION_SYMBOL]: true, paramCount: 2 }, { [FUNCTION_SYMBOL]: true, paramCount: 3 }] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toBe(null)
    })

    it('should return common range when functions have compatible ranges', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 1, max: 3 } },
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 2, max: 4 } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toEqual({ min: 2, max: 3 })
    })

    it('should return null if min and number are not compatible', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 3 } },
        { [FUNCTION_SYMBOL]: true, paramCount: 2 },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toBe(null)
    })

    it('should adjust min if even', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 1 } },
        { [FUNCTION_SYMBOL]: true, paramCount: { even: true } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toEqual({ min: 2, even: true })
    })

    it('should adjust max if even', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { max: 1 } },
        { [FUNCTION_SYMBOL]: true, paramCount: { even: true } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toEqual({ max: 0, even: true })
    })

    it('should return null when ranges are incompatible', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 3, max: 4 } },
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 1, max: 2 } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toBe(null)
    })

    it('should handle even/odd constraints', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { even: true } },
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 2, max: 4 } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toEqual({ min: 2, max: 4, even: true })
    })

    it('should return null when even and odd constraints conflict', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { even: true } },
        { [FUNCTION_SYMBOL]: true, paramCount: { odd: true } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toBe(null)
    })

    it('should return null when min is greater than max', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 4, max: 2 } },
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 1, max: 3 } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toBe(null)
    })

    it('should return null when odd constraint conflicts with min', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { odd: true, min: 2 } },
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 1, max: 2 } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toBe(null)
    })

    it('should return fixed count when min equals max', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 2, max: 2 } },
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 2, max: 2 } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toBe(2)
    })

    it('should return null when min equals max but conflicts with even constraint', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 2, max: 2 } },
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 2, max: 2, odd: true } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toBe(null)
    })

    it('should return null when min equals max but conflicts with odd constraint', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 1, max: 1 } },
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 1, max: 1, even: true } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toBe(null)
    })

    it('should return null when min equals max and min is odd but has even constraint', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 3, max: 3 } },
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 3, max: 3, even: true } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toBe(null)
    })

    it('should return null when min equals max and min is even but has odd constraint', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 2, max: 2 } },
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 2, max: 2, odd: true } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toBe(null)
    })

    it('should return fixed count when min equals max and constraints are compatible', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 2, max: 2, even: true } },
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 2, max: 2 } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toBe(2)
    })

    it('should return fixed count when min equals max and no constraints', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 2, max: 2 } },
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 2, max: 2 } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toBe(2)
    })

    it('should return fixed count when min equals max and constraints are compatible with min', () => {
      const functions = [
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 2, max: 2, even: true } },
        { [FUNCTION_SYMBOL]: true, paramCount: { min: 2, max: 2, even: true } },
      ] as FunctionLike[]
      expect(getCommonParamCountFromFunctions(functions)).toBe(2)
    })
  })

  describe('getParamCountFromFunction', () => {
    it('should return 1 for number input', () => {
      expect(getParamCountFromFunction(42)).toBe(1)
    })

    it('should return 1 for collection input', () => {
      expect(getParamCountFromFunction([])).toBe(1)
    })

    it('should return paramCount from function object', () => {
      const fn: FunctionLike = { [FUNCTION_SYMBOL]: true, paramCount: 3 }
      expect(getParamCountFromFunction(fn)).toBe(3)
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
      expect(() => assertNumberOfParams(2, node)).not.toThrow()
    })

    it('should throw when number of params does not match fixed count', () => {
      const node = createTestNode([1, 2, 3])
      expect(() => assertNumberOfParams(2, node)).toThrow(LitsError)
    })

    it('should throw when number of params is less than minimum', () => {
      const node = createTestNode([1])
      expect(() => assertNumberOfParams({ min: 2 }, node)).toThrow(LitsError)
    })

    it('should throw when number of params is more than maximum', () => {
      const node = createTestNode([1, 2, 3])
      expect(() => assertNumberOfParams({ max: 2 }, node)).toThrow(LitsError)
    })

    it('should throw when number of params is not even', () => {
      const node = createTestNode([1, 2, 3])
      expect(() => assertNumberOfParams({ even: true }, node)).toThrow(LitsError)
    })

    it('should throw when number of params is not odd', () => {
      const node = createTestNode([1, 2])
      expect(() => assertNumberOfParams({ odd: true }, node)).toThrow(LitsError)
    })

    it('should throw when number of params is less than minimum with even constraint', () => {
      const node = createTestNode([1])
      expect(() => assertNumberOfParams({ min: 2, even: true }, node)).toThrow(LitsError)
    })

    it('should throw when number of params is more than maximum with odd constraint', () => {
      const node = createTestNode([1, 2, 3, 4])
      expect(() => assertNumberOfParams({ max: 3, odd: true }, node)).toThrow(LitsError)
    })

    it('should throw when min is greater than max', () => {
      const node = createTestNode([1, 2])
      expect(() => assertNumberOfParams({ min: 3, max: 2 }, node)).toThrow(LitsError)
    })

    it('should throw when max is less than min', () => {
      const node = createTestNode([1, 2, 3])
      expect(() => assertNumberOfParams({ min: 2, max: 1 }, node)).toThrow(LitsError)
    })

    it('should throw when min is greater than max with even constraint', () => {
      const node = createTestNode([1, 2])
      expect(() => assertNumberOfParams({ min: 3, max: 2, even: true }, node)).toThrow(LitsError)
    })

    it('should throw when max is less than min with odd constraint', () => {
      const node = createTestNode([1, 2, 3])
      expect(() => assertNumberOfParams({ min: 2, max: 1, odd: true }, node)).toThrow(LitsError)
    })

    it('should throw with correct error message when params are less than min', () => {
      const node = createTestNode([1])
      expect(() => assertNumberOfParams({ min: 2 }, node)).toThrow()
    })

    it('should throw with correct error message when params are more than max', () => {
      const node = createTestNode([1, 2, 3])
      expect(() => assertNumberOfParams({ max: 2 }, node)).toThrow()
    })

    it('should throw with correct error message when params are not even', () => {
      const node = createTestNode([1, 2, 3])
      expect(() => assertNumberOfParams({ even: true }, node)).toThrow()
    })

    it('should throw with correct error message when params are not odd', () => {
      const node = createTestNode([1, 2])
      expect(() => assertNumberOfParams({ odd: true }, node)).toThrow()
    })
  })
})
