import { describe, expect, it } from 'vitest'
import { NodeTypes } from '../../constants/constants'
import { specialExpressionTypes } from '../../builtin/specialExpressionTypes'
import { LitsError } from '../../errors'
import { tokenize } from '../../tokenizer/tokenize'
import { minifyTokenStream } from '../../tokenizer/minifyTokenStream'
import type { ObjectNode } from '../../builtin/specialExpressions/object'
import type { AstNode } from '../types'
import { createParserContext } from './parseExpression'
import { parseObject } from './parseObject'

function createCtx(input: string) {
  const tokenStream = tokenize(input, false, undefined)
  const minified = minifyTokenStream(tokenStream, { removeWhiteSpace: true })
  return createParserContext(minified)
}

function getObjectParams(node: ObjectNode): AstNode[] {
  return node[1][1]
}

describe('parseObject', () => {
  describe('empty object', () => {
    it('should return a SpecialExpression node with object type', () => {
      const ctx = createCtx('{}')
      const result = parseObject(ctx)
      expect(result[0]).toBe(NodeTypes.SpecialExpression)
      expect(result[1][0]).toBe(specialExpressionTypes.object)
    })

    it('should have no params', () => {
      const ctx = createCtx('{}')
      const result = parseObject(ctx)
      expect(getObjectParams(result)).toEqual([])
    })

    it('should consume all tokens', () => {
      const ctx = createCtx('{}')
      parseObject(ctx)
      expect(ctx.isAtEnd()).toBe(true)
    })
  })

  describe('symbol keys', () => {
    it('should parse a symbol key as a String node', () => {
      const ctx = createCtx('{ a: 1 }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params).toHaveLength(2)
      expect(params[0]![0]).toBe(NodeTypes.String)
      expect(params[0]![1]).toBe('a')
    })

    it('should parse the value as a Number node', () => {
      const ctx = createCtx('{ a: 1 }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params[1]![0]).toBe(NodeTypes.Number)
      expect(params[1]![1]).toBe(1)
    })

    it('should parse multiple symbol keys', () => {
      const ctx = createCtx('{ a: 1, b: 2 }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params).toHaveLength(4)
      expect(params[0]![1]).toBe('a')
      expect(params[1]![1]).toBe(1)
      expect(params[2]![1]).toBe('b')
      expect(params[3]![1]).toBe(2)
    })

    it('should consume all tokens', () => {
      const ctx = createCtx('{ a: 1, b: 2 }')
      parseObject(ctx)
      expect(ctx.isAtEnd()).toBe(true)
    })
  })

  describe('string keys', () => {
    it('should parse a double-quoted string key', () => {
      const ctx = createCtx('{ "foo": 1 }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params[0]![0]).toBe(NodeTypes.String)
      expect(params[0]![1]).toBe('foo')
    })

    it('should parse a string key with spaces', () => {
      const ctx = createCtx('{ " ": 10 }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params[0]![1]).toBe(' ')
    })

    it('should parse a string key with escape sequences', () => {
      const ctx = createCtx('{ "a\\nb": 1 }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params[0]![1]).toBe('a\nb')
    })
  })

  describe('quoted symbol keys', () => {
    it('should parse a quoted symbol key and strip quotes', () => {
      const ctx = createCtx('{ \'foo bar\': 1 }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params[0]![0]).toBe(NodeTypes.String)
      expect(params[0]![1]).toBe('foo bar')
    })
  })

  describe('computed keys', () => {
    it('should parse a computed key with a string expression', () => {
      const ctx = createCtx('{ ["a"]: 1 }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      // Computed key is the parsed expression (String node)
      expect(params[0]![0]).toBe(NodeTypes.String)
      expect(params[0]![1]).toBe('a')
    })

    it('should parse a computed key with a complex expression', () => {
      const ctx = createCtx('{ ["a" ++ "b"]: 1 }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      // Computed key is a NormalExpression for ++
      expect(params[0]![0]).toBe(NodeTypes.NormalExpression)
    })

    it('should consume the closing bracket', () => {
      const ctx = createCtx('{ ["a"]: 1 }')
      parseObject(ctx)
      expect(ctx.isAtEnd()).toBe(true)
    })
  })

  describe('spread operator', () => {
    it('should parse spread as a Spread node', () => {
      const ctx = createCtx('{ ...x }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params).toHaveLength(1)
      expect(params[0]![0]).toBe(NodeTypes.Spread)
    })

    it('should parse spread payload as the expression', () => {
      const ctx = createCtx('{ ...x }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      // Spread payload is a UserDefinedSymbol node for 'x'
      const spreadPayload = params[0]![1] as AstNode
      expect(spreadPayload[0]).toBe(NodeTypes.UserDefinedSymbol)
      expect(spreadPayload[1]).toBe('x')
    })

    it('should parse spread mixed with key-value pairs', () => {
      const ctx = createCtx('{ ...x, a: 1 }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params).toHaveLength(3)
      expect(params[0]![0]).toBe(NodeTypes.Spread)
      expect(params[1]![0]).toBe(NodeTypes.String)
      expect(params[1]![1]).toBe('a')
      expect(params[2]![0]).toBe(NodeTypes.Number)
      expect(params[2]![1]).toBe(1)
    })

    it('should parse multiple spreads', () => {
      const ctx = createCtx('{ ...x, ...y }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params).toHaveLength(2)
      expect(params[0]![0]).toBe(NodeTypes.Spread)
      expect(params[1]![0]).toBe(NodeTypes.Spread)
    })
  })

  describe('expression values', () => {
    it('should parse arithmetic expression values', () => {
      const ctx = createCtx('{ a: 2 + 3 }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      // Value is a NormalExpression node for +
      expect(params[1]![0]).toBe(NodeTypes.NormalExpression)
    })

    it('should parse boolean values', () => {
      const ctx = createCtx('{ a: true }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params[1]![0]).toBe(NodeTypes.ReservedSymbol)
      expect(params[1]![1]).toBe('true')
    })

    it('should parse null values', () => {
      const ctx = createCtx('{ a: null }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params[1]![0]).toBe(NodeTypes.ReservedSymbol)
      expect(params[1]![1]).toBe('null')
    })

    it('should parse string values', () => {
      const ctx = createCtx('{ a: "hello" }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params[1]![0]).toBe(NodeTypes.String)
      expect(params[1]![1]).toBe('hello')
    })
  })

  describe('nested objects', () => {
    it('should parse nested object as a SpecialExpression', () => {
      const ctx = createCtx('{ a: { b: 1 } }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params[1]![0]).toBe(NodeTypes.SpecialExpression)
      expect((params[1]![1] as [number, AstNode[]])[0]).toBe(specialExpressionTypes.object)
    })

    it('should parse deeply nested objects', () => {
      const ctx = createCtx('{ a: { b: { c: 1 } } }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      const innerObject = params[1]!
      const innerParams = (innerObject[1] as [number, AstNode[]])[1]
      expect(innerParams[1]![0]).toBe(NodeTypes.SpecialExpression)
    })
  })

  describe('trailing comma', () => {
    it('should allow trailing comma', () => {
      const ctx = createCtx('{ a: 1, b: 2, }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params).toHaveLength(4)
      expect(ctx.isAtEnd()).toBe(true)
    })

    it('should allow trailing comma with single entry', () => {
      const ctx = createCtx('{ a: 1, }')
      const result = parseObject(ctx)
      const params = getObjectParams(result)
      expect(params).toHaveLength(2)
    })
  })

  describe('context position advancement', () => {
    it('should advance past the closing brace', () => {
      const ctx = createCtx('{ a: 1 }')
      parseObject(ctx)
      expect(ctx.isAtEnd()).toBe(true)
    })

    it('should leave remaining tokens after the object', () => {
      const ctx = createCtx('{ a: 1 } ;')
      parseObject(ctx)
      expect(ctx.isAtEnd()).toBe(false)
    })
  })

  describe('error cases', () => {
    it('should throw on numeric key', () => {
      const ctx = createCtx('{ 1: 1 }')
      expect(() => parseObject(ctx)).toThrow(LitsError)
    })

    it('should throw on missing colon', () => {
      const ctx = createCtx('{ a 1 }')
      expect(() => parseObject(ctx)).toThrow(LitsError)
    })

    it('should throw on missing value', () => {
      const ctx = createCtx('{ a: }')
      expect(() => parseObject(ctx)).toThrow(LitsError)
    })

    it('should throw on missing closing brace', () => {
      const ctx = createCtx('{ a: 1')
      expect(() => parseObject(ctx)).toThrow(LitsError)
    })

    it('should throw on consecutive commas', () => {
      const ctx = createCtx('{ a: 1,, b: 2 }')
      expect(() => parseObject(ctx)).toThrow(LitsError)
    })

    it('should throw on missing comma between entries', () => {
      const ctx = createCtx('{ a: 1 b: 2 }')
      expect(() => parseObject(ctx)).toThrow(LitsError)
    })

    it('should throw on computed key without closing bracket', () => {
      const ctx = createCtx('{ [a: 1 }')
      expect(() => parseObject(ctx)).toThrow(LitsError)
    })

    it('should throw when first token is not LBrace', () => {
      const ctx = createCtx('a: 1')
      expect(() => parseObject(ctx)).toThrow(LitsError)
    })
  })
})
