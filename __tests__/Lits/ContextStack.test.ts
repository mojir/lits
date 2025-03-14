import { describe, expect, it } from 'vitest'
import { ContextStackImpl, createContextStack } from '../../src/evaluator/ContextStack'
import type { SymbolNode } from '../../src/parser/types'
import { AstNodeType } from '../../src/constants/constants'

function createNameNode(name: string): SymbolNode {
  return {
    t: AstNodeType.Symbol,
    v: name,
    token: undefined,
    p: [],
    n: undefined,
  }
}

describe('contextStack', () => {
  it('create', () => {
    const contextStack = new ContextStackImpl({
      contexts: [{}],
      lazyValues: {
        foo: { read: () => 'foo' },
        bar: { read: () => 'bar' },
      },
    })

    const contextStack2 = contextStack.create({}, { foo: { read: () => 'xxx' } })

    expect(contextStack2.lookUp(createNameNode('foo'))).toEqual({ value: 'xxx' })
    expect(contextStack2.lookUp(createNameNode('bar'))).toEqual({ value: 'bar' })
  })
  it('should throw if adding duplicate export', () => {
    const contextStack = createContextStack()

    contextStack.exportValue('foo', 'bar')
    expect(() => contextStack.exportValue('foo', 'bar')).toThrow()
  })
  it('should throw if exporting special expression', () => {
    const contextStack = createContextStack()

    expect(() => contextStack.exportValue('try', 'bar')).toThrow()
  })
  it('should throw if exporting normal expression', () => {
    const contextStack = createContextStack()

    expect(() => contextStack.exportValue('reduce', 'bar')).toThrow()
  })
  it('should throw if storing duplicate', () => {
    const contextStack = createContextStack()

    contextStack.addValue('foo', 'bar')
    expect(() => contextStack.addValue('foo', 'bar')).toThrow()
  })
  it('should throw if storing special expression', () => {
    const contextStack = createContextStack()

    expect(() => contextStack.addValue('try', 'bar')).toThrow()
  })
  it('should throw if storing normal expression', () => {
    const contextStack = createContextStack()

    expect(() => contextStack.addValue('reduce', 'bar')).toThrow()
  })
})
