import { describe, expect, it } from 'vitest'
import { AstNodeType } from '../../src'
import { ContextStackImpl } from '../../src/evaluator/ContextStack'
import type { SymbolNode } from '../../src/parser/interface'

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
})
