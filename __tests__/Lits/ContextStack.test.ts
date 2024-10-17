import { describe, expect, it } from 'vitest'
import { AstNodeType } from '../../src'
import { ContextStack } from '../../src/evaluator/ContextStack'
import type { NameNode } from '../../src/parser/interface'

function getNameNode(name: string): NameNode {
  return {
    t: AstNodeType.Name,
    v: name,
  }
}

describe('contextStack', () => {
  it('create', () => {
    const contextStack = new ContextStack({
      contexts: [{}],
      lazyValues: {
        foo: { read: () => 'foo' },
        bar: { read: () => 'bar' },
      },
    })

    const contextStack2 = contextStack.create({}, { foo: { read: () => 'xxx' } })

    expect(contextStack2.lookUp(getNameNode('foo'))).toEqual({ value: 'xxx' })
    expect(contextStack2.lookUp(getNameNode('bar'))).toEqual({ value: 'bar' })
  })
})
