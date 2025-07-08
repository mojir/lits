import { describe, expect, it } from 'vitest'
import { createContextStack } from '../../src/evaluator/ContextStack'

describe('contextStack', () => {
  it('should throw if adding duplicate export', () => {
    const contextStack = createContextStack()

    contextStack.exportValues({ foo: 'bar' }, undefined)
    expect(() => contextStack.exportValues({ foo: 'bar' }, undefined)).toThrow()
  })
  it('should throw if exporting special expression', () => {
    const contextStack = createContextStack()

    expect(() => contextStack.exportValues({ try: 'bar' }, undefined)).toThrow()
  })
  it('should throw if exporting normal expression', () => {
    const contextStack = createContextStack()

    expect(() => contextStack.exportValues({ reduce: 'bar' }, undefined)).toThrow()
  })
  it('should throw if storing duplicate', () => {
    const contextStack = createContextStack()

    contextStack.addValues({ foo: 'bar' }, undefined)
    expect(() => contextStack.addValues({ foo: 'bar' }, undefined)).toThrow()
  })
  it('should throw if storing special expression', () => {
    const contextStack = createContextStack()

    expect(() => contextStack.addValues({ try: 'bar' }, undefined)).toThrow()
  })
  it('should throw if storing normal expression', () => {
    const contextStack = createContextStack()

    expect(() => contextStack.addValues({ reduce: 'bar' }, undefined)).toThrow()
  })
  it('should throw if exporting self', () => {
    const contextStack = createContextStack()

    expect(() => contextStack.exportValues({ self: 'bar' }, undefined)).toThrow()
  })
  it('should throw if storing self', () => {
    const contextStack = createContextStack()

    expect(() => contextStack.addValues({ self: 'bar' }, undefined)).toThrow()
  })
})
