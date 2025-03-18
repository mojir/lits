import { describe, expect, it } from 'vitest'
import { createContextStack } from '../../src/evaluator/ContextStack'

describe('contextStack', () => {
  it('should throw if adding duplicate export', () => {
    const contextStack = createContextStack()

    contextStack.exportValues({ foo: 'bar' })
    expect(() => contextStack.exportValues({ foo: 'bar' })).toThrow()
  })
  it('should throw if exporting special expression', () => {
    const contextStack = createContextStack()

    expect(() => contextStack.exportValues({ try: 'bar' })).toThrow()
  })
  it('should throw if exporting normal expression', () => {
    const contextStack = createContextStack()

    expect(() => contextStack.exportValues({ reduce: 'bar' })).toThrow()
  })
  it('should throw if storing duplicate', () => {
    const contextStack = createContextStack()

    contextStack.addValues({ foo: 'bar' })
    expect(() => contextStack.addValues({ foo: 'bar' })).toThrow()
  })
  it('should throw if storing special expression', () => {
    const contextStack = createContextStack()

    expect(() => contextStack.addValues({ try: 'bar' })).toThrow()
  })
  it('should throw if storing normal expression', () => {
    const contextStack = createContextStack()

    expect(() => contextStack.addValues({ reduce: 'bar' })).toThrow()
  })
})
