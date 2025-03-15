import { describe, expect, it } from 'vitest'
import { createContextStack } from '../../src/evaluator/ContextStack'

describe('contextStack', () => {
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
