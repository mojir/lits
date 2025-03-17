import { describe, expect, it } from 'vitest'
import type { BindingTarget } from '../parser/types'
import { getAllBindingTargetNames } from './bindingNode'

describe('getAllBindingTargetNames', () => {
  it('should return an empty array for an empty binding node', () => {
    const bindingTarget: BindingTarget = {
      type: 'object',
      elements: {},
      token: undefined,
    }

    const result = getAllBindingTargetNames(bindingTarget)
    expect(result).toEqual([])
  })

  it('should return a single name for a symbol target', () => {
    const bindingTarget: BindingTarget = {
      type: 'symbol',
      name: 'x',
      token: undefined,
    }

    const result = getAllBindingTargetNames(bindingTarget)
    expect(result).toEqual(['x'])
  })

  it('should return all names for an object target', () => {
    const bindingTarget: BindingTarget = {
      type: 'object',
      elements: {
        a: { type: 'symbol', name: 'a', token: undefined },
        b: { type: 'symbol', name: 'b', token: undefined },
      },
      token: undefined,
    }
    const result = getAllBindingTargetNames(bindingTarget)
    expect(result).toEqual(['a', 'b'])
  })

  it('should return all names for a nested object target', () => {
    const bindingTarget: BindingTarget = {
      type: 'object',
      elements: {
        a: {
          type: 'object',
          elements: {
            x: { type: 'symbol', name: 'x', token: undefined },
            y: { type: 'symbol', name: 'y', token: undefined },
          },
          token: undefined,
        },
        z: { type: 'symbol', name: 'z', token: undefined },
      },
      token: undefined,
    }

    const result = getAllBindingTargetNames(bindingTarget)
    expect(result).toEqual(['x', 'y', 'z'])
  })

  it('should handle alias names for symbols', () => {
    const bindingTarget: BindingTarget = {
      type: 'symbol',
      name: 'originalName',
      alias: 'aliasName',
      token: undefined,
    }

    const result = getAllBindingTargetNames(bindingTarget)
    expect(result).toEqual(['aliasName'])
  })

  it('should return all names for an array target', () => {
    const bindingTarget: BindingTarget = {
      type: 'array',
      elements: [
        { type: 'symbol', name: 'a', token: undefined },
        { type: 'symbol', name: 'b', token: undefined },
      ],
      token: undefined,
    }

    const result = getAllBindingTargetNames(bindingTarget)
    expect(result).toEqual(['a', 'b'])
  })

  it('should return all names for a deeply nested structure', () => {
    const bindingTarget: BindingTarget = {
      type: 'object',
      elements: {
        a: {
          type: 'array',
          elements: [
            { type: 'symbol', name: 'x', token: undefined },
            {
              type: 'object',
              elements: {
                y: { type: 'symbol', name: 'y', token: undefined },
                z: { type: 'symbol', name: 'z', token: undefined },
              },
              token: undefined,
            },
          ],
          token: undefined,
        },
      },
      token: undefined,
    }

    const result = getAllBindingTargetNames(bindingTarget)
    expect(result).toEqual(['x', 'y', 'z'])
  })
})
