import { describe, expect, it } from 'vitest'
import type { ArrayBindingTarget, BindingTarget } from '../parser/types'
import { bindingTargetTypes } from '../parser/types'
import { NodeTypes } from '../constants/constants'
import { getAllBindingTargetNames } from './bindingNode'

describe('getAllBindingTargetNames', () => {
  it('should return an empty array for an empty binding node', () => {
    const bindingTarget: BindingTarget = [bindingTargetTypes.array, [[], undefined]]

    const result = getAllBindingTargetNames(bindingTarget)
    expect(result).toEqual({})
  })

  it('should return a single name for a symbol target', () => {
    const bindingTarget: BindingTarget = [bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, 'x'], undefined]]

    const result = getAllBindingTargetNames(bindingTarget)
    expect(result).toEqual({ x: true })
  })

  it('should return all names for an object target', () => {
    const bindingTarget: BindingTarget = [bindingTargetTypes.object, [
      {
        a: [bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, 'a'], undefined]],
        b: [bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, 'b'], undefined]],
      },
      undefined,
    ]]
    const result = getAllBindingTargetNames(bindingTarget)
    expect(result).toEqual({ a: true, b: true })
  })

  it('should return all names for a nested object target', () => {
    const bindingTarget: BindingTarget = [bindingTargetTypes.object, [{ a: [bindingTargetTypes.object, [
      {
        x: [bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, 'x'], undefined]],
        y: [bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, 'y'], undefined]],
      },
      undefined,
    ]], z: [bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, 'z'], undefined]] }, undefined]]

    const result = getAllBindingTargetNames(bindingTarget)
    expect(result).toEqual({ x: true, y: true, z: true })
  })

  it('should return all names for an array target', () => {
    const bindingTarget: ArrayBindingTarget = [bindingTargetTypes.array, [
      [
        [bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, 'a'], undefined]],
        [bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, 'b'], undefined]],
      ],
      undefined,
    ]]

    const result = getAllBindingTargetNames(bindingTarget)
    expect(result).toEqual({ a: true, b: true })
  })

  it('should return all names for a deeply nested structure', () => {
    const bindingTarget: BindingTarget = [bindingTargetTypes.object, [
      {
        a: [bindingTargetTypes.array, [
          [
            [bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, 'x'], undefined]],
            [bindingTargetTypes.object, [
              {
                y: [bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, 'y'], undefined]],
                z: [bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, 'z'], undefined]],
              },
              undefined,
            ]],
          ],
          undefined,
        ]],
      },
      undefined,
    ]]

    const result = getAllBindingTargetNames(bindingTarget)
    expect(result).toEqual({ x: true, y: true, z: true })
  })
})
