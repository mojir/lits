import { describe, expect, it } from 'vitest'
import type { NameNode, RegularExpression } from '../src/parser/interface'
import { cloneColl, collHasKey, createNativeJsFunction, deepEqual, toNonNegativeInteger } from '../src/utils'
import { REGEXP_SYMBOL } from '../src/utils/symbols'
import { valueToString } from '../src/utils/debug/debugTools'
import { AstNodeType, FunctionType } from '../src'

describe('utils', () => {
  it('createNativeJsFunction', () => {
    const fnWithName = createNativeJsFunction(() => undefined, 'foo')
    expect(fnWithName.sourceCodeInfo).toBeUndefined()
    expect(fnWithName.n).toBe('foo')
    expect(typeof fnWithName.f.fn).toBe('function')
    expect(fnWithName.t).toBe(FunctionType.NativeJsFunction)
    expect(fnWithName.__fn).toBe(true)

    const fnWithoutName = createNativeJsFunction(() => undefined)
    expect(fnWithoutName.sourceCodeInfo).toBeUndefined()
    expect(fnWithoutName.n).toBeUndefined()
    expect(typeof fnWithoutName.f.fn).toBe('function')
    expect(fnWithoutName.t).toBe(FunctionType.NativeJsFunction)
    expect(fnWithoutName.__fn).toBe(true)
  })
  it('collHasKey', () => {
    expect(collHasKey(10, 1)).toBe(false)

    expect(collHasKey('Albert', 1)).toBe(true)
    expect(collHasKey('Albert', -1)).toBe(false)
    expect(collHasKey('Albert', 1.2)).toBe(false)
    expect(collHasKey('Albert', 6)).toBe(false)
    expect(collHasKey('', 0)).toBe(false)

    expect(collHasKey([1, 2, 3], 1)).toBe(true)
    expect(collHasKey([1, 2, 3], 6)).toBe(false)
    expect(collHasKey([], 0)).toBe(false)

    expect(collHasKey({ a: 1, b: 2 }, 'a')).toBe(true)
    expect(collHasKey({ a: 1, b: 2 }, 'b')).toBe(true)
    expect(collHasKey({ a: 1, b: 2 }, 'c')).toBe(false)
    expect(collHasKey({}, 0)).toBe(false)
    expect(collHasKey({}, 'a')).toBe(false)
  })

  const primitives = [0, 1, true, false, null, 'Albert', 'Mojir']
  describe('deepEqual', () => {
    it('primitives', () => {
      for (const a of primitives) {
        for (const b of primitives)
          expect(deepEqual(a, b)).toBe(a === b)
      }
    })
    it('regExp', () => {
      const a: RegularExpression = {
        [REGEXP_SYMBOL]: true,
        s: '^ab',
        f: '',
      }
      const b: RegularExpression = {
        [REGEXP_SYMBOL]: true,
        s: '^ab',
        f: '',
      }
      const c: RegularExpression = {
        [REGEXP_SYMBOL]: true,
        s: '^ab',
        f: 'g',
      }
      const d: RegularExpression = {
        [REGEXP_SYMBOL]: true,
        s: '^ab',
        f: 'g',
      }
      expect(deepEqual(a, a)).toBe(true)
      expect(deepEqual(a, b)).toBe(true)
      expect(deepEqual(a, c)).toBe(false)
      expect(deepEqual(a, d)).toBe(false)
      expect(deepEqual(b, b)).toBe(true)
      expect(deepEqual(b, c)).toBe(false)
      expect(deepEqual(b, d)).toBe(false)
      expect(deepEqual(c, c)).toBe(true)
      expect(deepEqual(c, d)).toBe(true)
    })
    it('nested structures', () => {
      expect(deepEqual([1, 2, 3], [1, 2, 3])).toBe(true)
      expect(deepEqual({ a: 1, b: 2 }, { a: 1, b: 2 })).toBe(true)
      expect(deepEqual([1, 2, { a: 1, b: 2 }], [1, 2, { b: 2, a: 1 }])).toBe(true)
    })
  })
  it('toNonNegativeInteger', () => {
    expect(toNonNegativeInteger(0)).toBe(0)
    expect(toNonNegativeInteger(-0.1)).toBe(0)
    expect(toNonNegativeInteger(-100)).toBe(0)
    expect(toNonNegativeInteger(0.01)).toBe(1)
    expect(toNonNegativeInteger(2.01)).toBe(3)
    expect(toNonNegativeInteger(4.0)).toBe(4)
  })

  describe('cloneColl', () => {
    it('samples', () => {
      expect(cloneColl({ a: 10 })).toEqual({ a: 10 })
      expect(cloneColl({ a: [1, 2, 3] })).toEqual({ a: [1, 2, 3] })
    })
    it('new instance', () => {
      const original = { a: [1, 2, 3] }
      const second = cloneColl(original)
      expect(original).not.toBe(second)
      second.a[0] = 10
      expect(original.a[0]).toBe(1)
    })
  })

  describe('helpers', () => {
    const n: NameNode = {
      t: AstNodeType.Name,
      v: 'Foo',
    }
    it('valueToString', () => {
      expect(valueToString(new Error('An error'))).toBe('Error: An error')
      expect(valueToString(n)).toBe('Name-node')
    })
  })
})
