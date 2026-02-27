import { describe, expect, it, vitest } from 'vitest'
import { Lits } from '../src/Lits/Lits'
import { LitsError } from '../src/errors'
import type { NativeJsFunction } from '../src/parser/types'
import { FUNCTION_SYMBOL } from '../src/utils/symbols'

const jsFunctions = {
  tripple: (value: number) => value * 3,
  throwError: () => {
    throw new Error('An error')
  },
  throwNumber: () => {
    // eslint-disable-next-line ts/no-throw-literal
    throw 1
  },
  throwString: () => {
    // eslint-disable-next-line ts/no-throw-literal
    throw 'An error'
  },
}

const nativeJsFunction: NativeJsFunction = {
  nativeFn: {
    fn: (value: number) => value * value,
  },
  name: 'square',
  functionType: 'NativeJsFunction',
  [FUNCTION_SYMBOL]: true,
  arity: { min: 1, max: 1 },
  docString: 'Squares a number',
}
const values = {
  obj: {
    square: nativeJsFunction,
  },
}

describe('nativeJsFunction', () => {
  const lits = new Lits()
  it('samples', () => {
    expect(lits.run('tripple(9)', { bindings: jsFunctions })).toBe(27)
    expect(lits.run('let a = tripple; a(9)', { bindings: jsFunctions })).toBe(27)
    expect(() => lits.run('throwError()', { bindings: jsFunctions })).toThrowError(LitsError)
    expect(() => lits.run('throwString()', { bindings: jsFunctions })).toThrowError(LitsError)
    expect(() => lits.run('throwNumber()', { bindings: jsFunctions })).toThrowError(LitsError)
  })
  it('builtin names cannot be shadowed', () => {
    expect(() => lits.run('+(1, 2, 3)', { bindings: { '+': () => 0 } })).toThrowError(LitsError)
    expect(() => lits.run('if true then false else true end', { bindings: { if: () => true } })).toThrowError(LitsError)
    expect(() => lits.run('1', { bindings: { self: () => true } })).toThrowError(LitsError)
  })
  it('dotted binding keys are rejected', () => {
    expect(() => lits.run('1', { bindings: { 'foo.bar': () => true } })).toThrowError(LitsError)
    expect(() => lits.run('1', { bindings: { '.bar': () => true } })).toThrowError(LitsError)
  })
  it('nested nativeJsFunction', () => {
    expect(lits.run('obj.square(9)', { bindings: values })).toBe(81)
  })
  it('infinity', () => {
    const fn = vitest.fn()
    lits.run('stuff(1 / 0)', { bindings: { stuff: fn } })
    expect(fn).toHaveBeenCalledWith(Number.POSITIVE_INFINITY)
  })
  it('bare function as binding', () => {
    expect(lits.run('dbl(5)', { bindings: { dbl: (x: number) => x * 2 } })).toBe(10)
  })
})
