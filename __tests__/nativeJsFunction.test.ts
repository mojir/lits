import { describe, expect, it, vitest } from 'vitest'
import { type JsFunction, Lits } from '../src/Lits/Lits'
import { LitsError } from '../src/errors'
import type { NativeJsFunction } from '../src/parser/types'
import { FUNCTION_SYMBOL } from '../src/utils/symbols'

const jsFunctions: Record<string, JsFunction> = {
  tripple: {
    fn: (value: number) => value * 3,
  },
  throwError: {
    fn: () => {
      throw new Error('An error')
    },
  },
  throwNumber: {
    fn: () => {
      // eslint-disable-next-line ts/no-throw-literal
      throw 1
    },
  },
  throwString: {
    fn: () => {
      // eslint-disable-next-line ts/no-throw-literal
      throw 'An error'
    },
  },
}

const stupidJsFunctions: Record<string, JsFunction> = {
  '+': {
    fn: (value: number) => value * 3,
  },
  'if': {
    fn: () => true,
  },
}

const nativeJsFunction: NativeJsFunction = {
  nativeFn: {
    fn: (value: number) => value * value,
  },
  name: 'square',
  functionType: 'NativeJsFunction',
  [FUNCTION_SYMBOL]: true,
}
const values = {
  obj: {
    square: nativeJsFunction,
  },
}

describe('nativeJsFunction', () => {
  const lits = new Lits()
  it('samples', () => {
    expect(lits.run('tripple(9)', { jsFunctions })).toBe(27)
    expect(lits.run('let a := tripple; a(9)', { jsFunctions })).toBe(27)
    expect(() => lits.run('throwError()', { jsFunctions })).toThrowError(LitsError)
    expect(() => lits.run('throwString()', { jsFunctions })).toThrowError(LitsError)
    expect(() => lits.run('throwNumber()', { jsFunctions })).toThrowError(LitsError)
  })
  it('builtin names cannot be shadowed', () => {
    const warn = console.warn
    console.warn = vitest.fn()
    expect(lits.run('+(1, 2, 3)', { jsFunctions: stupidJsFunctions })).toBe(6)
    expect(console.warn).toHaveBeenCalledTimes(2)
    expect(lits.run('if true then false else true end', { jsFunctions: stupidJsFunctions })).toBe(false)
    expect(console.warn).toHaveBeenCalledTimes(4)
    console.warn = warn
  })
  it('nested nativeJsFunction', () => {
    expect(lits.run('obj.square(9)', { values })).toBe(81)
  })
  it('infinity', () => {
    const fn = vitest.fn()
    const funs: Record<string, JsFunction> = {
      stuff: {
        fn,
      },
    }
    lits.run('stuff(1 / 0)', { jsFunctions: funs })
    expect(fn).toHaveBeenCalledWith(Number.POSITIVE_INFINITY)
  })
})
