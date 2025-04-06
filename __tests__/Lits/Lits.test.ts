import { beforeEach, describe, expect, it } from 'vitest'
import { UndefinedSymbolError } from '../../src/errors'
import { Cache } from '../../src/Lits/Cache'
import type { LazyValue } from '../../src/Lits/Lits'
import { Lits } from '../../src/Lits/Lits'
import { assertLitsFunction } from '../../src/typeGuards/litsFunction'
import { FUNCTION_SYMBOL } from '../../src/utils/symbols'
import type { Ast, NormalExpressionNodeWithName, UserDefinedFunction } from '../../src/parser/types'
import { NodeTypes } from '../../src/constants/constants'
import { normalExpressionTypes } from '../../src/builtin/normalExpressions'

describe('all tests', () => {
  describe('tEST', () => {
    let lits: Lits
    beforeEach(() => {
      lits = new Lits({ debug: true, astCacheSize: 0 })
    })
    it('without params', () => {
      const fn = lits.run('-> $1 + $2')
      assertLitsFunction(fn)
      expect(lits.apply(fn, [2, 3])).toBe(5)
    })
    it('with empty params', () => {
      const fn = lits.run('-> $1 + $2')
      assertLitsFunction(fn)
      expect(lits.apply(fn, [2, 3], {})).toBe(5)
    })

    it('with params', () => {
      const fn = lits.run('-> $1 + $2 + x')
      assertLitsFunction(fn)
      expect(lits.apply(fn, [2, 3], { contexts: [{ x: { value: 1 } }] })).toBe(6)
    })
  })

  describe('lazy host values as function', () => {
    it('that it works', () => {
      const lits = new Lits()
      const lazyHostValues: Record<string, LazyValue> = {
        x: {
          read: () => 42,
        },
        foo: {
          read: () => ({
            [FUNCTION_SYMBOL]: true,
            name: undefined,
            functionType: 'UserDefined',
            evaluatedfunction: [[], [[NodeTypes.Number, 42]], {}],
          } satisfies UserDefinedFunction),
        },
      }

      expect(lits.run('x', { lazyValues: lazyHostValues })).toBe(42)
      expect(lits.run('foo()', { lazyValues: lazyHostValues })).toBe(42)
      expect(lits.run('z', { lazyValues: { z: { read: () => 12 } } })).toBe(12)
    })
  })

  describe('runtime info', () => {
    it('getRuntimeInfo().', () => {
      const lits = new Lits()
      expect(lits.getRuntimeInfo()).toMatchSnapshot()
    })
    it('getRuntimeInfo() with ast cache > 0', () => {
      const lits = new Lits({ astCacheSize: 10 })
      expect(lits.getRuntimeInfo()).toMatchSnapshot()
    })
    it('getRuntimeInfo() with ast cache = 0', () => {
      const lits = new Lits({ astCacheSize: 0 })
      expect(lits.getRuntimeInfo()).toMatchSnapshot()
    })
  })

  describe('context', () => {
    let lits: Lits
    beforeEach(() => {
      lits = new Lits({ debug: true })
    })
    it('a function.', () => {
      lits = new Lits({ astCacheSize: 10 })
      const contexts = [lits.context('export function tripple(x) x * 3 end;')]
      expect(lits.run('tripple(10)', { contexts })).toBe(30)
      expect(lits.run('tripple(10)', { contexts })).toBe(30)
    })
    it('a function with ast.', () => {
      lits = new Lits({ astCacheSize: 10 })
      const parseResult = lits.parse(lits.tokenize('export function tripple(x) x * 3 end;'))
      const contexts = [lits.context(parseResult)]
      expect(lits.run('tripple(10)', { contexts })).toBe(30)
      expect(lits.run('tripple(10)', { contexts })).toBe(30)
    })

    it('a function - no cache', () => {
      lits = new Lits({ debug: true })
      const contexts = [lits.context('export function tripple(x) x * 3 end;', {})]
      expect(lits.run('tripple(10)', { contexts })).toBe(30)
      expect(lits.run('tripple(10)', { contexts })).toBe(30)
    })

    it('a function - initial cache', () => {
      const initialCache: Record<string, Ast> = {
        '2 ^ 4': {
          hasDebugData: false,
          body: [
            [NodeTypes.NormalExpression, [[NodeTypes.NormalBuiltinSymbol, normalExpressionTypes['^'] as number], [[NodeTypes.Number, 2], [NodeTypes.Number, 2]]]] satisfies NormalExpressionNodeWithName,
          ],
        },
      }
      lits = new Lits({ astCacheSize: 10, initialCache })
      expect(lits.run('2 ^ 2')).toBe(4)
      expect(lits.run('2 ^ 4')).toBe(4)
    })

    it('a variable.', () => {
      const contexts = [lits.context('export let magicNumber := 42;')]
      expect(lits.run('magicNumber', { contexts })).toBe(42)
    })

    it('a variable - again.', () => {
      const contexts = [
        lits.context(`
    export function zip?(input) boolean(match(input, #"^\\d{5}$")) end;
    export let NAME_LENGTH := 100;
    `),
      ]
      expect(lits.run('NAME_LENGTH', { contexts })).toBe(100)
    })

    it('a function with a built in normal expression name', () => {
      expect(() => lits.context('function inc(x) x + 1 end')).toThrow()
    })

    it('a function with a built in special expression name', () => {
      expect(() => lits.context('function and(x) x + 1 end')).toThrow()
    })

    it('a variable twice', () => {
      const contexts = [lits.context('export let magicNumber := 42; export function getMagic() 42 end;')]
      lits.context('export let magicNumber := 42; export function getMagic() 42 end;', { contexts })
    })

    it('more than one', () => {
      const contexts = [lits.context('export function tripple(x) x * 3 end;'), lits.context('export let magicNumber := 42;')]
      expect(lits.run('tripple(magicNumber)', { contexts })).toBe(126)
    })
  })

  function ast(n: number): Ast {
    return {
      hasDebugData: false,
      body: [[NodeTypes.Number, n]],
    }
  }

  describe('cache', () => {
    it('cannot set same key twice', () => {
      const cache = new Cache(10)
      cache.set('a', ast(1))
      expect(() => cache.set('a', ast(2))).toThrow()
    })

    it('getContent', () => {
      const cache = new Cache(10)
      cache.set('a', ast(1))
      cache.set('b', ast(2))
      expect(cache.getContent()).toEqual({
        a: ast(1),
        b: ast(2),
      })
    })
    it('getContent (null)', () => {
      const cache = new Cache(null)
      cache.set('a', ast(1))
      cache.set('b', ast(2))
      expect(cache.getContent()).toEqual({
        a: ast(1),
        b: ast(2),
      })
    })

    it('max cache size must be at least 1', () => {
      expect(() => new Cache(-1)).toThrow()
      expect(() => new Cache(0)).toThrow()
      expect(() => new Cache(0.1)).not.toThrow()
      expect(() => new Cache(1)).not.toThrow()
    })

    it('add an entry.', () => {
      const cache = new Cache(10)
      expect(cache.size).toBe(0)
      cache.set('a', ast(1))
      expect(cache.size).toBe(1)
      expect(cache.get('a')).toEqual(ast(1))
      expect(cache.has('a')).toBe(true)
    })

    it('clear cache.', () => {
      const cache = new Cache(10)
      cache.set('a', ast(1))
      cache.set('b', ast(2))
      cache.set('c', ast(3))
      expect(cache.size).toBe(3)
      cache.clear()
      expect(cache.size).toBe(0)
    })

    it('add an entry - cacheSize = 1', () => {
      const cache = new Cache(1)
      expect(cache.size).toBe(0)
      cache.set('a', ast(1))
      expect(cache.size).toBe(1)
      expect(cache.get('a')).toEqual(ast(1))
    })
    it('maxSize.', () => {
      const cache = new Cache(1)
      cache.set('a', ast(1))
      expect(cache.get('a')).toEqual(ast(1))
      cache.set('b', ast(2))
      expect(cache.size).toBe(1)
      expect(cache.get('a')).toBeUndefined()
      expect(cache.has('a')).toBe(false)
      expect(cache.get('b')).toEqual(ast(2))
      expect(cache.has('b')).toBe(true)
    })
  })

  describe('regressions', () => {
    let lits: Lits
    beforeEach(() => {
      lits = new Lits({ debug: true })
    })
    it.skip('sourceCodeInfo', () => {
      try {
        lits.run('let n := 3 write!(n)') // Missing semi
      }
      catch (error) {
      // eslint-disable-next-line ts/no-unsafe-member-access
        expect((error as any).sourceCodeInfo.position.line).toBe(3)
        // eslint-disable-next-line ts/no-unsafe-member-access
        expect((error as any).sourceCodeInfo.position.column).toBe(7)
      }
    })
    it('name not recognized', () => {
      expect(() => lits.run('asd()')).toThrowError(UndefinedSymbolError)
      expect(() => lits.run('asd')).toThrowError(UndefinedSymbolError)
    })

    it('unexpected argument', () => {
      try {
        lits.run('1 + + 2')
      }
      catch (error) {
      // eslint-disable-next-line ts/no-unsafe-assignment
        const anyError = error as any
        // eslint-disable-next-line ts/no-unsafe-member-access
        expect(anyError.sourceCodeInfo.position.line).toBe(1)
        // eslint-disable-next-line ts/no-unsafe-member-access
        expect(anyError.sourceCodeInfo.position.column).toBe(7)
      }
    })

    it('shoud handle double quoted in strings', () => {
    // You need to escape double quote with a backslash
      expect(lits.run('"\\""')).toBe('"')
      // You need to escape backslash with a backslash if it is at the end of the string
      expect(lits.run('"\\\\"')).toBe('\\')
      // You need to escape backslash with a backslash if it is followed by a double quote
      expect(lits.run('"\\"\\\\\\""')).toBe('"\\"')
      // Backslash before normal character is returning the character itself
      expect(lits.run('"\\abc"')).toBe('abc')
    })
  })
})
