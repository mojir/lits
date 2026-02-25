import { beforeEach, describe, expect, it, test } from 'vitest'
import { LitsError, UndefinedSymbolError } from '../../src/errors'
import { Cache } from '../../src/Lits/Cache'
import { Lits } from '../../src/Lits/Lits'
import { assertLitsFunction } from '../../src/typeGuards/litsFunction'
import type { Ast, NormalExpressionNodeWithName } from '../../src/parser/types'
import { NodeTypes } from '../../src/constants/constants'
import { normalExpressionTypes } from '../../src/builtin/normalExpressions'
import type { Context } from '../../src/evaluator/interface'
import { vectorModule } from '../../src/builtin/modules/vector'

describe('all tests', () => {
  describe('auto completer', () => {
    it('should return empty array if no token stream', () => {
      const lits = new Lits()
      const autoCompleter = lits.getAutoCompleter('', 0)
      expect(autoCompleter.getNextSuggestion()).toBeNull()
    })
    it('should return empty array if invalid token stream', () => {
      const lits = new Lits()
      const autoCompleter = lits.getAutoCompleter('12s', 3)
      expect(autoCompleter.getNextSuggestion()).toBeNull()
    })
    it('should return xxx', () => {
      const lits = new Lits()
      const autoCompleter = lits.getAutoCompleter('1 + xx + 2', 6, { bindings: { xxx: 1 } })
      expect(autoCompleter.getNextSuggestion()).toEqual({
        program: '1 + xxx + 2',
        position: 7,
      })
    })
  })

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

  describe('bindings', () => {
    let lits: Lits
    beforeEach(() => {
      lits = new Lits({ debug: true })
    })
    it('a function via bindings.', () => {
      lits = new Lits({ astCacheSize: 10 })
      const bindings = lits.run('let tripple = (x) -> do x * 3 end; {tripple: tripple}') as Record<string, unknown>
      expect(lits.run('tripple(10)', { bindings })).toBe(30)
      expect(lits.run('tripple(10)', { bindings })).toBe(30)
    })

    it('a function - no cache', () => {
      lits = new Lits({ debug: true })
      const bindings = lits.run('let tripple = (x) -> do x * 3 end; {tripple: tripple}') as Record<string, unknown>
      expect(lits.run('tripple(10)', { bindings })).toBe(30)
      expect(lits.run('tripple(10)', { bindings })).toBe(30)
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
      const bindings = lits.run('let magicNumber = 42; {magicNumber: magicNumber}') as Record<string, unknown>
      expect(lits.run('magicNumber', { bindings })).toBe(42)
    })

    it('a variable - again.', () => {
      const bindings = lits.run(`
    let zip? = (input) -> do boolean(re-match(input, #"^\\d{5}$")) end;
    let NAME_LENGTH = 100;
    {zip?: zip?, NAME_LENGTH: NAME_LENGTH}
    `) as Record<string, unknown>
      expect(lits.run('NAME_LENGTH', { bindings })).toBe(100)
    })

    it('a function with a built in normal expression name', () => {
      expect(() => lits.run('let inc = (x) -> x + 1 end')).toThrow(LitsError)
    })

    it('a function with a built in special expression name', () => {
      expect(() => lits.run('let and = (x) -> x + 1 end')).toThrow(LitsError)
    })

    test('global context with globalModuleScope', () => {
      const globalContext: Context = {}
      lits.run('let magicNumber = 42; let double = magicNumber * 2;', { globalContext, globalModuleScope: true })
      expect(globalContext).toEqual({
        magicNumber: {
          value: 42,
        },
        double: {
          value: 84,
        },
      })
    })

    it('more than one', () => {
      const bindings1 = lits.run('let tripple = (x) -> do x * 3 end; {tripple: tripple}') as Record<string, unknown>
      const bindings2 = lits.run('let magicNumber = 42; {magicNumber: magicNumber}') as Record<string, unknown>
      const bindings = { ...bindings1, ...bindings2 }
      expect(lits.run('tripple(magicNumber)', { bindings })).toBe(126)
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
    it('sourceCodeInfo', () => {
      try {
        lits.run('let n = 3; write!(m)') // Missing semi
      }
      catch (error) {
        expect((error as LitsError).sourceCodeInfo?.position.line).toBe(1)

        expect((error as LitsError).sourceCodeInfo?.position.column).toBe(19)
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

  describe('getUndefinedSymbols', () => {
    it('should find undefined symbols from string input', () => {
      const lits = new Lits()
      const result = lits.getUndefinedSymbols('x + y')
      expect(result).toEqual(new Set(['x', 'y']))
    })

    it('should find undefined symbols from AST input', () => {
      const lits = new Lits()
      const parsedAst = lits.parse(lits.tokenize('x + y'))
      const result = lits.getUndefinedSymbols(parsedAst)
      expect(result).toEqual(new Set(['x', 'y']))
    })

    it('should return empty set when all symbols are defined', () => {
      const lits = new Lits()
      const result = lits.getUndefinedSymbols('1 + 2')
      expect(result).toEqual(new Set())
    })

    it('should return empty set for import expression', () => {
      const lits = new Lits({ modules: [vectorModule] })
      const result = lits.getUndefinedSymbols('let v = import(vector); v.sum([1, 2])')
      expect(result).toEqual(new Set())
    })
  })

  describe('transformSymbols', () => {
    it('should transform symbol tokens', () => {
      const lits = new Lits()
      const tokenStream = lits.tokenize('x + y')
      const transformed = lits.transformSymbols(tokenStream, s => s === 'x' ? 'a' : s)
      const result = lits.untokenize(transformed)
      expect(result).toBe('a + y')
    })
  })

  describe('untokenize', () => {
    it('should convert token stream back to source code', () => {
      const lits = new Lits()
      const tokenStream = lits.tokenize('1 + 2')
      const result = lits.untokenize(tokenStream)
      expect(result).toBe('1 + 2')
    })
  })

  describe('tokenize with minify', () => {
    it('should minify token stream when minify option is true', () => {
      const lits = new Lits()
      const normal = lits.tokenize('1  +  2')
      const minified = lits.tokenize('1  +  2', { minify: true })
      expect(minified.tokens.length).toBeLessThanOrEqual(normal.tokens.length)
    })
  })
})
