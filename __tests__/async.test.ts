import { describe, expect, it } from 'vitest'
import { Lits } from '../src/Lits/Lits'
import { collectionUtilsModule } from '../src/builtin/modules/collection'
import { gridModule } from '../src/builtin/modules/grid'
import { numberTheoryModule } from '../src/builtin/modules/number-theory'
import { sequenceUtilsModule } from '../src/builtin/modules/sequence'
import { vectorModule } from '../src/builtin/modules/vector'
import type { LitsFunction } from '../src/parser/types'

describe('async support', () => {
  describe('async.run with sync functions', () => {
    const lits = new Lits()

    it('should handle simple sync operations', async () => {
      expect(await lits.async.run('1 + 2')).toBe(3)
    })

    it('should handle map with sync functions', async () => {
      expect(await lits.async.run('map([1, 2, 3], inc)')).toEqual([2, 3, 4])
    })

    it('should handle filter with sync functions', async () => {
      expect(await lits.async.run('filter([1, 2, 3, 4, 5], odd?)')).toEqual([1, 3, 5])
    })

    it('should handle reduce with sync functions', async () => {
      expect(await lits.async.run('reduce([1, 2, 3], +, 0)')).toBe(6)
    })
  })

  describe('async.run with async JS functions', () => {
    it('should await async JS function result in simple expression', async () => {
      const lits = new Lits()
      const result = await lits.async.run('10 + foo()', {
        bindings: {
          foo: { fn: async () => 5 },
        },
      })
      expect(result).toBe(15)
    })

    it('should await async JS function in let binding', async () => {
      const lits = new Lits()
      const result = await lits.async.run('let x = foo(); x * 2', {
        bindings: {
          foo: { fn: async () => 21 },
        },
      })
      expect(result).toBe(42)
    })

    it('should await async JS function in if condition', async () => {
      const lits = new Lits()
      const result = await lits.async.run('if foo() then "yes" else "no" end', {
        bindings: {
          foo: { fn: async () => true },
        },
      })
      expect(result).toBe('yes')
    })

    it('should await async JS function in if then-branch', async () => {
      const lits = new Lits()
      const result = await lits.async.run('if true then foo() else "no" end', {
        bindings: {
          foo: { fn: async () => 'yes' },
        },
      })
      expect(result).toBe('yes')
    })

    it('should await async JS function in and expression', async () => {
      const lits = new Lits()
      const result = await lits.async.run('foo() && 42', {
        bindings: {
          foo: { fn: async () => true },
        },
      })
      expect(result).toBe(42)
    })

    it('should short-circuit async and when first value is falsy', async () => {
      const lits = new Lits()
      const result = await lits.async.run('foo() && bar()', {
        bindings: {
          foo: { fn: async () => false },
          bar: { fn: async () => { throw new Error('should not be called') } },
        },
      })
      expect(result).toBe(false)
    })

    it('should await async JS function in or expression', async () => {
      const lits = new Lits()
      const result = await lits.async.run('foo() || 42', {
        bindings: {
          foo: { fn: async () => null },
        },
      })
      expect(result).toBe(42)
    })

    it('should await async JS function with let and semicolons', async () => {
      const lits = new Lits()
      const result = await lits.async.run('let x = foo(); x + 1', {
        bindings: {
          foo: { fn: async () => 10 },
        },
      })
      expect(result).toBe(11)
    })

    it('should await async JS function in block', async () => {
      const lits = new Lits()
      const result = await lits.async.run('do foo(); bar() end', {
        bindings: {
          foo: { fn: async () => 1 },
          bar: { fn: async () => 2 },
        },
      })
      expect(result).toBe(2)
    })

    it('should await async JS function in try/catch', async () => {
      const lits = new Lits()
      const result = await lits.async.run('try foo() catch "caught" end', {
        bindings: {
          foo: {
            fn: async () => {
              throw new Error('async error')
            },
          },
        },
      })
      expect(result).toBe('caught')
    })

    it('should handle async inside cond', async () => {
      const lits = new Lits()
      const result = await lits.async.run('cond case false then 1 case foo() then 2 case true then 3 end', {
        bindings: {
          foo: { fn: async () => true },
        },
      })
      expect(result).toBe(2)
    })

    it('should handle async in pipe operator', async () => {
      const lits = new Lits()
      const result = await lits.async.run('5 |> foo |> inc', {
        bindings: {
          foo: { fn: async (x: number) => x * 2, arity: { min: 1, max: 1 } },
        },
      })
      expect(result).toBe(11)
    })

    it('should handle async in apply', async () => {
      const lits = new Lits()
      const result = await lits.async.run('apply(foo, [1, 2, 3])', {
        bindings: {
          foo: { fn: async (...args: number[]) => args.reduce((a, b) => a + b, 0) },
        },
      })
      expect(result).toBe(6)
    })
  })

  describe('run() throws on async result', () => {
    it('should throw when sync run() encounters async JS function', () => {
      const lits = new Lits()
      expect(() => lits.run('foo()', {
        bindings: {
          foo: { fn: async () => 5 },
        },
      })).toThrow('Unexpected async result')
    })
  })

  describe('async with HOFs', () => {
    it('should handle async in map callback', async () => {
      const lits = new Lits()
      const result = await lits.async.run('map([1, 2, 3], foo)', {
        bindings: {
          foo: { fn: async (x: number) => x * 10, arity: { min: 1, max: 1 } },
        },
      })
      expect(result).toEqual([10, 20, 30])
    })

    it('should handle async in filter callback', async () => {
      const lits = new Lits()
      const result = await lits.async.run('filter([1, 2, 3, 4, 5], foo)', {
        bindings: {
          foo: { fn: async (x: number) => x > 3, arity: { min: 1, max: 1 } },
        },
      })
      expect(result).toEqual([4, 5])
    })

    it('should handle async in reduce callback', async () => {
      const lits = new Lits()
      const result = await lits.async.run('reduce([1, 2, 3], foo, 0)', {
        bindings: {
          foo: { fn: async (acc: number, x: number) => acc + x, arity: { min: 2, max: 2 } },
        },
      })
      expect(result).toBe(6)
    })

    it('should handle async in some callback', async () => {
      const lits = new Lits()
      const result = await lits.async.run('some([1, 2, 3, 4], foo)', {
        bindings: {
          foo: { fn: async (x: number) => x > 3, arity: { min: 1, max: 1 } },
        },
      })
      expect(result).toBe(4)
    })
  })

  describe('async with recur and loop', () => {
    it('should handle async function called from loop/recur', async () => {
      const lits = new Lits()
      const result = await lits.async.run(`
        loop(i = 0, acc = 0) -> do
          if i >= 5
            then acc
            else recur(i + 1, acc + foo(i))
          end
        end
      `, {
        bindings: {
          foo: { fn: async (x: number) => x * 2, arity: { min: 1, max: 1 } },
        },
      })
      // sum = 0*2 + 1*2 + 2*2 + 3*2 + 4*2 = 0 + 2 + 4 + 6 + 8 = 20
      expect(result).toBe(20)
    })
  })

  describe('async error handling', () => {
    it('should propagate async errors', async () => {
      const lits = new Lits()
      await expect(lits.async.run('foo()', {
        bindings: {
          foo: {
            fn: async () => {
              throw new Error('async failure')
            },
          },
        },
      })).rejects.toThrow('async failure')
    })

    it('should catch async errors in try/catch', async () => {
      const lits = new Lits()
      const result = await lits.async.run('try foo() catch "handled" end', {
        bindings: {
          foo: {
            fn: async () => {
              throw new Error('async error')
            },
          },
        },
      })
      expect(result).toBe('handled')
    })

    it('should bind error variable from async throw', async () => {
      const lits = new Lits()
      const result = await lits.async.run('try foo() catch(err) err.message end', {
        bindings: {
          foo: {
            fn: async () => {
              throw new Error('async boom')
            },
          },
        },
      })
      expect(result).toBe('Native function threw: "async boom"')
    })

    it('should catch async error after sync expressions in try body', async () => {
      const lits = new Lits()
      const result = await lits.async.run(`
        try
          let x = 10;
          let y = foo();
          x + y
        catch
          "caught"
        end
      `, {
        bindings: {
          foo: {
            fn: async () => {
              throw new Error('mid-body error')
            },
          },
        },
      })
      expect(result).toBe('caught')
    })

    it('should wrap sync and async native errors identically', async () => {
      const lits = new Lits()
      const syncResult = await lits.async.run('try syncFoo() catch(err) err.message end', {
        bindings: {
          syncFoo: {
            fn: () => {
              throw new Error('kaboom')
            },
          },
        },
      })
      const asyncResult = await lits.async.run('try asyncFoo() catch(err) err.message end', {
        bindings: {
          asyncFoo: {
            fn: async () => {
              throw new Error('kaboom')
            },
          },
        },
      })
      expect(syncResult).toBe('Native function threw: "kaboom"')
      expect(asyncResult).toBe(syncResult)
    })

    it('should catch async error inside map within try', async () => {
      const lits = new Lits()
      const result = await lits.async.run(`
        try
          map([1, 2, 3], x -> foo(x))
        catch(err)
          err.message
        end
      `, {
        bindings: {
          foo: {
            fn: async (x: number) => {
              if (x === 2)
                throw new Error('bad value')
              return x
            },
            arity: { min: 1, max: 1 },
          },
        },
      })
      expect(result).toBe('Native function threw: "bad value"')
    })
  })

  describe('async with multiple sequential async calls', () => {
    it('should evaluate async calls sequentially', async () => {
      const callOrder: number[] = []
      const lits = new Lits()
      const result = await lits.async.run('foo(1) + foo(2) + foo(3)', {
        bindings: {
          foo: {
            fn: async (x: number) => {
              await new Promise(resolve => setTimeout(resolve, 10))
              callOrder.push(x)
              return x
            },
            arity: { min: 1, max: 1 },
          },
        },
      })
      expect(result).toBe(6)
      expect(callOrder).toEqual([1, 2, 3])
    })
  })

  describe('async sequence module functions', () => {
    const asyncDouble = {
      fn: async (x: number) => x * 2,
      arity: { min: 1, max: 1 },
    }
    const asyncIsEven = {
      fn: async (x: number) => x % 2 === 0,
      arity: { min: 1, max: 1 },
    }
    const asyncFirst = {
      fn: async (x: string) => x[0],
      arity: { min: 1, max: 1 },
    }

    it('position with async predicate', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      const result = await lits.async.run('let su = import(sequence); su.position([1, 2, 3, 4], isEven)', {
        bindings: { isEven: asyncIsEven },
      })
      expect(result).toBe(1)
    })

    it('position with async predicate matching first element', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      const result = await lits.async.run('let su = import(sequence); su.position([2, 3, 4], isEven)', {
        bindings: { isEven: asyncIsEven },
      })
      expect(result).toBe(0)
    })

    it('position with async predicate matching no element', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      const result = await lits.async.run('let su = import(sequence); su.position([1, 3, 5], isEven)', {
        bindings: { isEven: asyncIsEven },
      })
      expect(result).toBe(null)
    })

    it('sort-by with async key function', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      const result = await lits.async.run('let su = import(sequence); su.sort-by([3, 1, 2], dbl)', {
        bindings: { dbl: asyncDouble },
      })
      expect(result).toEqual([1, 2, 3])
    })

    it('take-while with async predicate', async () => {
      const lits = new Lits()
      const result = await lits.async.run('take-while([2, 4, 5, 6], isEven)', {
        bindings: { isEven: asyncIsEven },
      })
      expect(result).toEqual([2, 4])
    })

    it('take-while with async predicate on string', async () => {
      const lits = new Lits()
      const result = await lits.async.run('take-while("aabbc", isA)', {
        bindings: { isA: { fn: async (c: unknown) => c === 'a' } },
      })
      expect(result).toBe('aa')
    })

    it('drop-while with async predicate', async () => {
      const lits = new Lits()
      const result = await lits.async.run('drop-while([2, 4, 5, 6], isEven)', {
        bindings: { isEven: asyncIsEven },
      })
      expect(result).toEqual([5, 6])
    })

    it('drop-while with async predicate on string', async () => {
      const lits = new Lits()
      const result = await lits.async.run('drop-while("aabbc", isA)', {
        bindings: { isA: { fn: async (c: unknown) => c === 'a' } },
      })
      expect(result).toBe('bbc')
    })

    it('remove with async predicate', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      const result = await lits.async.run('let su = import(sequence); su.remove([1, 2, 3, 4], isEven)', {
        bindings: { isEven: asyncIsEven },
      })
      expect(result).toEqual([1, 3])
    })

    it('split-with with async predicate', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      const result = await lits.async.run('let su = import(sequence); su.split-with([2, 4, 5, 6], isEven)', {
        bindings: { isEven: asyncIsEven },
      })
      expect(result).toEqual([[2, 4], [5, 6]])
    })

    it('group-by with async key function', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      const result = await lits.async.run('let su = import(sequence); su.group-by(["ab", "ac", "bc"], getFirst)', {
        bindings: { getFirst: asyncFirst },
      })
      expect(result).toEqual({ a: ['ab', 'ac'], b: ['bc'] })
    })

    it('partition-by with async function', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      const result = await lits.async.run('let su = import(sequence); su.partition-by([1, 1, 2, 2, 3], isEven)', {
        bindings: { isEven: asyncIsEven },
      })
      expect(result).toEqual([[1, 1], [2, 2], [3]])
    })
  })

  describe('async collection module functions', () => {
    const asyncInc = {
      fn: async (x: number) => (x ?? 0) + 1,
      arity: { min: 1, max: 1 },
    }
    const asyncIsEven = {
      fn: async (x: number) => x % 2 === 0,
      arity: { min: 1, max: 1 },
    }
    const asyncDouble = {
      fn: async (x: number) => x * 2,
      arity: { min: 1, max: 1 },
    }

    it('filteri with async predicate', async () => {
      const lits = new Lits({ modules: [collectionUtilsModule] })
      const result = await lits.async.run('let cu = import(collection); cu.filteri([1, 2, 3, 4], (x, i) -> isEven(x))', {
        bindings: { isEven: asyncIsEven },
      })
      expect(result).toEqual([2, 4])
    })

    it('mapi with async function', async () => {
      const lits = new Lits({ modules: [collectionUtilsModule] })
      const result = await lits.async.run('let cu = import(collection); cu.mapi([1, 2, 3], (x, i) -> dbl(x))', {
        bindings: { dbl: asyncDouble },
      })
      expect(result).toEqual([2, 4, 6])
    })

    it('update with async function', async () => {
      const lits = new Lits({ modules: [collectionUtilsModule] })
      const result = await lits.async.run('let cu = import(collection); cu.update({ a: 1 }, "a", incr)', {
        bindings: { incr: asyncInc },
      })
      expect(result).toEqual({ a: 2 })
    })

    it('every? with async predicate', async () => {
      const lits = new Lits({ modules: [collectionUtilsModule] })
      const result = await lits.async.run('let cu = import(collection); cu.every?([2, 4, 6], isEven)', {
        bindings: { isEven: asyncIsEven },
      })
      expect(result).toBe(true)
    })

    it('every? with async predicate failing on first element', async () => {
      const lits = new Lits({ modules: [collectionUtilsModule] })
      const result = await lits.async.run('let cu = import(collection); cu.every?([1, 2, 4], isEven)', {
        bindings: { isEven: asyncIsEven },
      })
      expect(result).toBe(false)
    })

    it('any? with async predicate', async () => {
      const lits = new Lits({ modules: [collectionUtilsModule] })
      const result = await lits.async.run('let cu = import(collection); cu.any?([1, 3, 4], isEven)', {
        bindings: { isEven: asyncIsEven },
      })
      expect(result).toBe(true)
    })

    it('not-any? with async predicate', async () => {
      const lits = new Lits({ modules: [collectionUtilsModule] })
      const result = await lits.async.run('let cu = import(collection); cu.not-any?([1, 3, 5], isEven)', {
        bindings: { isEven: asyncIsEven },
      })
      expect(result).toBe(true)
    })

    it('not-every? with async predicate', async () => {
      const lits = new Lits({ modules: [collectionUtilsModule] })
      const result = await lits.async.run('let cu = import(collection); cu.not-every?([2, 3, 4], isEven)', {
        bindings: { isEven: asyncIsEven },
      })
      expect(result).toBe(true)
    })
  })

  describe('async grid module functions', () => {
    const asyncIsPositive = {
      fn: async (x: number) => x > 0,
      arity: { min: 1, max: 1 },
    }
    const asyncDouble = {
      fn: async (x: number) => x * 2,
      arity: { min: 1, max: 1 },
    }
    const asyncMul = {
      fn: async (r: number, c: number) => r * 10 + c,
      arity: { min: 2, max: 2 },
    }
    const asyncAllPositive = {
      fn: async (row: number[]) => row.every(x => x > 0),
      arity: { min: 1, max: 1 },
    }

    it('every? with async predicate', async () => {
      const lits = new Lits({ modules: [gridModule] })
      const result = await lits.async.run('let g = import(grid); g.every?(g.fill(2, 2, 1), isPos)', {
        bindings: { isPos: asyncIsPositive },
      })
      expect(result).toBe(true)
    })

    it('some? with async predicate', async () => {
      const lits = new Lits({ modules: [gridModule] })
      const result = await lits.async.run('let g = import(grid); g.some?(g.fill(2, 2, 0), isPos)', {
        bindings: { isPos: asyncIsPositive },
      })
      expect(result).toBe(false)
    })

    it('every-row? with async predicate', async () => {
      const lits = new Lits({ modules: [gridModule] })
      const result = await lits.async.run('let g = import(grid); g.every-row?(g.fill(2, 2, 1), allPos)', {
        bindings: { allPos: asyncAllPositive },
      })
      expect(result).toBe(true)
    })

    it('some-row? with async predicate', async () => {
      const lits = new Lits({ modules: [gridModule] })
      const result = await lits.async.run('let g = import(grid); g.some-row?([[1, 2], [-1, -2]], allPos)', {
        bindings: { allPos: asyncAllPositive },
      })
      expect(result).toBe(true)
    })

    it('generate with async generator', async () => {
      const lits = new Lits({ modules: [gridModule] })
      const result = await lits.async.run('let g = import(grid); g.generate(2, 3, mul)', {
        bindings: { mul: asyncMul },
      })
      expect(result).toEqual([[0, 1, 2], [10, 11, 12]])
    })

    it('map with async function', async () => {
      const lits = new Lits({ modules: [gridModule] })
      const result = await lits.async.run('let g = import(grid); g.map([[1, 2], [3, 4]], dbl)', {
        bindings: { dbl: asyncDouble },
      })
      expect(result).toEqual([[2, 4], [6, 8]])
    })

    it('mapi with async function', async () => {
      const lits = new Lits({ modules: [gridModule] })
      const result = await lits.async.run('let g = import(grid); g.mapi([[1, 2], [3, 4]], (val, r, c) -> mul(r, c))', {
        bindings: { mul: asyncMul },
      })
      expect(result).toEqual([[0, 1], [10, 11]])
    })
  })

  describe('async vector module functions', () => {
    it('generate with async generator', async () => {
      const lits = new Lits({ modules: [vectorModule] })
      const result = await lits.async.run('let v = import(vector); v.generate(4, dbl)', {
        bindings: {
          dbl: {
            fn: async (x: number) => x * 2,
            arity: { min: 1, max: 1 },
          },
        },
      })
      expect(result).toEqual([0, 2, 4, 6])
    })
  })

  describe('async bugs — number-theory take-while', () => {
    it('should correctly evaluate async predicate in take-while', async () => {
      const lits = new Lits({ modules: [numberTheoryModule] })
      // fibonacci-take-while with async predicate: take fib numbers while < 10
      // Fibonacci: 0, 1, 1, 2, 3, 5, 8, 13, ...  → should return [0, 1, 1, 2, 3, 5, 8]
      const result = await lits.async.run(
        'let nt = import(number-theory); nt.fibonacci-take-while(lessThan10)',
        {
          bindings: {
            lessThan10: {
              fn: async (x: number) => x < 10,
              arity: { min: 1, max: 2 },
            },
          },
        },
      )
      expect(result).toEqual([0, 1, 1, 2, 3, 5, 8])
    })
  })

  describe('async — destructuring default values', () => {
    it('should resolve async default in array destructuring via let', async () => {
      const lits = new Lits()
      const result = await lits.async.run(
        'let [a = getFive()] = []; a + 1',
        {
          bindings: {
            getFive: { fn: async () => 5 },
          },
        },
      )
      expect(result).toBe(6)
    })

    it('should resolve async default in object destructuring via let', async () => {
      const lits = new Lits()
      const result = await lits.async.run(
        'let { a = getFive() } = {}; a + 1',
        {
          bindings: {
            getFive: { fn: async () => 5 },
          },
        },
      )
      expect(result).toBe(6)
    })

    it('should resolve async default in array destructuring via def', async () => {
      const lits = new Lits()
      const result = await lits.async.run(
        'export let [a = getFive()] = []; a + 1',
        {
          bindings: {
            getFive: { fn: async () => 5 },
          },
        },
      )
      expect(result).toBe(6)
    })

    it('should resolve async default in object destructuring via def', async () => {
      const lits = new Lits()
      const result = await lits.async.run(
        'export let { a = getFive() } = {}; a + 1',
        {
          bindings: {
            getFive: { fn: async () => 5 },
          },
        },
      )
      expect(result).toBe(6)
    })

    it('should resolve async default in user-defined function argument', async () => {
      const lits = new Lits()
      const result = await lits.async.run(
        `
          let foo = ([x = getFive()]) -> x * 2;
          foo([])
        `,
        {
          bindings: {
            getFive: { fn: async () => 5 },
          },
        },
      )
      expect(result).toBe(10)
    })

    it('should resolve async default in loop/recur bindings', async () => {
      const lits = new Lits()
      const result = await lits.async.run(
        `
          loop ({ x = getFive() } = {}) ->
            if x > 0 then
              recur({ x: x - 1 })
            else
              x
            end
        `,
        {
          bindings: {
            getFive: { fn: async () => 3 },
          },
        },
      )
      expect(result).toBe(0)
    })

    it('should resolve async default in for loop let-bindings', async () => {
      const lits = new Lits()
      const result = await lits.async.run(
        `
          for (pair in [[1, 2], [3, 4], [5]] let [x, y = getFive()] = pair) -> x + y
        `,
        {
          bindings: {
            getFive: { fn: async () => 99 },
          },
        },
      )
      expect(result).toEqual([3, 7, 104])
    })

    it('should resolve async default in doseq let-bindings', async () => {
      const lits = new Lits()
      // doseq returns null, but we verify it completes without error
      // and bindings resolve properly
      const result = await lits.async.run(
        `
          doseq (pair in [[1, 2], [5]] let [x, y = getFive()] = pair) -> x + y;
          true
        `,
        {
          bindings: {
            getFive: { fn: async () => 99 },
          },
        },
      )
      expect(result).toBe(true)
    })
  })

  describe('async — bernoulli take-while', () => {
    it('should handle async predicate in bernoulli take-while', async () => {
      const lits = new Lits({ modules: [numberTheoryModule] })
      const result = await lits.async.run(
        'let nt = import(number-theory); nt.bernoulli-take-while(asyncPred)',
        {
          bindings: {
            asyncPred: { fn: async (_value: unknown, index: unknown) => (index as number) < 5 },
          },
        },
      )
      expect(result).toHaveLength(5)
    })
  })

  describe('async.context', () => {
    it('should return context after async evaluation', async () => {
      const lits = new Lits()
      const ctx = await lits.async.context('export let x = foo()', {
        bindings: {
          foo: { fn: async () => 42 },
        },
      })
      expect(ctx.x).toEqual({ value: 42 })
    })

    it('should accept a pre-parsed AST', async () => {
      const lits = new Lits({ debug: true })
      const ast = lits.parse(lits.tokenize('export let x = 10'))
      const ctx = await lits.async.context(ast)
      expect(ctx.x).toEqual({ value: 10 })
    })
  })

  describe('async.apply', () => {
    it('should apply a lits function with async.apply', async () => {
      const lits = new Lits()
      const fn = lits.run('-> $ + 1') as LitsFunction
      const result = await lits.async.apply(fn, [9])
      expect(result).toBe(10)
    })
  })

  describe('sync context() throws on async', () => {
    it('should throw when context() encounters async result', () => {
      const lits = new Lits()
      expect(() => lits.context('export let x = foo()', {
        bindings: {
          foo: { fn: async () => 42 },
        },
      })).toThrow(TypeError)
    })
  })

  describe('sort with async comparator throws', () => {
    it('should throw when sort uses async comparator on strings', async () => {
      const lits = new Lits()
      await expect(lits.async.run('sort("cba", asyncCmp)', {
        bindings: {
          asyncCmp: { fn: async (a: unknown, b: unknown) => (a as string).localeCompare(b as string) },
        },
      })).rejects.toThrow(TypeError)
    })

    it('should throw when sort uses async comparator on arrays', async () => {
      const lits = new Lits()
      await expect(lits.async.run('sort([3, 1, 2], asyncCmp)', {
        bindings: {
          asyncCmp: { fn: async (a: unknown, b: unknown) => (a as number) - (b as number) },
        },
      })).rejects.toThrow(TypeError)
    })

    it('should throw when sort-by uses async comparator', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      await expect(lits.async.run('let su = import(sequence); su.sort-by([3, 1, 2], identity, asyncCmp)', {
        bindings: {
          asyncCmp: { fn: async (a: unknown, b: unknown) => (a as number) - (b as number) },
        },
      })).rejects.toThrow(TypeError)
    })
  })

  describe('async native function errors', () => {
    it('should handle async native fn rejecting with a string', async () => {
      const lits = new Lits()
      await expect(lits.async.run('boom()', {
        bindings: {
          // eslint-disable-next-line ts/no-throw-literal
          boom: { fn: async () => { throw 'string error' } },
        },
      })).rejects.toThrow('string error')
    })

    it('should handle async native fn rejecting with an Error object', async () => {
      const lits = new Lits()
      await expect(lits.async.run('boom()', {
        bindings: {
          boom: { fn: async () => { throw new Error('object error') } },
        },
      })).rejects.toThrow('object error')
    })
  })

  describe('async recur in user-defined function', () => {
    it('should handle async recur in user-defined function body', async () => {
      const lits = new Lits()
      const result = await lits.async.run(`
        let countdown = (n) -> if n <= 0 then asyncZero() else recur(n - 1) end;
        countdown(3)
      `, {
        bindings: {
          asyncZero: { fn: async () => 0 },
        },
      })
      expect(result).toBe(0)
    })
  })

  describe('async for loop without let bindings', () => {
    it('should work with async predicate in for body without let', async () => {
      const lits = new Lits()
      const result = await lits.async.run('for (x in [1, 2, 3]) -> asyncDouble(x)', {
        bindings: {
          asyncDouble: { fn: async (x: unknown) => (x as number) * 2 },
        },
      })
      expect(result).toEqual([2, 4, 6])
    })
  })

  describe('async native fn rejecting with non-string non-object', () => {
    it('should show <no message> for rejection with a number', async () => {
      const lits = new Lits()
      await expect(lits.async.run('boom()', {
        bindings: {
          // eslint-disable-next-line prefer-promise-reject-errors
          boom: { fn: () => Promise.reject(42) },
        },
      })).rejects.toThrow('<no message>')
    })
  })

  describe('async recur with async body in user-defined function', () => {
    it('should handle recur after async call in function body', async () => {
      const lits = new Lits()
      const result = await lits.async.run(`
        let f = (n) -> do
          asyncFn();
          if n <= 0 then 0 else recur(n - 1) end
        end;
        f(3)
      `, {
        bindings: { asyncFn: { fn: async () => 1 } },
      })
      expect(result).toBe(0)
    })
  })

  describe('async loop/recur', () => {
    it('should handle async loop body with recur', async () => {
      const lits = new Lits()
      const result = await lits.async.run(`
        loop (n = 3, acc = 0) -> do
          let v = asyncFn(n);
          if n <= 0 then acc else recur(n - 1, acc + v) end
        end
      `, {
        bindings: { asyncFn: { fn: async (x: unknown) => (x as number) * 2, arity: { min: 1, max: 1 } } },
      })
      // 3*2 + 2*2 + 1*2 = 6 + 4 + 2 = 12
      expect(result).toBe(12)
    })

    it('should propagate non-recur errors from async loop body', async () => {
      const lits = new Lits()
      await expect(lits.async.run(`
        loop (n = 0) -> do
          asyncFn();
          throw("loop-error")
        end
      `, {
        bindings: { asyncFn: { fn: async () => 1 } },
      })).rejects.toThrow('loop-error')
    })

    it('should handle async binding defaults during recur', async () => {
      const lits = new Lits()
      const result = await lits.async.run(`
        loop ({ x, y = asyncFn() } = { x: 3 }) ->
          if x <= 0 then y
          else recur({ x: x - 1 })
          end
      `, {
        bindings: { asyncFn: { fn: async () => 42 } },
      })
      expect(result).toBe(42)
    })

    it('should throw on recur param count mismatch in async loop', async () => {
      const lits = new Lits()
      await expect(lits.async.run(`
        loop (n = 3, acc = 0) -> do
          asyncFn();
          recur(1)
        end
      `, {
        bindings: { asyncFn: { fn: async () => 1 } },
      })).rejects.toThrow('recur expected 2 parameters, got 1')
    })

    it('should propagate non-recur errors through iterate in async loop', async () => {
      const lits = new Lits()
      await expect(lits.async.run(`
        loop ({ x = asyncFn() } = { x: 1 }) ->
          if x <= 0 then throw("done")
          else recur({})
          end
      `, {
        bindings: { asyncFn: { fn: async () => 0 } },
      })).rejects.toThrow('done')
    })

    it('should handle multiple bindings with async defaults during recur', async () => {
      const lits = new Lits()
      const result = await lits.async.run(`
        loop ({ x = asyncFn() } = { x: 1 }, y = 0) ->
          if x <= 0 then y
          else recur({}, y + 1)
          end
      `, {
        bindings: { asyncFn: { fn: async () => 0 } },
      })
      expect(result).toBe(1)
    })
  })
})
