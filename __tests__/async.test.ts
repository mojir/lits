import { describe, expect, it } from 'vitest'
import { Lits } from '../src/Lits/Lits'
import { collectionUtilsModule } from '../src/builtin/modules/collection'
import { gridModule } from '../src/builtin/modules/grid'
import { numberTheoryModule } from '../src/builtin/modules/number-theory'
import { sequenceUtilsModule } from '../src/builtin/modules/sequence'
import { vectorModule } from '../src/builtin/modules/vector'

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
        jsFunctions: {
          foo: { fn: async () => 5 },
        },
      })
      expect(result).toBe(15)
    })

    it('should await async JS function in let binding', async () => {
      const lits = new Lits()
      const result = await lits.async.run('let x = foo(); x * 2', {
        jsFunctions: {
          foo: { fn: async () => 21 },
        },
      })
      expect(result).toBe(42)
    })

    it('should await async JS function in if condition', async () => {
      const lits = new Lits()
      const result = await lits.async.run('if foo() then "yes" else "no" end', {
        jsFunctions: {
          foo: { fn: async () => true },
        },
      })
      expect(result).toBe('yes')
    })

    it('should await async JS function in if then-branch', async () => {
      const lits = new Lits()
      const result = await lits.async.run('if true then foo() else "no" end', {
        jsFunctions: {
          foo: { fn: async () => 'yes' },
        },
      })
      expect(result).toBe('yes')
    })

    it('should await async JS function in and expression', async () => {
      const lits = new Lits()
      const result = await lits.async.run('foo() && 42', {
        jsFunctions: {
          foo: { fn: async () => true },
        },
      })
      expect(result).toBe(42)
    })

    it('should short-circuit async and when first value is falsy', async () => {
      const lits = new Lits()
      const result = await lits.async.run('foo() && bar()', {
        jsFunctions: {
          foo: { fn: async () => false },
          bar: { fn: async () => { throw new Error('should not be called') } },
        },
      })
      expect(result).toBe(false)
    })

    it('should await async JS function in or expression', async () => {
      const lits = new Lits()
      const result = await lits.async.run('foo() || 42', {
        jsFunctions: {
          foo: { fn: async () => null },
        },
      })
      expect(result).toBe(42)
    })

    it('should await async JS function with let and semicolons', async () => {
      const lits = new Lits()
      const result = await lits.async.run('let x = foo(); x + 1', {
        jsFunctions: {
          foo: { fn: async () => 10 },
        },
      })
      expect(result).toBe(11)
    })

    it('should await async JS function in block', async () => {
      const lits = new Lits()
      const result = await lits.async.run('do foo(); bar() end', {
        jsFunctions: {
          foo: { fn: async () => 1 },
          bar: { fn: async () => 2 },
        },
      })
      expect(result).toBe(2)
    })

    it('should await async JS function in try/catch', async () => {
      const lits = new Lits()
      const result = await lits.async.run('try foo() catch "caught" end', {
        jsFunctions: {
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
        jsFunctions: {
          foo: { fn: async () => true },
        },
      })
      expect(result).toBe(2)
    })

    it('should handle async in pipe operator', async () => {
      const lits = new Lits()
      const result = await lits.async.run('5 |> foo |> inc', {
        jsFunctions: {
          foo: { fn: async (x: number) => x * 2, arity: { min: 1, max: 1 } },
        },
      })
      expect(result).toBe(11)
    })

    it('should handle async in apply', async () => {
      const lits = new Lits()
      const result = await lits.async.run('apply(foo, [1, 2, 3])', {
        jsFunctions: {
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
        jsFunctions: {
          foo: { fn: async () => 5 },
        },
      })).toThrow('Unexpected async result')
    })
  })

  describe('async with HOFs', () => {
    it('should handle async in map callback', async () => {
      const lits = new Lits()
      const result = await lits.async.run('map([1, 2, 3], foo)', {
        jsFunctions: {
          foo: { fn: async (x: number) => x * 10, arity: { min: 1, max: 1 } },
        },
      })
      expect(result).toEqual([10, 20, 30])
    })

    it('should handle async in filter callback', async () => {
      const lits = new Lits()
      const result = await lits.async.run('filter([1, 2, 3, 4, 5], foo)', {
        jsFunctions: {
          foo: { fn: async (x: number) => x > 3, arity: { min: 1, max: 1 } },
        },
      })
      expect(result).toEqual([4, 5])
    })

    it('should handle async in reduce callback', async () => {
      const lits = new Lits()
      const result = await lits.async.run('reduce([1, 2, 3], foo, 0)', {
        jsFunctions: {
          foo: { fn: async (acc: number, x: number) => acc + x, arity: { min: 2, max: 2 } },
        },
      })
      expect(result).toBe(6)
    })

    it('should handle async in some callback', async () => {
      const lits = new Lits()
      const result = await lits.async.run('some([1, 2, 3, 4], foo)', {
        jsFunctions: {
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
        jsFunctions: {
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
        jsFunctions: {
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
        jsFunctions: {
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
        jsFunctions: {
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
        jsFunctions: {
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
        jsFunctions: {
          syncFoo: {
            fn: () => {
              throw new Error('kaboom')
            },
          },
        },
      })
      const asyncResult = await lits.async.run('try asyncFoo() catch(err) err.message end', {
        jsFunctions: {
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
        jsFunctions: {
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
        jsFunctions: {
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
      const result = await lits.async.run('let su = import("sequence"); su.position([1, 2, 3, 4], isEven)', {
        jsFunctions: { isEven: asyncIsEven },
      })
      expect(result).toBe(1)
    })

    it('sort-by with async key function', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      const result = await lits.async.run('let su = import("sequence"); su.sort-by([3, 1, 2], dbl)', {
        jsFunctions: { dbl: asyncDouble },
      })
      expect(result).toEqual([1, 2, 3])
    })

    it('take-while with async predicate', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      const result = await lits.async.run('let su = import("sequence"); su.take-while([2, 4, 5, 6], isEven)', {
        jsFunctions: { isEven: asyncIsEven },
      })
      expect(result).toEqual([2, 4])
    })

    it('drop-while with async predicate', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      const result = await lits.async.run('let su = import("sequence"); su.drop-while([2, 4, 5, 6], isEven)', {
        jsFunctions: { isEven: asyncIsEven },
      })
      expect(result).toEqual([5, 6])
    })

    it('remove with async predicate', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      const result = await lits.async.run('let su = import("sequence"); su.remove([1, 2, 3, 4], isEven)', {
        jsFunctions: { isEven: asyncIsEven },
      })
      expect(result).toEqual([1, 3])
    })

    it('split-with with async predicate', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      const result = await lits.async.run('let su = import("sequence"); su.split-with([2, 4, 5, 6], isEven)', {
        jsFunctions: { isEven: asyncIsEven },
      })
      expect(result).toEqual([[2, 4], [5, 6]])
    })

    it('group-by with async key function', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      const result = await lits.async.run('let su = import("sequence"); su.group-by(["ab", "ac", "bc"], getFirst)', {
        jsFunctions: { getFirst: asyncFirst },
      })
      expect(result).toEqual({ a: ['ab', 'ac'], b: ['bc'] })
    })

    it('partition-by with async function', async () => {
      const lits = new Lits({ modules: [sequenceUtilsModule] })
      const result = await lits.async.run('let su = import("sequence"); su.partition-by([1, 1, 2, 2, 3], isEven)', {
        jsFunctions: { isEven: asyncIsEven },
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
      const result = await lits.async.run('let cu = import("collection"); cu.filteri([1, 2, 3, 4], (x, i) -> isEven(x))', {
        jsFunctions: { isEven: asyncIsEven },
      })
      expect(result).toEqual([2, 4])
    })

    it('mapi with async function', async () => {
      const lits = new Lits({ modules: [collectionUtilsModule] })
      const result = await lits.async.run('let cu = import("collection"); cu.mapi([1, 2, 3], (x, i) -> dbl(x))', {
        jsFunctions: { dbl: asyncDouble },
      })
      expect(result).toEqual([2, 4, 6])
    })

    it('update with async function', async () => {
      const lits = new Lits({ modules: [collectionUtilsModule] })
      const result = await lits.async.run('let cu = import("collection"); cu.update({ a: 1 }, "a", incr)', {
        jsFunctions: { incr: asyncInc },
      })
      expect(result).toEqual({ a: 2 })
    })

    it('every? with async predicate', async () => {
      const lits = new Lits({ modules: [collectionUtilsModule] })
      const result = await lits.async.run('let cu = import("collection"); cu.every?([2, 4, 6], isEven)', {
        jsFunctions: { isEven: asyncIsEven },
      })
      expect(result).toBe(true)
    })

    it('any? with async predicate', async () => {
      const lits = new Lits({ modules: [collectionUtilsModule] })
      const result = await lits.async.run('let cu = import("collection"); cu.any?([1, 3, 4], isEven)', {
        jsFunctions: { isEven: asyncIsEven },
      })
      expect(result).toBe(true)
    })

    it('not-any? with async predicate', async () => {
      const lits = new Lits({ modules: [collectionUtilsModule] })
      const result = await lits.async.run('let cu = import("collection"); cu.not-any?([1, 3, 5], isEven)', {
        jsFunctions: { isEven: asyncIsEven },
      })
      expect(result).toBe(true)
    })

    it('not-every? with async predicate', async () => {
      const lits = new Lits({ modules: [collectionUtilsModule] })
      const result = await lits.async.run('let cu = import("collection"); cu.not-every?([2, 3, 4], isEven)', {
        jsFunctions: { isEven: asyncIsEven },
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
      const result = await lits.async.run('let g = import("grid"); g.every?(g.fill(2, 2, 1), isPos)', {
        jsFunctions: { isPos: asyncIsPositive },
      })
      expect(result).toBe(true)
    })

    it('some? with async predicate', async () => {
      const lits = new Lits({ modules: [gridModule] })
      const result = await lits.async.run('let g = import("grid"); g.some?(g.fill(2, 2, 0), isPos)', {
        jsFunctions: { isPos: asyncIsPositive },
      })
      expect(result).toBe(false)
    })

    it('every-row? with async predicate', async () => {
      const lits = new Lits({ modules: [gridModule] })
      const result = await lits.async.run('let g = import("grid"); g.every-row?(g.fill(2, 2, 1), allPos)', {
        jsFunctions: { allPos: asyncAllPositive },
      })
      expect(result).toBe(true)
    })

    it('some-row? with async predicate', async () => {
      const lits = new Lits({ modules: [gridModule] })
      const result = await lits.async.run('let g = import("grid"); g.some-row?([[1, 2], [-1, -2]], allPos)', {
        jsFunctions: { allPos: asyncAllPositive },
      })
      expect(result).toBe(true)
    })

    it('generate with async generator', async () => {
      const lits = new Lits({ modules: [gridModule] })
      const result = await lits.async.run('let g = import("grid"); g.generate(2, 3, mul)', {
        jsFunctions: { mul: asyncMul },
      })
      expect(result).toEqual([[0, 1, 2], [10, 11, 12]])
    })

    it('map with async function', async () => {
      const lits = new Lits({ modules: [gridModule] })
      const result = await lits.async.run('let g = import("grid"); g.map([[1, 2], [3, 4]], dbl)', {
        jsFunctions: { dbl: asyncDouble },
      })
      expect(result).toEqual([[2, 4], [6, 8]])
    })

    it('mapi with async function', async () => {
      const lits = new Lits({ modules: [gridModule] })
      const result = await lits.async.run('let g = import("grid"); g.mapi([[1, 2], [3, 4]], (val, r, c) -> mul(r, c))', {
        jsFunctions: { mul: asyncMul },
      })
      expect(result).toEqual([[0, 1], [10, 11]])
    })
  })

  describe('async vector module functions', () => {
    it('generate with async generator', async () => {
      const lits = new Lits({ modules: [vectorModule] })
      const result = await lits.async.run('let v = import("vector"); v.generate(4, dbl)', {
        jsFunctions: {
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
        'let nt = import("number-theory"); nt.fibonacci-take-while(lessThan10)',
        {
          jsFunctions: {
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
          jsFunctions: {
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
          jsFunctions: {
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
          jsFunctions: {
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
          jsFunctions: {
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
          jsFunctions: {
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
          jsFunctions: {
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
          jsFunctions: {
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
          jsFunctions: {
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
        'let nt = import("number-theory"); nt.bernoulli-take-while(asyncPred)',
        {
          jsFunctions: {
            asyncPred: { fn: async (_value: unknown, index: unknown) => (index as number) < 5 },
          },
        },
      )
      expect(result).toHaveLength(5)
    })
  })
})
