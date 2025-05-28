/* eslint-disable no-console */
import { afterEach, beforeEach, describe, expect, it, test, vitest } from 'vitest'
import { Lits } from '../../../src/Lits/Lits'
import { LitsError, RecurSignal, UserDefinedError } from '../../../src/errors'

describe('functional functions.', () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    let oldLog: () => void
    let logSpy: (...args: unknown[]) => void
    beforeEach(() => {
      oldLog = console.log
      logSpy = vitest.fn()
      console.log = (...args) => {
        logSpy(...args)
      }
    })
    afterEach(() => {
      console.log = oldLog
    })
    describe('apply.', () => {
      it('samples.', () => {
        expect(lits.run('apply(+, [1, 2, 3, 4])')).toBe(10)
        expect(lits.run('+ apply [1, 2, 3, 4]')).toBe(10)
        expect(lits.run('apply(+, 1, 2, [3, 4])')).toBe(10)
        expect(() => lits.run('apply(+)')).toThrow(LitsError)
        expect(() => lits.run('apply(+, 2, 3)')).toThrow(LitsError)
      })
    })

    describe('identity.', () => {
      it('samples.', () => {
        expect(lits.run('identity("Albert")')).toBe('Albert')
        expect(lits.run('identity("")')).toBe('')
        expect(lits.run('identity(null)')).toBe(null)
        expect(lits.run('identity(false)')).toBe(false)
        expect(lits.run('identity(true)')).toBe(true)
        expect(lits.run('identity({ a: 1 })')).toEqual({ a: 1 })
        expect(lits.run('identity([1, 2, 3])')).toEqual([1, 2, 3])
        expect(() => lits.run('identity()')).toThrow(LitsError)
        expect(() => lits.run('identity(1, 2)')).toThrow(LitsError)
      })
    })

    describe('partial functions.', () => {
      it('samples.', () => {
        expect(lits.run('+(1, _)(2)')).toBe(3)
        expect(lits.run('+(1, _, _)(2, _)(2)')).toBe(5)
        expect(lits.run('+(_, _)(2, _)(2)')).toBe(4)
        expect(lits.run('+(_, _)(2, 2)')).toBe(4)
        expect(() => lits.run('+(_, _)(2)')).toThrow(LitsError)
        expect(() => lits.run('+(_, _)(2, 2, 2)')).toThrow(LitsError)
      })
    })

    describe('pipe |>', () => {
      it('samples.', () => {
        expect(lits.run('1 |> +(_, 2)')).toBe(3)
        expect(lits.run('|>(1, +(2, _))')).toBe(3)
        expect(lits.run(`range(10)
                           |> map(_, -> $ ^ 2) // [0, 1, 4, 9, 16, 25, 36, 49, 64, 81]
                           |> filter(_, odd?)  // [1, 9, 25, 49, 81]
                           |> reduce(_, +, 0)  // 165
                           |> sqrt             // 12.84523257866513
                           |> round(_, 2)`)).toBe(12.85)
      })
    })

    describe('comp.', () => {
      it('samples.', () => {
        expect(lits.run('let negative-quotient = comp(-, /); negative-quotient(9, 3)')).toBe(-3)
        expect(
          lits.run(`
        (
          -> apply(
            comp,
            first,
            repeat(rest, $2)
          )($1)
        )([1, 2, 3, 4, 5, 6, 7], 3)
      `),
        ).toBe(4)
        expect(lits.run('let x = { bar: { foo: 42 }}; comp("foo", "bar")(x)')).toBe(42)

        expect(lits.run('comp()(10)')).toBe(10)
        expect(lits.run('comp()(null)')).toBe(null)
        expect(lits.run('comp()({ "a": 10 })')).toEqual({ a: 10 })
        expect(lits.run('comp()(["x", 10, null])')).toEqual(['x', 10, null])
        expect(lits.run(`
let foo = comp(!, odd?);
[2, 3, 4, 5] filter foo`)).toEqual([2, 4])
        expect(() => lits.run('comp()(1, 2)')).toThrow(LitsError)
        expect(() => lits.run('comp(true)()')).toThrow(LitsError)
      })
    })

    describe('constanty.', () => {
      it('samples.', () => {
        expect(lits.run('constantly(10)(12, null, "x")')).toBe(10)
        expect(() => lits.run('constanty()')).toThrow(LitsError)
        expect(() => lits.run('constanty(10, 20)')).toThrow(LitsError)
      })
    })

    describe('juxt.', () => {
      it('samples.', () => {
        expect(lits.run('juxt(+, *, min, max)(3, 4, 6)')).toEqual([13, 72, 3, 6])
        expect(lits.run('juxt("a", "b")({ a: 1, b: 2, c: 3, d: 4})')).toEqual([1, 2])
        expect(lits.run('apply(juxt(+, *, min, max), range(1, 5))')).toEqual([10, 24, 1, 4])
        expect(() => lits.run('juxt(-> $, -> $2)')).toThrow() // Must accept same number of params
        // eslint-disable-next-line ts/no-unsafe-member-access
        expect((lits.run('juxt((x) -> x, (x, y = 1) -> x + y, (...c) -> 0)') as any).arity).toEqual({ min: 1, max: 1 })
        expect(() => lits.run('juxt()')).toThrow(LitsError)
      })
    })

    describe('complement.', () => {
      it('samples.', () => {
        expect(lits.run('complement(>)(4, 6)')).toBe(true)
        expect(lits.run('complement(==)(3, 3)')).toBe(false)
        expect(() => lits.run('complement()')).toThrow(LitsError)
        expect(() => lits.run('complement(>, <)')).toThrow(LitsError)
      })
    })

    describe('every-pred.', () => {
      it('samples.', () => {
        expect(lits.run('every-pred(string?, -> count($1) > 3)("Albert")')).toBe(true)
        expect(lits.run('every-pred(string?, -> count($1) > 3)("Albert", "Mojir")')).toBe(true)
        expect(lits.run('every-pred(string?, -> count($1) > 3)("Albert", "L", "Mojir")')).toBe(false)
        expect(lits.run('every-pred(string?, -> count($1) > 3)("Albert", [1, 2, 3, 4])')).toBe(false)
        expect(() => lits.run('every-pred()')).toThrow(LitsError)
      })
    })

    describe('some-pred.', () => {
      it('samples.', () => {
        expect(lits.run('some-pred(string?, -> count($1) > 3)("Albert", "M")')).toBe(true)
        expect(lits.run('some-pred(string?, -> count($1) > 3)("A", "M")')).toBe(true)
        expect(lits.run('some-pred(string?, -> count($1) > 3)([10, 20], [20, 10])')).toBe(false)
        expect(lits.run('some-pred(string?, -> count($1) > 3)("Albert", [10, 20])')).toBe(true)
        expect(() => lits.run('some-pred()')).toThrow(LitsError)
      })
    })

    describe('fnull.', () => {
      it('samples.', () => {
        expect(lits.run('fnull(+, 1, 2)(0, 0)')).toBe(0)
        expect(lits.run('fnull(+, 1, 2)(null, 0)')).toBe(1)
        expect(lits.run('fnull(+, 1, 2)(0, null)')).toBe(2)
        expect(lits.run('fnull(+, 1, 2)(null, null)')).toBe(3)
        expect(() => lits.run('fnull()')).toThrow(LitsError)
        expect(() => lits.run('fnull(+)')).toThrow(LitsError)
      })
    })

    describe('spread.', () => {
      it('samples.', () => {
        expect(lits.run(`
let params = [1, 2, 3];
+(...params)`)).toBe(6)
      })
      expect(() => lits.run(`
let params = {};
+(...params)`)).toThrow(LitsError)
    })

    describe('special expressions as normal expressions.', () => {
      test('samples.', () => {
        expect(lits.run(`
let and = &&;
true and false`)).toBe(false)
        expect(lits.run(`
let or = ||;
true or false`)).toBe(true)
        expect(lits.run(`
let obj = object;
obj("a", 1, "b", 2)`)).toEqual({ a: 1, b: 2 })
        expect(lits.run(`
let obj = object;
obj("a", 1, "b")`)).toEqual({ a: 1, b: null })
        expect(lits.run(`
let arr = array;
arr(1, 2, 3)`)).toEqual([1, 2, 3])
        expect(lits.run(`
let qq = ??;
null qq 0`)).toBe(0)
        expect(lits.run(`
let qq = ??;
0 qq 1`)).toBe(0)
        expect(lits.run(`
let qq = ??;
qq(null)`)).toBe(null)
        expect(() => lits.run(`
let r = recur;
r(1)`)).toThrow(RecurSignal)
      })
      expect(() => lits.run(`
let t = throw;
t("error")`)).toThrow(UserDefinedError)
      expect(() => lits.run('let t = \'if\';')).toThrow(LitsError)
      expect(() => lits.run('let d = defined?; d(+)')).toThrow(LitsError)
    })
  }
})
