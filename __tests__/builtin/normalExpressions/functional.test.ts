/* eslint-disable no-console */
import { Lispish } from '../../../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`functional functions`, () => {
  let oldLog: () => void
  let logSpy: (...args: unknown[]) => void
  beforeEach(() => {
    oldLog = console.log
    logSpy = jest.fn()
    console.log = (...args) => {
      logSpy(...args)
    }
  })
  afterEach(() => {
    console.log = oldLog
  })
  describe(`apply`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(apply + [1 2 3 4])`)).toBe(10)
      expect(lispish.run(`(apply + 1 2 [ 3 4])`)).toBe(10)
      expect(() => lispish.run(`(apply +)`)).toThrow()
      expect(() => lispish.run(`(apply + 2 3)`)).toThrow()
      expect(() => lispish.run(`(apply + [1 2] [3 4])`)).toThrow()
    })
  })

  describe(`identity`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(identity "Albert")`)).toBe(`Albert`)
      expect(lispish.run(`(identity "")`)).toBe(``)
      expect(lispish.run(`(identity nil)`)).toBe(null)
      expect(lispish.run(`(identity false)`)).toBe(false)
      expect(lispish.run(`(identity true)`)).toBe(true)
      expect(lispish.run(`(identity {"a" 1})`)).toEqual({ a: 1 })
      expect(lispish.run(`(identity [1 2 3])`)).toEqual([1, 2, 3])
      expect(() => lispish.run(`(identity)`)).toThrow()
      expect(() => lispish.run(`(identity 1 2)`)).toThrow()
    })
  })

  describe(`partial`, () => {
    test(`samples`, () => {
      expect(lispish.run(`((partial + 1) 2)`)).toBe(3)
      expect(lispish.run(`((partial (partial + 1) 2) 2)`)).toBe(5)
      expect(() => lispish.run(`((partial true))`)).toThrow()
      expect(() => lispish.run(`((partial mod 1))`)).toThrow()
    })
  })

  describe(`comp`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(def negative-quotient (comp - /)) (negative-quotient 9 3)`)).toBe(-3)
      expect(
        lispish.run(`
        (#((apply comp first (repeat %2 rest)) %1) [1 2 3 4 5 6 7] 3)
      `),
      ).toBe(4)
      expect(lispish.run(`(def x {"bar" {"foo" 42}}) ((comp "foo" "bar") x)`)).toBe(42)

      expect(lispish.run(`((comp) 10)`)).toBe(10)
      expect(lispish.run(`((comp) nil)`)).toBe(null)
      expect(lispish.run(`((comp) {"a" 10})`)).toEqual({ a: 10 })
      expect(lispish.run(`((comp) ["x" 10 nil])`)).toEqual([`x`, 10, null])
      lispish.run(`(comp "a" ["b" "c"])`)
      expect(() => lispish.run(`((comp) 1 2)`)).toThrow()
      expect(() => lispish.run(`((comp true))`)).toThrow()
      expect(() => lispish.run(`((comp mod 1))`)).toThrow()
    })
  })

  describe(`constanty`, () => {
    test(`samples`, () => {
      expect(lispish.run(`((constantly 10) 12 nil "x")`)).toBe(10)
      expect(() => lispish.run(`(constanty)`)).toThrow()
      expect(() => lispish.run(`(constanty 10 20)`)).toThrow()
    })
  })

  describe(`juxt`, () => {
    test(`samples`, () => {
      expect(lispish.run(`((juxt + * min max) 3 4 6)`)).toEqual([13, 72, 3, 6])
      expect(lispish.run(`((juxt "a" "b") {"a" 1, "b" 2, "c" 3, "d" 4})`)).toEqual([1, 2])
      expect(lispish.run(`(apply (juxt + * min max) (range 1 5))`)).toEqual([10, 24, 1, 4])
      expect(() => lispish.run(`(juxt)`)).toThrow()
    })
  })

  describe(`complement`, () => {
    test(`samples`, () => {
      expect(lispish.run(`((complement >) 4 6)`)).toBe(true)
      expect(lispish.run(`((complement =) 3 3)`)).toBe(false)
      expect(() => lispish.run(`(complement)`)).toThrow()
      expect(() => lispish.run(`(complement > <)`)).toThrow()
    })
  })

  describe(`every-pred`, () => {
    test(`samples`, () => {
      expect(lispish.run(`((every-pred string? #(> (count %1) 3)) "Albert" "Mojir")`)).toBe(true)
      expect(lispish.run(`((every-pred string? #(> (count %1) 3)) "Albert" "M")`)).toBe(false)
      expect(lispish.run(`((every-pred string? #(> (count %1) 3)) "Albert" [1 2 3])`)).toBe(false)
      expect(() => lispish.run(`(every-pred)`)).toThrow()
    })
  })

  describe(`some-pred`, () => {
    test(`samples`, () => {
      expect(lispish.run(`((some-pred string? #(> (count %1) 3)) "Albert" "M")`)).toBe(true)
      expect(lispish.run(`((some-pred string? #(> (count %1) 3)) "A" "M")`)).toBe(true)
      expect(lispish.run(`((some-pred string? #(> (count %1) 3)) [10 20] [20 10])`)).toBe(false)
      expect(lispish.run(`((some-pred string? #(> (count %1) 3)) "Albert" [10 20])`)).toBe(true)
      expect(() => lispish.run(`(some-pred)`)).toThrow()
    })
  })

  describe(`fnil`, () => {
    test(`samples`, () => {
      expect(lispish.run(`((fnil + 1 2) 0 0)`)).toBe(0)
      expect(lispish.run(`((fnil + 1 2) nil 0)`)).toBe(1)
      expect(lispish.run(`((fnil + 1 2) 0 nil)`)).toBe(2)
      expect(lispish.run(`((fnil + 1 2) nil nil)`)).toBe(3)
      expect(() => lispish.run(`(fnil)`)).toThrow()
      expect(() => lispish.run(`(fnil +)`)).toThrow()
    })
  })
})
