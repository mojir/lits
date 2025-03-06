/* eslint-disable no-console */
import { afterEach, beforeEach, describe, expect, it, vitest } from 'vitest'
import { Lits } from '../../../src'

describe('functional functions', () => {
  for (const lits of [new Lits({ polish: true }), new Lits({ debug: true, polish: true })]) {
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
    describe('apply', () => {
      it('samples', () => {
        expect(lits.run('(apply + [1 2 3 4])')).toBe(10)
        expect(lits.run('(apply + 1 2 [ 3 4])')).toBe(10)
        expect(() => lits.run('(apply +)')).toThrow()
        expect(() => lits.run('(apply + 2 3)')).toThrow()
        expect(() => lits.run('(apply + [1 2] [3 4])')).toThrow()
      })
    })

    describe('identity', () => {
      it('samples', () => {
        expect(lits.run('(identity "Albert")')).toBe('Albert')
        expect(lits.run('(identity "")')).toBe('')
        expect(lits.run('(identity nil)')).toBe(null)
        expect(lits.run('(identity false)')).toBe(false)
        expect(lits.run('(identity true)')).toBe(true)
        expect(lits.run('(identity {:a 1})')).toEqual({ a: 1 })
        expect(lits.run('(identity [1 2 3])')).toEqual([1, 2, 3])
        expect(() => lits.run('(identity)')).toThrow()
        expect(() => lits.run('(identity 1 2)')).toThrow()
      })
    })

    describe('partial', () => {
      it('samples', () => {
        expect(lits.run('((partial + 1) 2)')).toBe(3)
        expect(lits.run('((partial (partial + 1) 2) 2)')).toBe(5)
        expect(() => lits.run('((partial true))')).toThrow()
        expect(() => lits.run('((partial mod 1))')).toThrow()
      })
    })

    describe('comp', () => {
      it('samples', () => {
        expect(lits.run('(def \'negative-quotient\' (comp - /)) (\'negative-quotient\' 9 3)')).toBe(-3)
        expect(
          lits.run(`
        (#((apply comp first (repeat rest %2)) %1) [1 2 3 4 5 6 7] 3)
      `),
        ).toBe(4)
        expect(lits.run('(def x {"bar" {"foo" 42}}) ((comp "foo" "bar") x)')).toBe(42)

        expect(lits.run('((comp) 10)')).toBe(10)
        expect(lits.run('((comp) nil)')).toBe(null)
        expect(lits.run('((comp) {:a 10})')).toEqual({ a: 10 })
        expect(lits.run('((comp) [:x 10 nil])')).toEqual(['x', 10, null])
        lits.run('(comp :a [:b :c])')
        expect(() => lits.run('((comp) 1 2)')).toThrow()
        expect(() => lits.run('((comp true))')).toThrow()
        expect(() => lits.run('((comp mod 1))')).toThrow()
      })
    })

    describe('constanty', () => {
      it('samples', () => {
        expect(lits.run('((constantly 10) 12 nil :x)')).toBe(10)
        expect(() => lits.run('(constanty)')).toThrow()
        expect(() => lits.run('(constanty 10 20)')).toThrow()
      })
    })

    describe('juxt', () => {
      it('samples', () => {
        expect(lits.run('((juxt + * min max) 3 4 6)')).toEqual([13, 72, 3, 6])
        expect(lits.run('((juxt :a :b) {:a 1, :b 2, :c 3, :d 4})')).toEqual([1, 2])
        expect(lits.run('(apply (juxt + * min max) (range 1 5))')).toEqual([10, 24, 1, 4])
        expect(() => lits.run('(juxt)')).toThrow()
      })
    })

    describe('complement', () => {
      it('samples', () => {
        expect(lits.run('((complement >) 4 6)')).toBe(true)
        expect(lits.run('((complement ==) 3 3)')).toBe(false)
        expect(() => lits.run('(complement)')).toThrow()
        expect(() => lits.run('(complement > <)')).toThrow()
      })
    })

    describe('every_pred', () => {
      it('samples', () => {
        expect(lits.run('((every_pred string? #(> (count %1) 3)) "Albert" "Mojir")')).toBe(true)
        expect(lits.run('((every_pred string? #(> (count %1) 3)) "Albert" :M)')).toBe(false)
        expect(lits.run('((every_pred string? #(> (count %1) 3)) "Albert" [1 2 3])')).toBe(false)
        expect(() => lits.run('(every_pred)')).toThrow()
      })
    })

    describe('some_pred', () => {
      it('samples', () => {
        expect(lits.run('((some_pred string? #(> (count %1) 3)) "Albert" :M)')).toBe(true)
        expect(lits.run('((some_pred string? #(> (count %1) 3)) :A :M)')).toBe(true)
        expect(lits.run('((some_pred string? #(> (count %1) 3)) [10 20] [20 10])')).toBe(false)
        expect(lits.run('((some_pred string? #(> (count %1) 3)) "Albert" [10 20])')).toBe(true)
        expect(() => lits.run('(some_pred)')).toThrow()
      })
    })

    describe('fnil', () => {
      it('samples', () => {
        expect(lits.run('((fnil + 1 2) 0 0)')).toBe(0)
        expect(lits.run('((fnil + 1 2) nil 0)')).toBe(1)
        expect(lits.run('((fnil + 1 2) 0 nil)')).toBe(2)
        expect(lits.run('((fnil + 1 2) nil nil)')).toBe(3)
        expect(() => lits.run('(fnil)')).toThrow()
        expect(() => lits.run('(fnil +)')).toThrow()
      })
    })
  }
})
