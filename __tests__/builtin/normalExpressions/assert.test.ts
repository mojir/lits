import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src/Lits/Lits'
import { AssertionError } from '../../../src/errors'

describe('assert functions', () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    describe('assert.', () => {
      it('samples', () => {
        expect(() => lits.run('assert(false)')).toThrowError(AssertionError)
        expect(() => lits.run('assert(false, "Expected true")')).toThrowError(AssertionError)
        expect(() => lits.run('assert(null)')).toThrowError(AssertionError)
        expect(() => lits.run('assert(0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert("")')).toThrowError(AssertionError)
        expect(lits.run('assert([])')).toEqual([])
        expect(lits.run('assert(true)')).toBe(true)
        expect(lits.run('assert(1)')).toBe(1)
        expect(lits.run('assert("0")')).toBe('0')
      })
    })
    describe('assert=', () => {
      it('samples', () => {
        expect(() => lits.run('assert=(1, 0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert=({ a := 1 }, { a := 2 })')).toThrowError(AssertionError)
        expect(() => lits.run('assert=({ a := 1 }, { a := 2 }, "Expected deep equal")')).toThrowError(AssertionError)
        expect(lits.run('assert=({ a := 1 }, { a := 1 })')).toBeNull()
      })
    })
    describe('assert!=', () => {
      it('samples', () => {
        expect(() => lits.run('assert!=(0, 0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert!=({ a := 2 }, { a := 2 })')).toThrowError(AssertionError)
        expect(() => lits.run('assert!=({ a := 2 }, { a := 2 }, "Expected not deep equal")')).toThrowError(
          AssertionError,
        )
        expect(lits.run('assert!=({ a := 2 }, { a := 1 })')).toBeNull()
      })
    })
    describe('assert-gt', () => {
      it('samples', () => {
        expect(() => lits.run('assert-gt(0, 0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-gt(0, 1)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-gt("Albert", "albert")')).toThrowError(AssertionError)
        expect(() => lits.run('assert-gt("Albert", "albert", "Expected greater than")')).toThrowError(AssertionError)
        expect(lits.run('assert-gt(1, 0)')).toBeNull()
        expect(lits.run('assert-gt("albert", "Albert")')).toBeNull()
      })
    })
    describe('assert-lt', () => {
      it('samples', () => {
        expect(() => lits.run('assert-lt(0, 0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-lt(1, 0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-lt("albert", "Albert")')).toThrowError(AssertionError)
        expect(() => lits.run('assert-lt("albert", "Albert", "Expected less than")')).toThrowError(AssertionError)
        expect(lits.run('assert-lt(0, 1)')).toBeNull()
        expect(lits.run('assert-lt("Albert", "albert")')).toBeNull()
      })
    })
    describe('assert-gte', () => {
      it('samples', () => {
        expect(() => lits.run('assert-gte(0, 1)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-gte("Albert", "albert")')).toThrowError(AssertionError)
        expect(() => lits.run('assert-gte("Albert", "albert", "Expected greater than or equal")')).toThrowError(
          AssertionError,
        )
        expect(lits.run('assert-gte(1, 0)')).toBeNull()
        expect(lits.run('assert-gte(1, 1)')).toBeNull()
        expect(lits.run('assert-gte("albert", "albert")')).toBeNull()
        expect(lits.run('assert-gte("albert", "Albert")')).toBeNull()
      })
    })
    describe('assert-lte', () => {
      it('samples', () => {
        expect(() => lits.run('assert-lte(1, 0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-lte("albert", "Albert")')).toThrowError(AssertionError)
        expect(() => lits.run('assert-lte("albert", "Albert", "Expected less than or equal")')).toThrowError(AssertionError)
        expect(lits.run('assert-lte(0, 1)')).toBeNull()
        expect(lits.run('assert-lte(1, 1)')).toBeNull()
        expect(lits.run('assert-lte("albert", "albert")')).toBeNull()
        expect(lits.run('assert-lte("Albert", "albert")')).toBeNull()
      })
    })
    describe('assert-true', () => {
      it('samples', () => {
        expect(() => lits.run('assert-true(false)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-true(false, "Expected false")')).toThrowError(AssertionError)
        expect(() => lits.run('assert-true(1)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-true(null)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-true("x")')).toThrowError(AssertionError)
        expect(lits.run('assert-true(true)')).toBeNull()
      })
    })
    describe('assert-false', () => {
      it('samples', () => {
        expect(() => lits.run('assert-false(true)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-false(true, "Expected false")')).toThrowError(AssertionError)
        expect(() => lits.run('assert-false(null)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-false(0)')).toThrowError(AssertionError)
        expect(lits.run('assert-false(false)')).toBeNull()
      })
    })

    describe('assert-truthy', () => {
      it('samples', () => {
        expect(lits.run('assert-truthy(true)')).toBeNull()
        expect(lits.run('assert-truthy([])')).toBeNull()
        expect(lits.run('assert-truthy({})')).toBeNull()
        expect(lits.run('assert-truthy(1)')).toBeNull()
        expect(lits.run('assert-truthy("hej")')).toBeNull()
        expect(lits.run('assert-truthy(-> $ + $)')).toBeNull()
        expect(() => lits.run('assert-truthy(false)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-truthy(null, "Expected true")')).toThrowError(AssertionError)
        expect(() => lits.run('assert-truthy(0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-truthy("")')).toThrowError(AssertionError)
      })
    })

    describe('assert-falsy', () => {
      it('samples', () => {
        expect(() => lits.run('assert-falsy(true)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-falsy([])')).toThrowError(AssertionError)
        expect(() => lits.run('assert-falsy({})')).toThrowError(AssertionError)
        expect(() => lits.run('assert-falsy(1)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-falsy("hej")')).toThrowError(AssertionError)
        expect(() => lits.run('assert-falsy(-> $1 + $1)')).toThrowError(AssertionError)
        expect(lits.run('assert-falsy(false)')).toBeNull()
        expect(lits.run('assert-falsy(null, "Expected true")')).toBeNull()
        expect(lits.run('assert-falsy(0)')).toBeNull()
        expect(lits.run('assert-falsy("")')).toBeNull()
      })
    })

    describe('assert-null', () => {
      it('samples', () => {
        expect(() => lits.run('assert-null(false)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-null(0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert-null("")')).toThrowError(AssertionError)
        expect(() => lits.run('assert-null("hej")')).toThrowError(AssertionError)
        expect(() => lits.run('assert-null([])')).toThrowError(AssertionError)
        expect(() => lits.run('assert-null({})')).toThrowError(AssertionError)
        expect(() => lits.run('assert-null(-> $ + $)')).toThrowError(AssertionError)
        expect(lits.run('assert-null(null, "Should be null")')).toBeNull()
      })
    })

    describe('assert-throws', () => {
      it('samples', () => {
        expect(() => lits.run('assert-throws(-> identity("X")) "Should throw")')).toThrow()
        expect(() => lits.run('assert-throws(-> throw("X"))')).not.toThrow()
      })
    })

    describe('assert-not-throws', () => {
      it('samples', () => {
        expect(() => lits.run('assert-not-throws(-> identity("X"), "Should not throw")')).not.toThrow()
        expect(() => lits.run('assert-not-throws(-> throw("X"))')).toThrow()
      })
    })

    describe('assert-throws-error', () => {
      it('samples', () => {
        expect(() => lits.run('assert-throws-error(-> identity("X"), "X", "Should throw X")')).toThrow()
        expect(() => lits.run('assert-throws-error(-> throw("Y"), "X")')).toThrow()
        expect(() => lits.run('assert-throws-error(-> throw("X"), "X")')).not.toThrow()
      })
    })
  }
})
