import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src'
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
        expect(() => lits.run('assert=(0, 1)')).toThrowError(AssertionError)
        expect(() => lits.run('assert=(0, 1, "Expected same")')).toThrowError(AssertionError)
        expect(lits.run('assert=(1, 1)')).toBeNull()
        expect(lits.run('assert=("Albert", "Albert")')).toBeNull()
      })
    })
    describe('assert!=', () => {
      it('samples', () => {
        expect(() => lits.run('assert!=(0, 0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert!=(0, 0, "Expected different")')).toThrowError(AssertionError)
        expect(lits.run('assert!=(0, 1)')).toBeNull()
        expect(lits.run('assert!=("Albert", "Mojir")')).toBeNull()
      })
    })
    describe('assert_equal', () => {
      it('samples', () => {
        expect(() => lits.run('assert_equal(1, 0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_equal({"a" = 1}, {"a" = 2})')).toThrowError(AssertionError)
        expect(() => lits.run('assert_equal({"a" = 1}, {"a" = 2}, "Expected deep equal")')).toThrowError(AssertionError)
        expect(lits.run('assert_equal({"a" = 1}, {"a" = 1})')).toBeNull()
      })
    })
    describe('assert_not_equal', () => {
      it('samples', () => {
        expect(() => lits.run('assert_not_equal(0, 0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_not_equal({"a" = 2}, {"a" = 2})')).toThrowError(AssertionError)
        expect(() => lits.run('assert_not_equal({"a" = 2}, {"a" = 2}, "Expected not deep equal")')).toThrowError(
          AssertionError,
        )
        expect(lits.run('assert_not_equal({"a" = 2}, {"a" = 1})')).toBeNull()
      })
    })
    describe('assert_gt', () => {
      it('samples', () => {
        expect(() => lits.run('assert_gt(0, 0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_gt(0, 1)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_gt("Albert", "albert")')).toThrowError(AssertionError)
        expect(() => lits.run('assert_gt("Albert", "albert", "Expected greater than")')).toThrowError(AssertionError)
        expect(lits.run('assert_gt(1, 0)')).toBeNull()
        expect(lits.run('assert_gt("albert", "Albert")')).toBeNull()
      })
    })
    describe('assert_lt', () => {
      it('samples', () => {
        expect(() => lits.run('assert_lt(0, 0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_lt(1, 0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_lt("albert", "Albert")')).toThrowError(AssertionError)
        expect(() => lits.run('assert_lt("albert", "Albert", "Expected less than")')).toThrowError(AssertionError)
        expect(lits.run('assert_lt(0, 1)')).toBeNull()
        expect(lits.run('assert_lt("Albert", "albert")')).toBeNull()
      })
    })
    describe('assert_gte', () => {
      it('samples', () => {
        expect(() => lits.run('assert_gte(0, 1)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_gte("Albert", "albert")')).toThrowError(AssertionError)
        expect(() => lits.run('assert_gte("Albert", "albert", "Expected greater than or equal")')).toThrowError(
          AssertionError,
        )
        expect(lits.run('assert_gte(1, 0)')).toBeNull()
        expect(lits.run('assert_gte(1, 1)')).toBeNull()
        expect(lits.run('assert_gte("albert", "albert")')).toBeNull()
        expect(lits.run('assert_gte("albert", "Albert")')).toBeNull()
      })
    })
    describe('assert_lte', () => {
      it('samples', () => {
        expect(() => lits.run('assert_lte(1, 0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_lte("albert", "Albert")')).toThrowError(AssertionError)
        expect(() => lits.run('assert_lte("albert", "Albert", "Expected less than or equal")')).toThrowError(AssertionError)
        expect(lits.run('assert_lte(0, 1)')).toBeNull()
        expect(lits.run('assert_lte(1, 1)')).toBeNull()
        expect(lits.run('assert_lte("albert", "albert")')).toBeNull()
        expect(lits.run('assert_lte("Albert", "albert")')).toBeNull()
      })
    })
    describe('assert_true', () => {
      it('samples', () => {
        expect(() => lits.run('assert_true(false)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_true(false, "Expected false")')).toThrowError(AssertionError)
        expect(() => lits.run('assert_true(1)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_true(null)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_true("x")')).toThrowError(AssertionError)
        expect(lits.run('assert_true(true)')).toBeNull()
      })
    })
    describe('assert_false', () => {
      it('samples', () => {
        expect(() => lits.run('assert_false(true)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_false(true, "Expected false")')).toThrowError(AssertionError)
        expect(() => lits.run('assert_false(null)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_false(0)')).toThrowError(AssertionError)
        expect(lits.run('assert_false(false)')).toBeNull()
      })
    })

    describe('assert_truthy', () => {
      it('samples', () => {
        expect(lits.run('assert_truthy(true)')).toBeNull()
        expect(lits.run('assert_truthy([])')).toBeNull()
        expect(lits.run('assert_truthy({})')).toBeNull()
        expect(lits.run('assert_truthy(1)')).toBeNull()
        expect(lits.run('assert_truthy("hej")')).toBeNull()
        expect(lits.run('assert_truthy(#(+ %1 %1))')).toBeNull()
        expect(() => lits.run('assert_truthy(false)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_truthy(null "Expected true")')).toThrowError(AssertionError)
        expect(() => lits.run('assert_truthy(0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_truthy("")')).toThrowError(AssertionError)
      })
    })

    describe('assert_falsy', () => {
      it('samples', () => {
        expect(() => lits.run('assert_falsy(true)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_falsy([])')).toThrowError(AssertionError)
        expect(() => lits.run('assert_falsy({})')).toThrowError(AssertionError)
        expect(() => lits.run('assert_falsy(1)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_falsy("hej")')).toThrowError(AssertionError)
        expect(() => lits.run('assert_falsy(=> $1 + $1)')).toThrowError(AssertionError)
        expect(lits.run('assert_falsy(false)')).toBeNull()
        expect(lits.run('assert_falsy(null, "Expected true")')).toBeNull()
        expect(lits.run('assert_falsy(0)')).toBeNull()
        expect(lits.run('assert_falsy("")')).toBeNull()
      })
    })

    describe('assert_null', () => {
      it('samples', () => {
        expect(() => lits.run('assert_null(false)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_null(0)')).toThrowError(AssertionError)
        expect(() => lits.run('assert_null("")')).toThrowError(AssertionError)
        expect(() => lits.run('assert_null("hej")')).toThrowError(AssertionError)
        expect(() => lits.run('assert_null([])')).toThrowError(AssertionError)
        expect(() => lits.run('assert_null({})')).toThrowError(AssertionError)
        expect(() => lits.run('assert_null(=> $ + $)')).toThrowError(AssertionError)
        expect(lits.run('assert_null(null, "Should be null")')).toBeNull()
      })
    })

    describe('assert_throws', () => {
      it('samples', () => {
        expect(() => lits.run('assert_throws(=> identity("X")) "Should throw")')).toThrow()
        expect(() => lits.run('assert_throws(=> throw("X"))')).not.toThrow()
      })
    })

    describe('assert_not_throws', () => {
      it('samples', () => {
        expect(() => lits.run('assert_not_throws(=> identity("X"), "Should not throw")')).not.toThrow()
        expect(() => lits.run('assert_not_throws(=> throw("X"))')).toThrow()
      })
    })

    describe('assert_throws_error', () => {
      it('samples', () => {
        expect(() => lits.run('assert_throws_error(=> identity("X"), "X", "Should throw X")')).toThrow()
        expect(() => lits.run('assert_throws_error(=> throw("Y"), "X")')).toThrow()
        expect(() => lits.run('assert_throws_error(=> throw("X"), "X")')).not.toThrow()
      })
    })
  }
})
