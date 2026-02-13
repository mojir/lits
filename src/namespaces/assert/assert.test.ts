import { describe, expect, it } from 'vitest'
import { Lits } from '../../Lits/Lits'
import { AssertionError, LitsError } from '../../errors'

describe('assert functions', () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    // Helper to run grid namespace functions with the new import syntax
    const runWithAssert = (code: string): unknown => {
      // Replace 'grid:functionName(' with 'let g = import("Grid"); g.functionName('
      const modifiedCode = `let Assert = import("Assert"); ${code}`
      return lits.run(modifiedCode)
    }
    describe('assert.', () => {
      it('samples', () => {
        expect(() => runWithAssert('Assert.assert(false)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert(false, "Expected true")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert(null)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert(0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert("")')).toThrowError(AssertionError)
        expect(runWithAssert('Assert.assert([])')).toEqual([])
        expect(runWithAssert('Assert.assert(true)')).toBe(true)
        expect(runWithAssert('Assert.assert(1)')).toBe(1)
        expect(runWithAssert('Assert.assert("0")')).toBe('0')
      })
    })
    describe('assert=', () => {
      it('samples', () => {
        expect(() => runWithAssert('Assert.assert=(1, 0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert=({ a: 1 }, { a: 2 })')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert=({ a: 1 }, { a: 2 }, "Expected deep equal")')).toThrowError(AssertionError)
        expect(runWithAssert('Assert.assert=({ a: 1 }, { a: 1 })')).toBeNull()
      })
    })
    describe('assert!=', () => {
      it('samples', () => {
        expect(() => runWithAssert('Assert.assert!=(0, 0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert!=({ a: 2 }, { a: 2 })')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert!=({ a: 2 }, { a: 2 }, "Expected not deep equal")')).toThrowError(
          AssertionError,
        )
        expect(runWithAssert('Assert.assert!=({ a: 2 }, { a: 1 })')).toBeNull()
      })
    })
    describe('assert-gt', () => {
      it('samples', () => {
        expect(() => runWithAssert('Assert.assert-gt(0, 0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-gt(0, 1)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-gt("Albert", "albert")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-gt("Albert", "albert", "Expected greater than")')).toThrowError(AssertionError)
        expect(runWithAssert('Assert.assert-gt(1, 0)')).toBeNull()
        expect(runWithAssert('Assert.assert-gt("albert", "Albert")')).toBeNull()
      })
    })
    describe('assert-lt', () => {
      it('samples', () => {
        expect(() => runWithAssert('Assert.assert-lt(0, 0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-lt(1, 0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-lt("albert", "Albert")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-lt("albert", "Albert", "Expected less than")')).toThrowError(AssertionError)
        expect(runWithAssert('Assert.assert-lt(0, 1)')).toBeNull()
        expect(runWithAssert('Assert.assert-lt("Albert", "albert")')).toBeNull()
      })
    })
    describe('assert-gte', () => {
      it('samples', () => {
        expect(() => runWithAssert('Assert.assert-gte(0, 1)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-gte("Albert", "albert")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-gte("Albert", "albert", "Expected greater than or equal")')).toThrowError(
          AssertionError,
        )
        expect(runWithAssert('Assert.assert-gte(1, 0)')).toBeNull()
        expect(runWithAssert('Assert.assert-gte(1, 1)')).toBeNull()
        expect(runWithAssert('Assert.assert-gte("albert", "albert")')).toBeNull()
        expect(runWithAssert('Assert.assert-gte("albert", "Albert")')).toBeNull()
      })
    })
    describe('assert-lte', () => {
      it('samples', () => {
        expect(() => runWithAssert('Assert.assert-lte(1, 0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-lte("albert", "Albert")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-lte("albert", "Albert", "Expected less than or equal")')).toThrowError(AssertionError)
        expect(runWithAssert('Assert.assert-lte(0, 1)')).toBeNull()
        expect(runWithAssert('Assert.assert-lte(1, 1)')).toBeNull()
        expect(runWithAssert('Assert.assert-lte("albert", "albert")')).toBeNull()
        expect(runWithAssert('Assert.assert-lte("Albert", "albert")')).toBeNull()
      })
    })
    describe('assert-true', () => {
      it('samples', () => {
        expect(() => runWithAssert('Assert.assert-true(false)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-true(false, "Expected false")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-true(1)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-true(null)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-true("x")')).toThrowError(AssertionError)
        expect(runWithAssert('Assert.assert-true(true)')).toBeNull()
      })
    })
    describe('assert-false', () => {
      it('samples', () => {
        expect(() => runWithAssert('Assert.assert-false(true)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-false(true, "Expected false")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-false(null)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-false(0)')).toThrowError(AssertionError)
        expect(runWithAssert('Assert.assert-false(false)')).toBeNull()
      })
    })

    describe('assert-truthy', () => {
      it('samples', () => {
        expect(runWithAssert('Assert.assert-truthy(true)')).toBeNull()
        expect(runWithAssert('Assert.assert-truthy([])')).toBeNull()
        expect(runWithAssert('Assert.assert-truthy({})')).toBeNull()
        expect(runWithAssert('Assert.assert-truthy(1)')).toBeNull()
        expect(runWithAssert('Assert.assert-truthy("hej")')).toBeNull()
        expect(runWithAssert('Assert.assert-truthy(-> $ + $)')).toBeNull()
        expect(() => runWithAssert('Assert.assert-truthy(false)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-truthy(null, "Expected true")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-truthy(0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-truthy("")')).toThrowError(AssertionError)
      })
    })

    describe('assert-falsy', () => {
      it('samples', () => {
        expect(() => runWithAssert('Assert.assert-falsy(true)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-falsy([])')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-falsy({})')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-falsy(1)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-falsy("hej")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-falsy(-> $1 + $1)')).toThrowError(AssertionError)
        expect(runWithAssert('Assert.assert-falsy(false)')).toBeNull()
        expect(runWithAssert('Assert.assert-falsy(null, "Expected true")')).toBeNull()
        expect(runWithAssert('Assert.assert-falsy(0)')).toBeNull()
        expect(runWithAssert('Assert.assert-falsy("")')).toBeNull()
      })
    })

    describe('assert-null', () => {
      it('samples', () => {
        expect(() => runWithAssert('Assert.assert-null(false)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-null(0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-null("")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-null("hej")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-null([])')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-null({})')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-null(-> $ + $)')).toThrowError(AssertionError)
        expect(runWithAssert('Assert.assert-null(null, "Should be null")')).toBeNull()
      })
    })

    describe('assert-throws', () => {
      it('samples', () => {
        expect(() => runWithAssert('Assert.assert-throws(-> identity("X")) "Should throw")')).toThrow(LitsError)
        expect(() => runWithAssert('Assert.assert-throws(-> throw("X"))')).not.toThrow()
        expect(() => runWithAssert('Assert.assert-throws(-> throw("X"), "I knew it")')).not.toThrow()
        expect(() => runWithAssert('Assert.assert-throws(-> throw("X"), 10)')).toThrow(LitsError)
      })
    })

    describe('assert-not-throws', () => {
      it('samples', () => {
        expect(() => runWithAssert('Assert.assert-not-throws(-> identity("X"), "Should not throw")')).not.toThrow()
        expect(() => runWithAssert('Assert.assert-not-throws(-> throw("X"))')).toThrow(LitsError)
      })
    })

    describe('assert-throws-error', () => {
      it('samples', () => {
        expect(() => runWithAssert('Assert.assert-throws-error(-> identity("X"), "X", "Should throw X")')).toThrow(LitsError)
        expect(() => runWithAssert('Assert.assert-throws-error(-> throw("Y"), "X")')).toThrow(LitsError)
        expect(() => runWithAssert('Assert.assert-throws-error(-> throw("X"), "X")')).not.toThrow()
      })
    })
  }
})
