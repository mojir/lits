import { describe, expect, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { AssertionError, LitsError } from '../../../errors'
import { assertModule } from './'

describe('assert functions', () => {
  for (const lits of [new Lits({ modules: [assertModule] }), new Lits({ debug: true, modules: [assertModule] })]) {
    // Helper to run grid module functions with the new import syntax
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

    describe('assert-array', () => {
      it('samples', () => {
        expect(runWithAssert('Assert.assert-array([])')).toBeNull()
        expect(runWithAssert('Assert.assert-array([1, 2, 3])')).toBeNull()
        expect(() => runWithAssert('Assert.assert-array("string")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-array(42)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-array(true)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-array(null)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-array({}, "Expected an array")')).toThrowError(AssertionError)
      })
    })

    describe('assert-boolean', () => {
      it('samples', () => {
        expect(runWithAssert('Assert.assert-boolean(true)')).toBeNull()
        expect(runWithAssert('Assert.assert-boolean(false)')).toBeNull()
        expect(() => runWithAssert('Assert.assert-boolean(1)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-boolean(0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-boolean("true")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-boolean(null)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-boolean([], "Expected a boolean")')).toThrowError(AssertionError)
      })
    })

    describe('assert-collection', () => {
      it('samples', () => {
        expect(runWithAssert('Assert.assert-collection([])')).toBeNull()
        expect(runWithAssert('Assert.assert-collection([1, 2])')).toBeNull()
        expect(runWithAssert('Assert.assert-collection({})')).toBeNull()
        expect(runWithAssert('Assert.assert-collection({ a: 1 })')).toBeNull()
        expect(runWithAssert('Assert.assert-collection("hello")')).toBeNull()
        expect(() => runWithAssert('Assert.assert-collection(42)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-collection(true)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-collection(null, "Expected a collection")')).toThrowError(AssertionError)
      })
    })

    describe('assert-function', () => {
      it('samples', () => {
        expect(runWithAssert('Assert.assert-function(-> $ + 1)')).toBeNull()
        expect(runWithAssert('Assert.assert-function((x, y) -> x + y)')).toBeNull()
        expect(() => runWithAssert('Assert.assert-function(42)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-function("string")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-function([], "Expected a function")')).toThrowError(AssertionError)
      })
    })

    describe('assert-grid', () => {
      it('samples', () => {
        expect(runWithAssert('Assert.assert-grid([[1, 2], [3, 4]])')).toBeNull()
        expect(runWithAssert('Assert.assert-grid([["a", "b"], ["c", "d"]])')).toBeNull()
        expect(() => runWithAssert('Assert.assert-grid([[1, 2], [3]])')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-grid([1, 2])')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-grid(42, "Expected a grid")')).toThrowError(AssertionError)
      })
    })

    describe('assert-integer', () => {
      it('samples', () => {
        expect(runWithAssert('Assert.assert-integer(42)')).toBeNull()
        expect(runWithAssert('Assert.assert-integer(0)')).toBeNull()
        expect(runWithAssert('Assert.assert-integer(-7)')).toBeNull()
        expect(() => runWithAssert('Assert.assert-integer(3.14)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-integer("42")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-integer(true, "Expected an integer")')).toThrowError(AssertionError)
      })
    })

    describe('assert-matrix', () => {
      it('samples', () => {
        expect(runWithAssert('Assert.assert-matrix([[1, 2], [3, 4]])')).toBeNull()
        expect(() => runWithAssert('Assert.assert-matrix([["a", "b"], ["c", "d"]])')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-matrix([[1, 2], [3]])')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-matrix([1, 2])')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-matrix(42, "Expected a matrix")')).toThrowError(AssertionError)
      })
    })

    describe('assert-number', () => {
      it('samples', () => {
        expect(runWithAssert('Assert.assert-number(42)')).toBeNull()
        expect(runWithAssert('Assert.assert-number(3.14)')).toBeNull()
        expect(runWithAssert('Assert.assert-number(0)')).toBeNull()
        expect(runWithAssert('Assert.assert-number(-1)')).toBeNull()
        expect(() => runWithAssert('Assert.assert-number("42")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-number(true)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-number(null, "Expected a number")')).toThrowError(AssertionError)
      })
    })

    describe('assert-object', () => {
      it('samples', () => {
        expect(runWithAssert('Assert.assert-object({})')).toBeNull()
        expect(runWithAssert('Assert.assert-object({ a: 1 })')).toBeNull()
        expect(() => runWithAssert('Assert.assert-object([])')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-object("string")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-object(42, "Expected an object")')).toThrowError(AssertionError)
      })
    })

    describe('assert-regexp', () => {
      it('samples', () => {
        expect(runWithAssert('Assert.assert-regexp(#"^start")')).toBeNull()
        expect(runWithAssert('Assert.assert-regexp(regexp("test"))')).toBeNull()
        expect(() => runWithAssert('Assert.assert-regexp("string")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-regexp(42, "Expected a regexp")')).toThrowError(AssertionError)
      })
    })

    describe('assert-sequence', () => {
      it('samples', () => {
        expect(runWithAssert('Assert.assert-sequence([])')).toBeNull()
        expect(runWithAssert('Assert.assert-sequence([1, 2])')).toBeNull()
        expect(runWithAssert('Assert.assert-sequence("hello")')).toBeNull()
        expect(() => runWithAssert('Assert.assert-sequence({})')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-sequence(42)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-sequence(null, "Expected a sequence")')).toThrowError(AssertionError)
      })
    })

    describe('assert-string', () => {
      it('samples', () => {
        expect(runWithAssert('Assert.assert-string("")')).toBeNull()
        expect(runWithAssert('Assert.assert-string("hello")')).toBeNull()
        expect(() => runWithAssert('Assert.assert-string(42)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-string(true)')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-string([], "Expected a string")')).toThrowError(AssertionError)
      })
    })

    describe('assert-vector', () => {
      it('samples', () => {
        expect(runWithAssert('Assert.assert-vector([])')).toBeNull()
        expect(runWithAssert('Assert.assert-vector([1, 2, 3])')).toBeNull()
        expect(() => runWithAssert('Assert.assert-vector(["a", "b"])')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-vector("string")')).toThrowError(AssertionError)
        expect(() => runWithAssert('Assert.assert-vector(42, "Expected a vector")')).toThrowError(AssertionError)
      })
    })
  }
})
