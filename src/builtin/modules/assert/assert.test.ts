import { describe, expect, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { AssertionError, LitsError } from '../../../errors'
import { assertModule } from './'

describe('assert functions', () => {
  for (const lits of [new Lits({ modules: [assertModule] }), new Lits({ debug: true, modules: [assertModule] })]) {
    // Helper to run grid module functions with the new import syntax
    const runWithAssert = (code: string): unknown => {
      // Replace 'grid:functionName(' with 'let g = import("grid"); g.functionName('
      const modifiedCode = `let assert = import("assert"); ${code}`
      return lits.run(modifiedCode)
    }
    describe('assert.', () => {
      it('samples', () => {
        expect(() => runWithAssert('assert.assert(false)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert(false, "Expected true")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert(null)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert(0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert("")')).toThrowError(AssertionError)
        expect(runWithAssert('assert.assert([])')).toEqual([])
        expect(runWithAssert('assert.assert(true)')).toBe(true)
        expect(runWithAssert('assert.assert(1)')).toBe(1)
        expect(runWithAssert('assert.assert("0")')).toBe('0')
      })
    })
    describe('assert=', () => {
      it('samples', () => {
        expect(() => runWithAssert('assert.assert=(1, 0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert=({ a: 1 }, { a: 2 })')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert=({ a: 1 }, { a: 2 }, "Expected deep equal")')).toThrowError(AssertionError)
        expect(runWithAssert('assert.assert=({ a: 1 }, { a: 1 })')).toBeNull()
      })
    })
    describe('assert!=', () => {
      it('samples', () => {
        expect(() => runWithAssert('assert.assert!=(0, 0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert!=({ a: 2 }, { a: 2 })')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert!=({ a: 2 }, { a: 2 }, "Expected not deep equal")')).toThrowError(
          AssertionError,
        )
        expect(runWithAssert('assert.assert!=({ a: 2 }, { a: 1 })')).toBeNull()
      })
    })
    describe('assert-gt', () => {
      it('samples', () => {
        expect(() => runWithAssert('assert.assert-gt(0, 0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-gt(0, 1)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-gt("Albert", "albert")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-gt("Albert", "albert", "Expected greater than")')).toThrowError(AssertionError)
        expect(runWithAssert('assert.assert-gt(1, 0)')).toBeNull()
        expect(runWithAssert('assert.assert-gt("albert", "Albert")')).toBeNull()
      })
    })
    describe('assert-lt', () => {
      it('samples', () => {
        expect(() => runWithAssert('assert.assert-lt(0, 0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-lt(1, 0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-lt("albert", "Albert")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-lt("albert", "Albert", "Expected less than")')).toThrowError(AssertionError)
        expect(runWithAssert('assert.assert-lt(0, 1)')).toBeNull()
        expect(runWithAssert('assert.assert-lt("Albert", "albert")')).toBeNull()
      })
    })
    describe('assert-gte', () => {
      it('samples', () => {
        expect(() => runWithAssert('assert.assert-gte(0, 1)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-gte("Albert", "albert")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-gte("Albert", "albert", "Expected greater than or equal")')).toThrowError(
          AssertionError,
        )
        expect(runWithAssert('assert.assert-gte(1, 0)')).toBeNull()
        expect(runWithAssert('assert.assert-gte(1, 1)')).toBeNull()
        expect(runWithAssert('assert.assert-gte("albert", "albert")')).toBeNull()
        expect(runWithAssert('assert.assert-gte("albert", "Albert")')).toBeNull()
      })
    })
    describe('assert-lte', () => {
      it('samples', () => {
        expect(() => runWithAssert('assert.assert-lte(1, 0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-lte("albert", "Albert")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-lte("albert", "Albert", "Expected less than or equal")')).toThrowError(AssertionError)
        expect(runWithAssert('assert.assert-lte(0, 1)')).toBeNull()
        expect(runWithAssert('assert.assert-lte(1, 1)')).toBeNull()
        expect(runWithAssert('assert.assert-lte("albert", "albert")')).toBeNull()
        expect(runWithAssert('assert.assert-lte("Albert", "albert")')).toBeNull()
      })
    })
    describe('assert-true', () => {
      it('samples', () => {
        expect(() => runWithAssert('assert.assert-true(false)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-true(false, "Expected false")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-true(1)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-true(null)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-true("x")')).toThrowError(AssertionError)
        expect(runWithAssert('assert.assert-true(true)')).toBeNull()
      })
    })
    describe('assert-false', () => {
      it('samples', () => {
        expect(() => runWithAssert('assert.assert-false(true)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-false(true, "Expected false")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-false(null)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-false(0)')).toThrowError(AssertionError)
        expect(runWithAssert('assert.assert-false(false)')).toBeNull()
      })
    })

    describe('assert-truthy', () => {
      it('samples', () => {
        expect(runWithAssert('assert.assert-truthy(true)')).toBeNull()
        expect(runWithAssert('assert.assert-truthy([])')).toBeNull()
        expect(runWithAssert('assert.assert-truthy({})')).toBeNull()
        expect(runWithAssert('assert.assert-truthy(1)')).toBeNull()
        expect(runWithAssert('assert.assert-truthy("hej")')).toBeNull()
        expect(runWithAssert('assert.assert-truthy(-> $ + $)')).toBeNull()
        expect(() => runWithAssert('assert.assert-truthy(false)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-truthy(null, "Expected true")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-truthy(0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-truthy("")')).toThrowError(AssertionError)
      })
    })

    describe('assert-falsy', () => {
      it('samples', () => {
        expect(() => runWithAssert('assert.assert-falsy(true)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-falsy([])')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-falsy({})')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-falsy(1)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-falsy("hej")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-falsy(-> $1 + $1)')).toThrowError(AssertionError)
        expect(runWithAssert('assert.assert-falsy(false)')).toBeNull()
        expect(runWithAssert('assert.assert-falsy(null, "Expected true")')).toBeNull()
        expect(runWithAssert('assert.assert-falsy(0)')).toBeNull()
        expect(runWithAssert('assert.assert-falsy("")')).toBeNull()
      })
    })

    describe('assert-null', () => {
      it('samples', () => {
        expect(() => runWithAssert('assert.assert-null(false)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-null(0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-null("")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-null("hej")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-null([])')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-null({})')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-null(-> $ + $)')).toThrowError(AssertionError)
        expect(runWithAssert('assert.assert-null(null, "Should be null")')).toBeNull()
      })
    })

    describe('assert-throws', () => {
      it('samples', () => {
        expect(() => runWithAssert('assert.assert-throws(-> identity("X")) "Should throw")')).toThrow(LitsError)
        expect(() => runWithAssert('assert.assert-throws(-> throw("X"))')).not.toThrow()
        expect(() => runWithAssert('assert.assert-throws(-> throw("X"), "I knew it")')).not.toThrow()
        expect(() => runWithAssert('assert.assert-throws(-> throw("X"), 10)')).toThrow(LitsError)
      })
    })

    describe('assert-not-throws', () => {
      it('samples', () => {
        expect(() => runWithAssert('assert.assert-not-throws(-> identity("X"), "Should not throw")')).not.toThrow()
        expect(() => runWithAssert('assert.assert-not-throws(-> throw("X"))')).toThrow(LitsError)
      })
    })

    describe('assert-throws-error', () => {
      it('samples', () => {
        expect(() => runWithAssert('assert.assert-throws-error(-> identity("X"), "X", "Should throw X")')).toThrow(LitsError)
        expect(() => runWithAssert('assert.assert-throws-error(-> throw("Y"), "X")')).toThrow(LitsError)
        expect(() => runWithAssert('assert.assert-throws-error(-> throw("X"), "X")')).not.toThrow()
      })
    })

    describe('assert-array', () => {
      it('samples', () => {
        expect(runWithAssert('assert.assert-array([])')).toBeNull()
        expect(runWithAssert('assert.assert-array([1, 2, 3])')).toBeNull()
        expect(() => runWithAssert('assert.assert-array("string")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-array(42)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-array(true)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-array(null)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-array({}, "Expected an array")')).toThrowError(AssertionError)
      })
    })

    describe('assert-boolean', () => {
      it('samples', () => {
        expect(runWithAssert('assert.assert-boolean(true)')).toBeNull()
        expect(runWithAssert('assert.assert-boolean(false)')).toBeNull()
        expect(() => runWithAssert('assert.assert-boolean(1)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-boolean(0)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-boolean("true")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-boolean(null)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-boolean([], "Expected a boolean")')).toThrowError(AssertionError)
      })
    })

    describe('assert-collection', () => {
      it('samples', () => {
        expect(runWithAssert('assert.assert-collection([])')).toBeNull()
        expect(runWithAssert('assert.assert-collection([1, 2])')).toBeNull()
        expect(runWithAssert('assert.assert-collection({})')).toBeNull()
        expect(runWithAssert('assert.assert-collection({ a: 1 })')).toBeNull()
        expect(runWithAssert('assert.assert-collection("hello")')).toBeNull()
        expect(() => runWithAssert('assert.assert-collection(42)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-collection(true)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-collection(null, "Expected a collection")')).toThrowError(AssertionError)
      })
    })

    describe('assert-function', () => {
      it('samples', () => {
        expect(runWithAssert('assert.assert-function(-> $ + 1)')).toBeNull()
        expect(runWithAssert('assert.assert-function((x, y) -> x + y)')).toBeNull()
        expect(() => runWithAssert('assert.assert-function(42)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-function("string")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-function([], "Expected a function")')).toThrowError(AssertionError)
      })
    })

    describe('assert-grid', () => {
      it('samples', () => {
        expect(runWithAssert('assert.assert-grid([[1, 2], [3, 4]])')).toBeNull()
        expect(runWithAssert('assert.assert-grid([["a", "b"], ["c", "d"]])')).toBeNull()
        expect(() => runWithAssert('assert.assert-grid([[1, 2], [3]])')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-grid([1, 2])')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-grid(42, "Expected a grid")')).toThrowError(AssertionError)
      })
    })

    describe('assert-integer', () => {
      it('samples', () => {
        expect(runWithAssert('assert.assert-integer(42)')).toBeNull()
        expect(runWithAssert('assert.assert-integer(0)')).toBeNull()
        expect(runWithAssert('assert.assert-integer(-7)')).toBeNull()
        expect(() => runWithAssert('assert.assert-integer(3.14)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-integer("42")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-integer(true, "Expected an integer")')).toThrowError(AssertionError)
      })
    })

    describe('assert-matrix', () => {
      it('samples', () => {
        expect(runWithAssert('assert.assert-matrix([[1, 2], [3, 4]])')).toBeNull()
        expect(() => runWithAssert('assert.assert-matrix([["a", "b"], ["c", "d"]])')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-matrix([[1, 2], [3]])')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-matrix([1, 2])')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-matrix(42, "Expected a matrix")')).toThrowError(AssertionError)
      })
    })

    describe('assert-number', () => {
      it('samples', () => {
        expect(runWithAssert('assert.assert-number(42)')).toBeNull()
        expect(runWithAssert('assert.assert-number(3.14)')).toBeNull()
        expect(runWithAssert('assert.assert-number(0)')).toBeNull()
        expect(runWithAssert('assert.assert-number(-1)')).toBeNull()
        expect(() => runWithAssert('assert.assert-number("42")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-number(true)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-number(null, "Expected a number")')).toThrowError(AssertionError)
      })
    })

    describe('assert-object', () => {
      it('samples', () => {
        expect(runWithAssert('assert.assert-object({})')).toBeNull()
        expect(runWithAssert('assert.assert-object({ a: 1 })')).toBeNull()
        expect(() => runWithAssert('assert.assert-object([])')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-object("string")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-object(42, "Expected an object")')).toThrowError(AssertionError)
      })
    })

    describe('assert-regexp', () => {
      it('samples', () => {
        expect(runWithAssert('assert.assert-regexp(#"^start")')).toBeNull()
        expect(runWithAssert('assert.assert-regexp(regexp("test"))')).toBeNull()
        expect(() => runWithAssert('assert.assert-regexp("string")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-regexp(42, "Expected a regexp")')).toThrowError(AssertionError)
      })
    })

    describe('assert-sequence', () => {
      it('samples', () => {
        expect(runWithAssert('assert.assert-sequence([])')).toBeNull()
        expect(runWithAssert('assert.assert-sequence([1, 2])')).toBeNull()
        expect(runWithAssert('assert.assert-sequence("hello")')).toBeNull()
        expect(() => runWithAssert('assert.assert-sequence({})')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-sequence(42)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-sequence(null, "Expected a sequence")')).toThrowError(AssertionError)
      })
    })

    describe('assert-string', () => {
      it('samples', () => {
        expect(runWithAssert('assert.assert-string("")')).toBeNull()
        expect(runWithAssert('assert.assert-string("hello")')).toBeNull()
        expect(() => runWithAssert('assert.assert-string(42)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-string(true)')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-string([], "Expected a string")')).toThrowError(AssertionError)
      })
    })

    describe('assert-vector', () => {
      it('samples', () => {
        expect(runWithAssert('assert.assert-vector([])')).toBeNull()
        expect(runWithAssert('assert.assert-vector([1, 2, 3])')).toBeNull()
        expect(() => runWithAssert('assert.assert-vector(["a", "b"])')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-vector("string")')).toThrowError(AssertionError)
        expect(() => runWithAssert('assert.assert-vector(42, "Expected a vector")')).toThrowError(AssertionError)
      })
    })
  }
})
