/* eslint-disable no-console */
import { Lits } from '../../../src'
import { AssertionError } from '../../../src/errors'

describe(`assert functions`, () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    describe(`assert`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert false)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert false "Expected true")`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert nil)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert 0)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert "")`)).toThrowError(AssertionError)
        expect(lits.run(`(assert [])`)).toEqual([])
        expect(lits.run(`(assert true)`)).toBe(true)
        expect(lits.run(`(assert 1)`)).toBe(1)
        expect(lits.run(`(assert :0)`)).toBe(`0`)
      })
    })
    describe(`assert=`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert= 0 1)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert= 0 1 "Expected same")`)).toThrowError(AssertionError)
        expect(lits.run(`(assert= 1 1)`)).toBeNull()
        expect(lits.run(`(assert= :Albert :Albert)`)).toBeNull()
        expect(lits.run(`(assert= :Albert "Albert")`)).toBeNull()
      })
    })
    describe(`assert-not=`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert-not= 0 0)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-not= 0 0 "Expected different")`)).toThrowError(AssertionError)
        expect(lits.run(`(assert-not= 0 1)`)).toBeNull()
        expect(lits.run(`(assert-not= :Albert :Mojir)`)).toBeNull()
      })
    })
    describe(`assert-equal`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert-equal 1 0)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-equal {:a 1} {:a 2})`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-equal {:a 1} {:a 2} "Expected deep equal")`)).toThrowError(AssertionError)
        expect(lits.run(`(assert-equal {:a 1} {:a 1})`)).toBeNull()
      })
    })
    describe(`assert-not-equal`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert-not-equal 0 0)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-not-equal {:a 2} {:a 2})`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-not-equal {:a 2} {:a 2} "Expected not deep equal")`)).toThrowError(
          AssertionError,
        )
        expect(lits.run(`(assert-not-equal {:a 2} {:a 1})`)).toBeNull()
      })
    })
    describe(`assert>`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert> 0 0)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert> 0 1)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert> :Albert :albert)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert> :Albert :albert "Expected greater than")`)).toThrowError(AssertionError)
        expect(lits.run(`(assert> 1 0)`)).toBeNull()
        expect(lits.run(`(assert> :albert :Albert)`)).toBeNull()
      })
    })
    describe(`assert<`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert< 0 0)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert< 1 0)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert< :albert :Albert)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert< :albert :Albert "Expected less than")`)).toThrowError(AssertionError)
        expect(lits.run(`(assert< 0 1)`)).toBeNull()
        expect(lits.run(`(assert< :Albert :albert)`)).toBeNull()
      })
    })
    describe(`assert>=`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert>= 0 1)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert>= :Albert :albert)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert>= :Albert :albert "Expected greater than or equal")`)).toThrowError(
          AssertionError,
        )
        expect(lits.run(`(assert>= 1 0)`)).toBeNull()
        expect(lits.run(`(assert>= 1 1)`)).toBeNull()
        expect(lits.run(`(assert>= :albert :albert)`)).toBeNull()
        expect(lits.run(`(assert>= :albert :Albert)`)).toBeNull()
      })
    })
    describe(`assert<=`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert<= 1 0)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert<= :albert :Albert)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert<= :albert :Albert "Expected less than or equal")`)).toThrowError(AssertionError)
        expect(lits.run(`(assert<= 0 1)`)).toBeNull()
        expect(lits.run(`(assert<= 1 1)`)).toBeNull()
        expect(lits.run(`(assert<= :albert :albert)`)).toBeNull()
        expect(lits.run(`(assert<= :Albert :albert)`)).toBeNull()
      })
    })
    describe(`assert-true`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert-true false)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-true false "Expected false")`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-true 1)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-true nil)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-true :x)`)).toThrowError(AssertionError)
        expect(lits.run(`(assert-true true)`)).toBeNull()
      })
    })
    describe(`assert-false`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert-false true)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-false true "Expected false")`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-false nil)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-false 0)`)).toThrowError(AssertionError)
        expect(lits.run(`(assert-false false)`)).toBeNull()
      })
    })

    describe(`assert-truthy`, () => {
      test(`samples`, () => {
        expect(lits.run(`(assert-truthy true)`)).toBeNull()
        expect(lits.run(`(assert-truthy [])`)).toBeNull()
        expect(lits.run(`(assert-truthy {})`)).toBeNull()
        expect(lits.run(`(assert-truthy 1)`)).toBeNull()
        expect(lits.run(`(assert-truthy :hej)`)).toBeNull()
        expect(lits.run(`(assert-truthy #(+ %1 %1))`)).toBeNull()
        expect(() => lits.run(`(assert-truthy false)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-truthy nil "Expected true")`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-truthy 0)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-truthy "")`)).toThrowError(AssertionError)
      })
    })

    describe(`assert-falsy`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert-falsy true)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-falsy [])`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-falsy {})`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-falsy 1)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-falsy :hej)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-falsy #(+ %1 %1))`)).toThrowError(AssertionError)
        expect(lits.run(`(assert-falsy false)`)).toBeNull()
        expect(lits.run(`(assert-falsy nil "Expected true")`)).toBeNull()
        expect(lits.run(`(assert-falsy 0)`)).toBeNull()
        expect(lits.run(`(assert-falsy "")`)).toBeNull()
      })
    })

    describe(`assert-nil`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert-nil false)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-nil 0)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-nil "")`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-nil :hej)`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-nil [])`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-nil {})`)).toThrowError(AssertionError)
        expect(() => lits.run(`(assert-nil #(+ %1 %1))`)).toThrowError(AssertionError)
        expect(lits.run(`(assert-nil nil "Should be nil")`)).toBeNull()
      })
    })

    describe(`assert-throws`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert-throws (fn [] (identity :X)) "Should throw")`)).toThrow()
        expect(() => lits.run(`(assert-throws (fn [] (throw :X)))`)).not.toThrow()
      })
    })

    describe(`assert-not-throws`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert-not-throws (fn [] (identity :X)) "Should not throw")`)).not.toThrow()
        expect(() => lits.run(`(assert-not-throws (fn [] (throw :X)))`)).toThrow()
      })
    })

    describe(`assert-throws-error`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(assert-throws-error (fn [] (identity :X)) :X "Should throw X")`)).toThrow()
        expect(() => lits.run(`(assert-throws-error (fn [] (throw :Y)) :X)`)).toThrow()
        expect(() => lits.run(`(assert-throws-error (fn [] (throw :X)) :X)`)).not.toThrow()
      })
    })
  }
})
