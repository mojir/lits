/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lispish } from '../../../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`misc functions`, () => {
  let oldLog: () => void
  let logSpy: jest.Mock<any, any>
  beforeEach(() => {
    oldLog = console.log
    logSpy = jest.fn()
    console.log = logSpy
  })
  afterEach(() => {
    console.log = oldLog
  })
  describe(`inst-ms`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(inst-ms 1)`)).toThrow()
      expect(() => lispish.run(`(inst-ms "x")`)).toThrow()
      expect(() => lispish.run(`(inst-ms undefined)`)).toThrow()
      expect(lispish.run(`(inst-ms)`)).toBeLessThanOrEqual(Date.now())
    })
  })

  describe(`not=`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(not=)`)).toThrow()
      expect(lispish.run(`(not= 1)`)).toBe(true)
      expect(lispish.run(`(not= 1 1)`)).toBe(false)
      expect(lispish.run(`(not= 1 2)`)).toBe(true)
      expect(lispish.run(`(not= 1 2 1)`)).toBe(false)
      expect(lispish.run(`(not= 1 2 3)`)).toBe(true)
      expect(lispish.run(`(not= "1")`)).toBe(true)
      expect(lispish.run(`(not= "1" "1")`)).toBe(false)
      expect(lispish.run(`(not= "1" "2")`)).toBe(true)
      expect(lispish.run(`(not= "1" "2" "1")`)).toBe(false)
      expect(lispish.run(`(not= "1" "2" 3)`)).toBe(true)
      expect(lispish.run(`(not= null undefined)`)).toBe(true)
      expect(lispish.run(`(not= null 0)`)).toBe(true)
      expect(lispish.run(`(not= 1 undefined 1)`)).toBe(false)
      expect(lispish.run(`(not= 1 true 3)`)).toBe(true)
      expect(lispish.run(`(not= 1 false 3)`)).toBe(true)
    })
  })

  describe(`=`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(=)`)).toThrow()
      expect(lispish.run(`(= 1)`)).toBe(true)
      expect(lispish.run(`(= 1 1)`)).toBe(true)
      expect(lispish.run(`(= 1 2)`)).toBe(false)
      expect(lispish.run(`(= 1 2 1)`)).toBe(false)
      expect(lispish.run(`(= 1 2 3)`)).toBe(false)
      expect(lispish.run(`(= "1")`)).toBe(true)
      expect(lispish.run(`(= "1" "1")`)).toBe(true)
      expect(lispish.run(`(= "1" "2")`)).toBe(false)
      expect(lispish.run(`(= "1" "2" "1")`)).toBe(false)
      expect(lispish.run(`(= "1" "2" "3")`)).toBe(false)
      expect(lispish.run(`(= "2" "2" "2")`)).toBe(true)
      expect(lispish.run(`(= 1 "2" 3)`)).toBe(false)
      expect(lispish.run(`(= 1 null 3)`)).toBe(false)
      expect(lispish.run(`(= 1 undefined 3)`)).toBe(false)
      expect(lispish.run(`(= 1 true 3)`)).toBe(false)
      expect(lispish.run(`(= 1 false 3)`)).toBe(false)
      expect(lispish.run(`(= null null)`)).toBe(true)
      expect(lispish.run(`(= undefined undefined)`)).toBe(true)
      expect(lispish.run(`(= true true)`)).toBe(true)
      expect(lispish.run(`(= false false)`)).toBe(true)
      expect(lispish.run(`(= null undefined)`)).toBe(false)
    })

    test(`Object equality`, () => {
      const program = `
        (def obj1 (object "x" 10))
        (def obj2 (object "x" 10))
        [(= obj1 obj1) (= obj1 obj2)]
      `
      expect(lispish.run(program)).toEqual([true, false])
    })

    test(`Array equality`, () => {
      const program = `
        (def array1 [1 2 3])
        (def array2 [1 2 3])
        [(= array1 array1) (= array1 array2)]
      `
      expect(lispish.run(program)).toEqual([true, false])
    })
  })

  describe(`not`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(not)`)).toThrow()
      expect(lispish.run(`(not 0)`)).toBe(true)
      expect(lispish.run(`(not "")`)).toBe(true)
      expect(lispish.run(`(not "0")`)).toBe(false)
      expect(lispish.run(`(not 1)`)).toBe(false)
      expect(lispish.run(`(not -1)`)).toBe(false)
      expect(lispish.run(`(not [])`)).toBe(false)
      expect(lispish.run(`(not false)`)).toBe(true)
      expect(lispish.run(`(not true)`)).toBe(false)
      expect(lispish.run(`(not null)`)).toBe(true)
      expect(lispish.run(`(not undefined)`)).toBe(true)
      expect(() => lispish.run(`(not 0 1)`)).toThrow()
    })
  })

  describe(`write!`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(write!)`)).toBeUndefined()
      expect(lispish.run(`(write! 1)`)).toBe(1)
      expect(lispish.run(`(write! "1")`)).toBe(`1`)
      expect(lispish.run(`(write! 100 [] "1")`)).toBe(`1`)
      expect(lispish.run(`(write! [])`)).toEqual([])
      expect(lispish.run(`(write! (object))`)).toEqual({})
      expect(lispish.run(`(write! null)`)).toBe(null)
      expect(lispish.run(`(write! undefined)`)).toBe(undefined)
      expect(lispish.run(`(write! true)`)).toBe(true)
      expect(lispish.run(`(write! false)`)).toBe(false)
    })
    test(`that it does console.log`, () => {
      lispish.run(`(write! 1)`)
      expect(logSpy).toHaveBeenCalledWith(1)
    })
  })

  describe(`get-path`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(get-path [1 2 3] "[1]")`)).toBe(2)
      expect(lispish.run(`(get-path (object "a" 1) "a")`)).toBe(1)
      expect(lispish.run(`(get-path (object "a" (object "b" [1 2 3])) "a.b[1]")`)).toBe(2)
      expect(lispish.run(`(get-path O "a.b[1]")`, { vars: { O: { a: { b: [1, 2, 3] } } } })).toBe(2)
      expect(lispish.run(`(get-path O "a.c[1]")`, { vars: { O: { a: { b: [1, 2, 3] } } } })).toBeUndefined()
      expect(lispish.run(`(get-path O "")`, { vars: { O: { a: { b: [1, 2, 3] } } } })).toEqual({ a: { b: [1, 2, 3] } })
      expect(() => lispish.run(`(get-path O)`, { vars: { O: { a: { b: [1, 2, 3] } } } })).toThrow()
      expect(() => lispish.run(`(get-path)`, { vars: { O: { a: { b: [1, 2, 3] } } } })).toThrow()
      expect(() => lispish.run(`(get-path O "a" "b")`, { vars: { O: { a: { b: [1, 2, 3] } } } })).toThrow()
      expect(() => lispish.run(`(get-path (regexp "abc" "a")`)).toThrow()
    })
  })

  describe(`apply`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(apply + [1 2 3 4])`)).toBe(10)
      expect(() => lispish.run(`(apply +)`)).toThrow()
      expect(() => lispish.run(`(apply + 2 3)`)).toThrow()
      expect(() => lispish.run(`(apply + [1 2] [3 4])`)).toThrow()
    })
  })

  describe(`debug!`, () => {
    let oldError: () => void
    let lastErrorLog = ``
    beforeEach(() => {
      oldError = console.error
      console.error = message => {
        lastErrorLog = message
      }
    })
    afterEach(() => {
      console.error = oldError
    })
    test(`samples`, () => {
      expect(lispish.run(`(debug!)`)).toBeUndefined()
      expect(() => lispish.run(`(debug! 0)`)).toThrow()
      expect(() => lispish.run(`(debug! undefined)`)).toThrow()
      expect(() => lispish.run(`(debug! null)`)).toThrow()
      expect(() => lispish.run(`(debug! true)`)).toThrow()
      expect(() => lispish.run(`(debug! false)`)).toThrow()
      expect(() => lispish.run(`(debug! [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(debug! (object "a" 1))`)).toThrow()
      expect(() => lispish.run(`(debug! "label")`)).toThrow()
      expect(() => lispish.run(`(debug! "" 0)`)).toThrow()
    })
    test(`multiple contexts`, () => {
      lispish.import(`(def x 10) (defn foo [] "foo") (def bar (fn [] "bar")) (def plus +)`)
      lispish.run(`((fn [z] (debug!) (+ z 1)) 10)`, { vars: { y: 20 } })
      expect(lastErrorLog).toMatchSnapshot()
    })
  })

  describe(`boolean`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(boolean 0)`)).toBe(false)
      expect(lispish.run(`(boolean 1)`)).toBe(true)
      expect(lispish.run(`(boolean "Albert")`)).toBe(true)
      expect(lispish.run(`(boolean "")`)).toBe(false)
      expect(lispish.run(`(boolean true)`)).toBe(true)
      expect(lispish.run(`(boolean false)`)).toBe(false)
      expect(lispish.run(`(boolean null)`)).toBe(false)
      expect(lispish.run(`(boolean undefined)`)).toBe(false)
      expect(lispish.run(`(boolean [])`)).toBe(true)
      expect(lispish.run(`(boolean {})`)).toBe(true)
      expect(() => lispish.run(`(boolean)`)).toThrow()
      expect(() => lispish.run(`(boolean 2 3)`)).toThrow()
    })
  })
})
