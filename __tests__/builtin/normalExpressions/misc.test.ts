/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { lispish } from '../../../src'

describe('misc functions', () => {
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
  describe('now', () => {
    test('samples', () => {
      expect(() => lispish(`(now 1)`)).toThrow()
      expect(() => lispish(`(now "x")`)).toThrow()
      expect(() => lispish(`(now undefined)`)).toThrow()
      expect(lispish(`(now)`)).toBeLessThanOrEqual(Date.now())
    })
  })

  describe('!=', () => {
    test('samples', () => {
      expect(() => lispish(`(!=)`)).toThrow()
      expect(lispish(`(!= 1)`)).toBe(true)
      expect(lispish(`(!= 1 1)`)).toBe(false)
      expect(lispish(`(!= 1 2)`)).toBe(true)
      expect(lispish(`(!= 1 2 1)`)).toBe(false)
      expect(lispish(`(!= 1 2 3)`)).toBe(true)
      expect(lispish(`(!= "1")`)).toBe(true)
      expect(lispish(`(!= "1" "1")`)).toBe(false)
      expect(lispish(`(!= "1" "2")`)).toBe(true)
      expect(lispish(`(!= "1" "2" "1")`)).toBe(false)
      expect(lispish(`(!= "1" "2" 3)`)).toBe(true)
      expect(lispish(`(!= null undefined)`)).toBe(true)
      expect(lispish(`(!= null 0)`)).toBe(true)
      expect(lispish(`(!= 1 undefined 1)`)).toBe(false)
      expect(lispish(`(!= 1 true 3)`)).toBe(true)
      expect(lispish(`(!= 1 false 3)`)).toBe(true)
    })
  })

  describe('=', () => {
    test('samples', () => {
      expect(() => lispish(`(=)`)).toThrow()
      expect(lispish(`(= 1)`)).toBe(true)
      expect(lispish(`(= 1 1)`)).toBe(true)
      expect(lispish(`(= 1 2)`)).toBe(false)
      expect(lispish(`(= 1 2 1)`)).toBe(false)
      expect(lispish(`(= 1 2 3)`)).toBe(false)
      expect(lispish(`(= "1")`)).toBe(true)
      expect(lispish(`(= "1" "1")`)).toBe(true)
      expect(lispish(`(= "1" "2")`)).toBe(false)
      expect(lispish(`(= "1" "2" "1")`)).toBe(false)
      expect(lispish(`(= "1" "2" "3")`)).toBe(false)
      expect(lispish(`(= "2" "2" "2")`)).toBe(true)
      expect(lispish(`(= 1 "2" 3)`)).toBe(false)
      expect(lispish(`(= 1 null 3)`)).toBe(false)
      expect(lispish(`(= 1 undefined 3)`)).toBe(false)
      expect(lispish(`(= 1 true 3)`)).toBe(false)
      expect(lispish(`(= 1 false 3)`)).toBe(false)
      expect(lispish(`(= null null)`)).toBe(true)
      expect(lispish(`(= undefined undefined)`)).toBe(true)
      expect(lispish(`(= true true)`)).toBe(true)
      expect(lispish(`(= false false)`)).toBe(true)
      expect(lispish(`(= null undefined)`)).toBe(false)
    })

    test('Object equality', () => {
      const program = `
        (setq obj1 (object "x" 10))
        (setq obj2 (object "x" 10))
        '((= obj1 obj1) (= obj1 obj2))
      `
      expect(lispish(program)).toEqual([true, false])
    })

    test('List equality', () => {
      const program = `
        (setq list1 '(1 2 3))
        (setq list2 '(1 2 3))
        '((= list1 list1) (= list1 list2))
      `
      expect(lispish(program)).toEqual([true, false])
    })
  })

  describe('not', () => {
    test('samples', () => {
      expect(() => lispish(`(not)`)).toThrow()
      expect(lispish(`(not 0)`)).toBe(true)
      expect(lispish(`(not "")`)).toBe(true)
      expect(lispish(`(not "0")`)).toBe(false)
      expect(lispish(`(not 1)`)).toBe(false)
      expect(lispish(`(not -1)`)).toBe(false)
      expect(lispish(`(not '())`)).toBe(false)
      expect(lispish(`(not false)`)).toBe(true)
      expect(lispish(`(not true)`)).toBe(false)
      expect(lispish(`(not null)`)).toBe(true)
      expect(lispish(`(not undefined)`)).toBe(true)
      expect(() => lispish(`(not 0 1)`)).toThrow()
    })
  })

  describe('write', () => {
    test('samples', () => {
      expect(lispish(`(write)`)).toBeUndefined()
      expect(lispish(`(write 1)`)).toBe(1)
      expect(lispish(`(write "1")`)).toBe('1')
      expect(lispish(`(write 100 '() "1")`)).toBe('1')
      expect(lispish(`(write '())`)).toEqual([])
      expect(lispish(`(write (object))`)).toEqual({})
      expect(lispish(`(write null)`)).toBe(null)
      expect(lispish(`(write undefined)`)).toBe(undefined)
      expect(lispish(`(write true)`)).toBe(true)
      expect(lispish(`(write false)`)).toBe(false)
    })
    test('that it does console.log', () => {
      lispish(`(write 1)`)
      expect(logSpy).toHaveBeenCalledWith(1)
    })
  })

  describe('get-path', () => {
    test('samples', () => {
      expect(lispish(`(get-path '(1 2 3) "[1]")`)).toBe(2)
      expect(lispish(`(get-path (object "a" 1) "a")`)).toBe(1)
      expect(lispish(`(get-path (object "a" (object "b" '(1 2 3))) "a.b[1]")`)).toBe(2)
      expect(lispish(`(get-path O "a.b[1]")`, { O: { a: { b: [1, 2, 3] } } })).toBe(2)
      expect(lispish(`(get-path O "a.c[1]")`, { O: { a: { b: [1, 2, 3] } } })).toBeUndefined()
      expect(lispish(`(get-path O "")`, { O: { a: { b: [1, 2, 3] } } })).toBeUndefined()
      expect(() => lispish(`(get-path O)`, { O: { a: { b: [1, 2, 3] } } })).toThrow()
      expect(() => lispish(`(get-path)`, { O: { a: { b: [1, 2, 3] } } })).toThrow()
      expect(() => lispish(`(get-path O "a" "b")`, { O: { a: { b: [1, 2, 3] } } })).toThrow()
      expect(() => lispish(`(get-path (regexp "abc" "a")`)).toThrow()
    })
  })

  describe('progn', () => {
    test('samples', () => {
      expect(lispish(`(progn '(1 2 3) "[1]" (+ 1 2))`)).toBe(3)
      expect(lispish(`(progn (object "a" 1) "a")`)).toBe('a')
      expect(lispish(`(progn)`)).toBeUndefined()
    })
  })

  describe('apply', () => {
    test('samples', () => {
      expect(lispish("(apply #'+ '(1 2 3 4))")).toBe(10)
      expect(() => lispish("(apply #'+)")).toThrow()
      expect(() => lispish("(apply #'+ 2 3)")).toThrow()
      expect(() => lispish("(apply #'+ '(1 2) '(3 4))")).toThrow()
    })
  })

  describe('debug', () => {
    let oldError: () => void
    beforeEach(() => {
      oldError = console.error
      console.error = () => undefined
    })
    afterEach(() => {
      console.error = oldError
    })
    test('samples', () => {
      expect(lispish(`(debug "Stopping here")`)).toBeUndefined()
      expect(() => lispish(`(debug)`)).toThrow()
      expect(() => lispish(`(debug 0)`)).toThrow()
      expect(() => lispish(`(debug undefined)`)).toThrow()
      expect(() => lispish(`(debug null)`)).toThrow()
      expect(() => lispish(`(debug true)`)).toThrow()
      expect(() => lispish(`(debug false)`)).toThrow()
      expect(() => lispish(`(debug '(1 2 3))`)).toThrow()
      expect(() => lispish(`(debug (object "a" 1))`)).toThrow()
      expect(() => lispish(`(debug "" 0)`)).toThrow()
    })
  })
})
