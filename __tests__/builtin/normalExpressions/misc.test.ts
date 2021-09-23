/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lispish } from '../../../src'

const lispish = new Lispish()

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
      expect(() => lispish.run(`(now 1)`)).toThrow()
      expect(() => lispish.run(`(now "x")`)).toThrow()
      expect(() => lispish.run(`(now undefined)`)).toThrow()
      expect(lispish.run(`(now)`)).toBeLessThanOrEqual(Date.now())
    })
  })

  describe('!=', () => {
    test('samples', () => {
      expect(() => lispish.run(`(!=)`)).toThrow()
      expect(lispish.run(`(!= 1)`)).toBe(true)
      expect(lispish.run(`(!= 1 1)`)).toBe(false)
      expect(lispish.run(`(!= 1 2)`)).toBe(true)
      expect(lispish.run(`(!= 1 2 1)`)).toBe(false)
      expect(lispish.run(`(!= 1 2 3)`)).toBe(true)
      expect(lispish.run(`(!= "1")`)).toBe(true)
      expect(lispish.run(`(!= "1" "1")`)).toBe(false)
      expect(lispish.run(`(!= "1" "2")`)).toBe(true)
      expect(lispish.run(`(!= "1" "2" "1")`)).toBe(false)
      expect(lispish.run(`(!= "1" "2" 3)`)).toBe(true)
      expect(lispish.run(`(!= null undefined)`)).toBe(true)
      expect(lispish.run(`(!= null 0)`)).toBe(true)
      expect(lispish.run(`(!= 1 undefined 1)`)).toBe(false)
      expect(lispish.run(`(!= 1 true 3)`)).toBe(true)
      expect(lispish.run(`(!= 1 false 3)`)).toBe(true)
    })
  })

  describe('=', () => {
    test('samples', () => {
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

    test('Object equality', () => {
      const program = `
        (setq obj1 (object "x" 10))
        (setq obj2 (object "x" 10))
        '((= obj1 obj1) (= obj1 obj2))
      `
      expect(lispish.run(program)).toEqual([true, false])
    })

    test('List equality', () => {
      const program = `
        (setq list1 '(1 2 3))
        (setq list2 '(1 2 3))
        '((= list1 list1) (= list1 list2))
      `
      expect(lispish.run(program)).toEqual([true, false])
    })
  })

  describe('not', () => {
    test('samples', () => {
      expect(() => lispish.run(`(not)`)).toThrow()
      expect(lispish.run(`(not 0)`)).toBe(true)
      expect(lispish.run(`(not "")`)).toBe(true)
      expect(lispish.run(`(not "0")`)).toBe(false)
      expect(lispish.run(`(not 1)`)).toBe(false)
      expect(lispish.run(`(not -1)`)).toBe(false)
      expect(lispish.run(`(not '())`)).toBe(false)
      expect(lispish.run(`(not false)`)).toBe(true)
      expect(lispish.run(`(not true)`)).toBe(false)
      expect(lispish.run(`(not null)`)).toBe(true)
      expect(lispish.run(`(not undefined)`)).toBe(true)
      expect(() => lispish.run(`(not 0 1)`)).toThrow()
    })
  })

  describe('write', () => {
    test('samples', () => {
      expect(lispish.run(`(write)`)).toBeUndefined()
      expect(lispish.run(`(write 1)`)).toBe(1)
      expect(lispish.run(`(write "1")`)).toBe('1')
      expect(lispish.run(`(write 100 '() "1")`)).toBe('1')
      expect(lispish.run(`(write '())`)).toEqual([])
      expect(lispish.run(`(write (object))`)).toEqual({})
      expect(lispish.run(`(write null)`)).toBe(null)
      expect(lispish.run(`(write undefined)`)).toBe(undefined)
      expect(lispish.run(`(write true)`)).toBe(true)
      expect(lispish.run(`(write false)`)).toBe(false)
    })
    test('that it does console.log', () => {
      lispish.run(`(write 1)`)
      expect(logSpy).toHaveBeenCalledWith(1)
    })
  })

  describe('get-path', () => {
    test('samples', () => {
      expect(lispish.run(`(get-path '(1 2 3) "[1]")`)).toBe(2)
      expect(lispish.run(`(get-path (object "a" 1) "a")`)).toBe(1)
      expect(lispish.run(`(get-path (object "a" (object "b" '(1 2 3))) "a.b[1]")`)).toBe(2)
      expect(lispish.run(`(get-path O "a.b[1]")`, { globalVariables: { O: { a: { b: [1, 2, 3] } } } })).toBe(2)
      expect(lispish.run(`(get-path O "a.c[1]")`, { globalVariables: { O: { a: { b: [1, 2, 3] } } } })).toBeUndefined()
      expect(lispish.run(`(get-path O "")`, { globalVariables: { O: { a: { b: [1, 2, 3] } } } })).toBeUndefined()
      expect(() => lispish.run(`(get-path O)`, { globalVariables: { O: { a: { b: [1, 2, 3] } } } })).toThrow()
      expect(() => lispish.run(`(get-path)`, { globalVariables: { O: { a: { b: [1, 2, 3] } } } })).toThrow()
      expect(() => lispish.run(`(get-path O "a" "b")`, { globalVariables: { O: { a: { b: [1, 2, 3] } } } })).toThrow()
      expect(() => lispish.run(`(get-path (regexp "abc" "a")`)).toThrow()
    })
  })

  describe('progn', () => {
    test('samples', () => {
      expect(lispish.run(`(progn '(1 2 3) "[1]" (+ 1 2))`)).toBe(3)
      expect(lispish.run(`(progn (object "a" 1) "a")`)).toBe('a')
      expect(lispish.run(`(progn)`)).toBeUndefined()
    })
  })

  describe('apply', () => {
    test('samples', () => {
      expect(lispish.run("(apply #'+ '(1 2 3 4))")).toBe(10)
      expect(() => lispish.run("(apply #'+)")).toThrow()
      expect(() => lispish.run("(apply #'+ 2 3)")).toThrow()
      expect(() => lispish.run("(apply #'+ '(1 2) '(3 4))")).toThrow()
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
      expect(lispish.run(`(debug "Stopping here")`)).toBeUndefined()
      expect(() => lispish.run(`(debug)`)).toThrow()
      expect(() => lispish.run(`(debug 0)`)).toThrow()
      expect(() => lispish.run(`(debug undefined)`)).toThrow()
      expect(() => lispish.run(`(debug null)`)).toThrow()
      expect(() => lispish.run(`(debug true)`)).toThrow()
      expect(() => lispish.run(`(debug false)`)).toThrow()
      expect(() => lispish.run(`(debug '(1 2 3))`)).toThrow()
      expect(() => lispish.run(`(debug (object "a" 1))`)).toThrow()
      expect(() => lispish.run(`(debug "" 0)`)).toThrow()
    })
  })
})
