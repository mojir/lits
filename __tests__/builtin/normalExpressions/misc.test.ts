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
      expect(() => lispish('(now 1)')).toThrow()
      expect(() => lispish('(now "x")')).toThrow()
      expect(() => lispish('(now undefined)')).toThrow()
      expect(lispish('(now)')).toBeLessThanOrEqual(Date.now())
    })
  })

  describe('!=', () => {
    test('samples', () => {
      expect(() => lispish('(!=)')).toThrow()
      expect(lispish('(!= 1)')).toBe(true)
      expect(lispish('(!= 1 1)')).toBe(false)
      expect(lispish('(!= 1 2)')).toBe(true)
      expect(lispish('(!= 1 2 1)')).toBe(false)
      expect(lispish('(!= 1 2 3)')).toBe(true)
      expect(lispish('(!= "1")')).toBe(true)
      expect(lispish('(!= "1" "1")')).toBe(false)
      expect(lispish('(!= "1" "2")')).toBe(true)
      expect(lispish('(!= "1" "2" "1")')).toBe(false)
      expect(lispish('(!= "1" "2" 3)')).toBe(true)
      expect(lispish('(!= null undefined)')).toBe(true)
      expect(lispish('(!= null 0)')).toBe(true)
      expect(lispish('(!= 1 undefined 1)')).toBe(false)
      expect(lispish('(!= 1 true 3)')).toBe(true)
      expect(lispish('(!= 1 false 3)')).toBe(true)
    })
  })

  describe('=', () => {
    test('samples', () => {
      expect(() => lispish('(=)')).toThrow()
      expect(lispish('(= 1)')).toBe(true)
      expect(lispish('(= 1 1)')).toBe(true)
      expect(lispish('(= 1 2)')).toBe(false)
      expect(lispish('(= 1 2 1)')).toBe(false)
      expect(lispish('(= 1 2 3)')).toBe(false)
      expect(lispish('(= "1")')).toBe(true)
      expect(lispish('(= "1" "1")')).toBe(true)
      expect(lispish('(= "1" "2")')).toBe(false)
      expect(lispish('(= "1" "2" "1")')).toBe(false)
      expect(lispish('(= "1" "2" "3")')).toBe(false)
      expect(lispish('(= "2" "2" "2")')).toBe(true)
      expect(lispish('(= 1 "2" 3)')).toBe(false)
      expect(lispish('(= 1 null 3)')).toBe(false)
      expect(lispish('(= 1 undefined 3)')).toBe(false)
      expect(lispish('(= 1 true 3)')).toBe(false)
      expect(lispish('(= 1 false 3)')).toBe(false)
      expect(lispish('(= null null)')).toBe(true)
      expect(lispish('(= undefined undefined)')).toBe(true)
      expect(lispish('(= true true)')).toBe(true)
      expect(lispish('(= false false)')).toBe(true)
      expect(lispish('(= null undefined)')).toBe(false)
    })
  })

  describe('not', () => {
    test('samples', () => {
      expect(() => lispish('(not)')).toThrow()
      expect(lispish('(not 0)')).toBe(true)
      expect(lispish('(not "")')).toBe(true)
      expect(lispish('(not "0")')).toBe(false)
      expect(lispish('(not 1)')).toBe(false)
      expect(lispish('(not -1)')).toBe(false)
      expect(lispish('(not (list))')).toBe(false)
      expect(lispish('(not false)')).toBe(true)
      expect(lispish('(not true)')).toBe(false)
      expect(lispish('(not null)')).toBe(true)
      expect(lispish('(not undefined)')).toBe(true)
      expect(() => lispish('(not 0 1)')).toThrow()
    })
  })

  describe('object', () => {
    test('samples', () => {
      expect(lispish(`(object)`)).toEqual({})
      expect(lispish(`(object "x" 1)`)).toEqual({ x: 1 })
      expect(lispish(`(object "x" 1 "x" 2)`)).toEqual({ x: 2 })
      expect(lispish(`(object "a" null "b" true "c" false "d" undefined "e" (object "x" (list)))`)).toEqual({
        a: null,
        b: true,
        c: false,
        d: undefined,
        e: { x: [] },
      })
      expect(lispish(`(let ((a "a")) (object a 1))`)).toEqual({ a: 1 })
      expect(() => lispish(`(object "x")`)).toThrow()
      expect(() => lispish(`(object "x")`)).toThrow()
      expect(() => lispish(`(object "x" 1 "y")`)).toThrow()
      expect(() => lispish(`(object 0 1)`)).toThrow()
      expect(() => lispish(`(object true 1)`)).toThrow()
      expect(() => lispish(`(object false 1)`)).toThrow()
      expect(() => lispish(`(object null 1)`)).toThrow()
      expect(() => lispish(`(object undefined 1)`)).toThrow()
      expect(() => lispish(`(object (list) 1)`)).toThrow()
      expect(() => lispish(`(object (object) 1)`)).toThrow()
    })
  })

  describe('write', () => {
    test('samples', () => {
      expect(lispish('(write 1)')).toBe(1)
      expect(lispish('(write "1")')).toBe('1')
      expect(lispish('(write (list))')).toEqual([])
      expect(lispish('(write (object))')).toEqual({})
      expect(lispish('(write null)')).toBe(null)
      expect(lispish('(write undefined)')).toBe(undefined)
      expect(lispish('(write true)')).toBe(true)
      expect(lispish('(write false)')).toBe(false)
    })
    test('that it does console.log', () => {
      lispish('(write 1)')
      expect(logSpy).toHaveBeenCalledWith(1)
    })
  })
})
