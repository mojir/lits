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

    test('Object equality', () => {
      const program = `
        (setq obj1 (object "x" 10))
        (setq obj2 (object "x" 10))
        (list (= obj1 obj1) (= obj1 obj2))
      `
      expect(lispish(program)).toEqual([true, false])
    })

    test('List equality', () => {
      const program = `
        (setq list1 (list 1 2 3))
        (setq list2 (list 1 2 3))
        (list (= list1 list1) (= list1 list2))
      `
      expect(lispish(program)).toEqual([true, false])
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
