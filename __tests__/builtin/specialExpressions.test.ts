/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { lispish } from '../../src'

describe('evaluator', () => {
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

  describe('setq', () => {
    test('samples', () => {
      expect(lispish(`(setq a 10) a`)).toBe(10)
      expect(lispish(`(setq a 10) (setq a 20) a`)).toBe(20)
      expect(() => lispish(`(setq a)`)).toThrow()
      expect(() => lispish(`(setq a 10 10)`)).toThrow()
      expect(() => lispish(`(setq 1 10)`)).toThrow()
      expect(() => lispish(`(setq null 10)`)).toThrow()
      expect(() => lispish(`(setq undefined 10)`)).toThrow()
      expect(() => lispish(`(setq false 10)`)).toThrow()
      expect(() => lispish(`(setq true 10)`)).toThrow()
      expect(() => lispish(`(setq (array) 10)`)).toThrow()
      expect(() => lispish(`(setq (object) 10)`)).toThrow()
      expect(() => lispish(`(setq "a" 10)`)).toThrow()
    })

    test('local variable', () => {
      const program = `
        (setq x "A")     ;Global variable x
        (write x)        ;LISPISH> "A"
        (let ((x "B"))   ;Local variable x
          (write x)      ;LISPISH> "B"
          (setq x "C")   ;Set local variable x
          (write x)      ;LISPISH> "C"
        )
        (write x)        ;LISPISH> "A" - global variable x
      `
      lispish(program)
      expect(logSpy).toHaveBeenNthCalledWith(1, 'LISPISH>', 'A')
      expect(logSpy).toHaveBeenNthCalledWith(2, 'LISPISH>', 'B')
      expect(logSpy).toHaveBeenNthCalledWith(3, 'LISPISH>', 'C')
      expect(logSpy).toHaveBeenNthCalledWith(4, 'LISPISH>', 'A')
    })
  })
  describe('if', () => {
    test('samples', () => {
      expect(lispish(`(if true "A" "B")`)).toBe('A')
      expect(lispish(`(if false "A" "B")`)).toBe('B')
      expect(lispish(`(if null "A" "B")`)).toBe('B')
      expect(lispish(`(if undefined "A" "B")`)).toBe('B')
      expect(lispish(`(if "" "A" "B")`)).toBe('B')
      expect(lispish(`(if "x" "A" "B")`)).toBe('A')
      expect(lispish(`(if 0 "A" "B")`)).toBe('B')
      expect(lispish(`(if 1 "A" "B")`)).toBe('A')
      expect(lispish(`(if -1 "A" "B")`)).toBe('A')
      expect(lispish(`(if (array) "A" "B")`)).toBe('A')
      expect(lispish(`(if (object) "A" "B")`)).toBe('A')
      expect(() => lispish(`(if)`)).toThrow()
      expect(() => lispish(`(if true)`)).toThrow()
      expect(() => lispish(`(if true "A")`)).toThrow()
      expect(() => lispish(`(if true "A" "B" "Q")`)).toThrow()
    })
    test('That special form "if" only evaluate the correct path (true)', () => {
      lispish(`(if true (write "A") (write "B"))`)
      expect(logSpy).toHaveBeenCalledWith('LISPISH>', 'A')
      expect(logSpy).not.toHaveBeenCalledWith('LISPISH>', 'B')
    })
    test('That special form "if" only evaluate the correct path (false)', () => {
      lispish(`(if false (write "A") (write "B"))`)
      expect(logSpy).not.toHaveBeenCalledWith('LISPISH>', 'A')
      expect(logSpy).toHaveBeenCalledWith('LISPISH>', 'B')
    })
  })

  describe('let', () => {
    test('samples', () => {
      expect(lispish(`(let ((a "A")) a)`)).toBe('A')
      expect(lispish(`(let ((a "A") (b "B")) a b)`)).toBe('B')
      expect(lispish(`(let ((a "A") (b "B")) a b)`)).toBe('B')
    })
    test('local and global variables', () => {
      expect(() =>
        lispish(`
          (let (
            (a "A")
            (b a)     ;Cannot access local variable a here. This is what let* whould be for
          )
            b
          )
        `),
      ).toThrow()
      expect(
        lispish(`
          (setq a "X")
          (let (
            (a "A")
            (b a)     ;a is the global variable
          )
            b
          )
        `),
      ).toBe('X')
    })
  })
})
