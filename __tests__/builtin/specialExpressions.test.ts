/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { executeProgram } from '../../src'

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
      expect(executeProgram(`(setq a 10) a`)).toBe(10)
      expect(executeProgram(`(setq a 10) (setq a 20) a`)).toBe(20)
      expect(() => executeProgram(`(setq a)`)).toThrow()
      expect(() => executeProgram(`(setq a 10 10)`)).toThrow()
      expect(() => executeProgram(`(setq 1 10)`)).toThrow()
      expect(() => executeProgram(`(setq null 10)`)).toThrow()
      expect(() => executeProgram(`(setq undefined 10)`)).toThrow()
      expect(() => executeProgram(`(setq false 10)`)).toThrow()
      expect(() => executeProgram(`(setq true 10)`)).toThrow()
      expect(() => executeProgram(`(setq (array) 10)`)).toThrow()
      expect(() => executeProgram(`(setq (object) 10)`)).toThrow()
      expect(() => executeProgram(`(setq "a" 10)`)).toThrow()
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
      executeProgram(program)
      expect(logSpy).toHaveBeenNthCalledWith(1, 'LISPISH>', 'A')
      expect(logSpy).toHaveBeenNthCalledWith(2, 'LISPISH>', 'B')
      expect(logSpy).toHaveBeenNthCalledWith(3, 'LISPISH>', 'C')
      expect(logSpy).toHaveBeenNthCalledWith(4, 'LISPISH>', 'A')
    })
  })
  describe('if', () => {
    test('samples', () => {
      expect(executeProgram(`(if true "A" "B")`)).toBe('A')
      expect(executeProgram(`(if false "A" "B")`)).toBe('B')
      expect(executeProgram(`(if null "A" "B")`)).toBe('B')
      expect(executeProgram(`(if undefined "A" "B")`)).toBe('B')
      expect(executeProgram(`(if "" "A" "B")`)).toBe('B')
      expect(executeProgram(`(if "x" "A" "B")`)).toBe('A')
      expect(executeProgram(`(if 0 "A" "B")`)).toBe('B')
      expect(executeProgram(`(if 1 "A" "B")`)).toBe('A')
      expect(executeProgram(`(if -1 "A" "B")`)).toBe('A')
      expect(executeProgram(`(if (array) "A" "B")`)).toBe('A')
      expect(executeProgram(`(if (object) "A" "B")`)).toBe('A')
      expect(() => executeProgram(`(if)`)).toThrow()
      expect(() => executeProgram(`(if true)`)).toThrow()
      expect(() => executeProgram(`(if true "A")`)).toThrow()
      expect(() => executeProgram(`(if true "A" "B" "Q")`)).toThrow()
    })
    test('That special form "if" only evaluate the correct path (true)', () => {
      executeProgram(`(if true (write "A") (write "B"))`)
      expect(logSpy).toHaveBeenCalledWith('LISPISH>', 'A')
      expect(logSpy).not.toHaveBeenCalledWith('LISPISH>', 'B')
    })
    test('That special form "if" only evaluate the correct path (false)', () => {
      executeProgram(`(if false (write "A") (write "B"))`)
      expect(logSpy).not.toHaveBeenCalledWith('LISPISH>', 'A')
      expect(logSpy).toHaveBeenCalledWith('LISPISH>', 'B')
    })
  })

  describe('let', () => {
    test('samples', () => {
      expect(executeProgram(`(let ((a "A")) a)`)).toBe('A')
      expect(executeProgram(`(let ((a "A") (b "B")) a b)`)).toBe('B')
    })
  })
})
