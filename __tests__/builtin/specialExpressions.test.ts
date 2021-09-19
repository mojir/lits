/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { lispish } from '../../src'
import { ReturnFromSignal, ReturnSignal } from '../../src/errors'
import { FunctionScope } from '../../src/evaluator/interface'

describe('specialExpressions', () => {
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
      expect(() => lispish(`(setq true false)`)).toThrow()
      expect(() => lispish(`(setq a)`)).toThrow()
      expect(() => lispish(`(setq a 10 10)`)).toThrow()
      expect(() => lispish(`(setq 1 10)`)).toThrow()
      expect(() => lispish(`(setq null 10)`)).toThrow()
      expect(() => lispish(`(setq undefined 10)`)).toThrow()
      expect(() => lispish(`(setq false 10)`)).toThrow()
      expect(() => lispish(`(setq true 10)`)).toThrow()
      expect(() => lispish(`(setq (list) 10)`)).toThrow()
      expect(() => lispish(`(setq (object) 10)`)).toThrow()
      expect(() => lispish(`(setq "a" 10)`)).toThrow()
    })

    test('local variable', () => {
      const program = `
        (setq x "A")     ;Global variable x
        (write x)        ;"A"
        (let ((x "B"))   ;Local variable x
          (write x)      ;"B"
          (setq x "C")   ;Set local variable x
          (write x)      ;"C"
        )
        (write x)        ;"A" - global variable x
      `
      lispish(program)
      expect(logSpy).toHaveBeenNthCalledWith(1, 'A')
      expect(logSpy).toHaveBeenNthCalledWith(2, 'B')
      expect(logSpy).toHaveBeenNthCalledWith(3, 'C')
      expect(logSpy).toHaveBeenNthCalledWith(4, 'A')
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
      expect(lispish(`(if (list) "A" "B")`)).toBe('A')
      expect(lispish(`(if (object) "A" "B")`)).toBe('A')
      expect(() => lispish(`(if)`)).toThrow()
      expect(() => lispish(`(if true)`)).toThrow()
      expect(() => lispish(`(if true "A")`)).toThrow()
      expect(() => lispish(`(if true "A" "B" "Q")`)).toThrow()
    })
    test('That special form "if" only evaluate the correct path (true)', () => {
      lispish(`(if true (write "A") (write "B"))`)
      expect(logSpy).toHaveBeenCalledWith('A')
      expect(logSpy).not.toHaveBeenCalledWith('B')
    })
    test('That special form "if" only evaluate the correct path (false)', () => {
      lispish(`(if false (write "A") (write "B"))`)
      expect(logSpy).not.toHaveBeenCalledWith('A')
      expect(logSpy).toHaveBeenCalledWith('B')
    })
  })

  describe('let', () => {
    test('samples', () => {
      expect(lispish(`(let ((a "A")) a)`)).toBe('A')
      expect(lispish(`(let ((a "A") (b "B")) a b)`)).toBe('B')
      expect(lispish(`(let ((a "A") (b "B")) a b)`)).toBe('B')
      expect(lispish(`(let ((a (+ 10 20)) (b "B")) b a)`)).toBe(30)
      expect(() => lispish(`(let)`)).toThrow()
      expect(() => lispish(`(let ())`)).toThrow()
      expect(() => lispish(`(let (()))`)).toThrow()
      expect(() => lispish(`(let ((let ((b "B")) b)))`)).toThrow()
      expect(() => lispish(`(let ((a "A") b) a`)).toThrow()
      expect(() => lispish(`(let (a "A"))`)).toThrow()
      expect(() => lispish(`(let (a "A") a)`)).toThrow()
      expect(() => lispish(`(let ((a (lambda () 1))) a)`)).toThrow()
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

  describe('and', () => {
    test('samples', () => {
      expect(lispish('(and)')).toBe(true)
      expect(lispish('(and 0)')).toBe(0)
      expect(lispish('(and 0 1)')).toBe(0)
      expect(lispish('(and 2 0)')).toBe(0)
      expect(lispish('(and 2 0 1)')).toBe(0)
      expect(lispish('(and 2 3 0)')).toBe(0)
      expect(lispish('(and 2 3 "")')).toBe('')
      expect(lispish('(and 2 3 "x")')).toBe('x')
      expect(lispish('(and false 1)')).toBe(false)
      expect(lispish('(and 1 false)')).toBe(false)
      expect(lispish('(and 1 undefined)')).toBe(undefined)
      expect(lispish('(and 1 null)')).toBe(null)
      expect(lispish('(and 2 2 false)')).toBe(false)
      expect(lispish('(and 3 true 3)')).toBe(3)
    })
    describe('short circuit', () => {
      test('true, false', () => {
        expect(lispish('(and (write true) (write false))')).toBe(false)
        expect(logSpy).toHaveBeenNthCalledWith(1, true)
        expect(logSpy).toHaveBeenNthCalledWith(2, false)
      })
      test('true, 1', () => {
        expect(lispish('(and (write true) (write 1))')).toBe(1)
        expect(logSpy).toHaveBeenNthCalledWith(1, true)
        expect(logSpy).toHaveBeenNthCalledWith(2, 1)
      })
      test('false, true', () => {
        expect(lispish('(and (write false) (write true))')).toBe(false)
        expect(logSpy).toHaveBeenCalledWith(false)
        expect(logSpy).not.toHaveBeenCalledWith(true)
      })
      test('false, true', () => {
        expect(lispish('(and (write false) (write 0))')).toBe(false)
        expect(logSpy).toHaveBeenCalledWith(false)
        expect(logSpy).not.toHaveBeenCalledWith(0)
      })
    })
  })

  describe('or', () => {
    test('samples', () => {
      expect(lispish('(or)')).toBe(false)
      expect(lispish('(or 0)')).toBe(0)
      expect(lispish('(or 0 1)')).toBe(1)
      expect(lispish('(or 2 0)')).toBe(2)
      expect(lispish('(or null 0 false undefined)')).toBe(undefined)
      expect(lispish('(or null 0 1 undefined)')).toBe(1)
    })
    describe('short circuit', () => {
      test('true, false', () => {
        expect(lispish('(or (write true) (write false))')).toBe(true)
        expect(logSpy).toHaveBeenCalledWith(true)
        expect(logSpy).not.toHaveBeenCalledWith(false)
      })
      test('true, 1', () => {
        expect(lispish('(or (write true) (write 1))')).toBe(true)
        expect(logSpy).toHaveBeenCalledWith(true)
        expect(logSpy).not.toHaveBeenCalledWith(1)
      })
      test('false, true', () => {
        expect(lispish('(or (write false) (write true))')).toBe(true)
        expect(logSpy).toHaveBeenNthCalledWith(1, false)
        expect(logSpy).toHaveBeenNthCalledWith(2, true)
      })
      test('false, true', () => {
        expect(lispish('(or (write false) (write 0))')).toBe(0)
        expect(logSpy).toHaveBeenNthCalledWith(1, false)
        expect(logSpy).toHaveBeenNthCalledWith(2, 0)
      })
    })
  })

  describe('cond', () => {
    test('samples', () => {
      expect(lispish('(cond)')).toBeUndefined()
      expect(lispish('(cond (true 10) (true 20))')).toBe(10)
      expect(lispish('(cond (true 10))')).toBe(10)
      expect(lispish('(cond (false 20) (true (+ 5 5)) )')).toBe(10)
      expect(
        lispish('(cond ((> 5 10) 20) ((> 10 10) (write "Hej") (+ 5 5)) ((>= 10 10) "This will work" (+ 5 5 5)))'),
      ).toBe(15)
    })
    test('middle condition true', () => {
      expect(
        lispish('(cond ((> 5 10) (write 20)) ((>= 10 10) (+ 5 5)) ((write (>= 10 10)) "This will work" (+ 5 5 5)))'),
      ).toBe(10)
      expect(logSpy).not.toHaveBeenCalled()
    })
  })

  describe('defun', () => {
    test('samples', () => {
      const functions: FunctionScope = {}
      lispish('(defun add (a b) (+ a b))', {}, { variables: {}, functions })
      expect(functions.add).toBeTruthy()
      expect(() => lispish('(defun add () 10)')).not.toThrow()
      expect(() => lispish('(defun x (a a) 10)')).toThrow()
      expect(() => lispish('(defun true () 10)')).toThrow()
      expect(() => lispish('(defun false () 10)')).toThrow()
      expect(() => lispish('(defun null () 10)')).toThrow()
      expect(() => lispish('(defun undefined () 10)')).toThrow()
      expect(() => lispish('(defun add ("s") 10)')).toThrow()
      expect(() => lispish('(defun "add" (a b) (+ a b))')).toThrow()
      expect(() => lispish('(defun add 1 (+ a b))')).toThrow()
      expect(() => lispish('(defun add (a b))')).toThrow()
    })
    test('call defun function', () => {
      // expect(lispish('(defun sumOneToN (n) (if (<= n 1) n (+ n (sumOneToN (- n 1))))) (sumOneToN 10)')).toBe(55)
      expect(lispish("(defun applyWithVal (fn val) (fn val)) (applyWithVal #'1+ 10)")).toBe(11)
    })
  })

  describe('function', () => {
    test('samples', () => {
      lispish('(function +)')
      expect(() => lispish('(function)')).toThrow()
      expect(() => lispish('(function "k")')).toThrow()
      expect(() => lispish('(function k s)')).toThrow()
      expect(() => lispish('(function add)')).toThrow()
      expect(() => lispish('(function true)')).toThrow()
      expect(() => lispish('(function false)')).toThrow()
      expect(() => lispish('(function null)')).toThrow()
      expect(() => lispish('(function undefined)')).toThrow()
      expect(() => lispish('(defun add (x y) (+ x y)) (function add)')).not.toThrow()
    })
  })

  describe('lambda', () => {
    test('samples', () => {
      lispish('(lambda (x) (+ x 1))')
      lispish('(lambda () 1)')
      expect(() => lispish('((lambda (x) (+ y 1)) 10)')).toThrow()
      expect(() => lispish('(lambda (false) 1)')).toThrow()
      expect(() => lispish('(lambda (true) 1)')).toThrow()
      expect(() => lispish('(lambda (null) 1)')).toThrow()
      expect(() => lispish('(lambda (undefined) 1)')).toThrow()
      expect(() => lispish('(lambda)')).toThrow()
      expect(() => lispish('(lambda (x))')).toThrow()
      expect(() => lispish('(lambda "k")')).toThrow()
      expect(() => lispish('(lambda k s)')).toThrow()
      expect(() => lispish('(lambda add)')).toThrow()
    })
  })

  describe('return-from', () => {
    test('samples', () => {
      expect(() => lispish('(return-from x (* 2 4))')).toThrowError(ReturnFromSignal)
      expect(() => lispish('(return-from x)')).not.toThrowError(ReturnFromSignal)
      expect(() => lispish('(return-from x)')).toThrow()
      expect(() => lispish('(return-from)')).not.toThrowError(ReturnFromSignal)
      expect(() => lispish('(return-from)')).toThrow()
      expect(() => lispish('(return-from "x" 10)')).not.toThrowError(ReturnFromSignal)
      expect(() => lispish('(return-from "x" 10)')).toThrow()
      try {
        lispish('(return-from x (* 2 4))')
        throw 'Not expecting this'
      } catch (e) {
        expect(e).toBeInstanceOf(ReturnFromSignal)
        expect((e as ReturnFromSignal).name).toBe('ReturnFromSignal')
        expect((e as ReturnFromSignal).value).toBe(8)
        expect((e as ReturnFromSignal).blockName).toBe('x')
      }
    })
    test('in action', () => {
      const program = `
      (defun x () (write "Hej") (return-from x "Kalle") (write "san"))
      (x)
      `
      expect(lispish(program)).toBe('Kalle')
    })
    test('nested block 1', () => {
      const program = `
        (block x
          (setq val 1)
          (block y
            (return-from y undefined)
          )
          (setq val 2)
        )
        val
      `
      expect(lispish(program)).toBe(2)
    })

    test('nested block 2', () => {
      const program = `
        (block x
          (setq val 1)
          (block y
            (return-from x undefined)
          )
          (setq val 2)
        )
        val
      `
      expect(lispish(program)).toBe(1)
    })
    test('return from lambda', () => {
      const program = `
        ((lambda () 
          (return "A")
          "B"
        ))
      `
      expect(lispish(program)).toBe("A")
    })
  })

  describe('return', () => {
    test('samples', () => {
      expect(() => lispish('(return (* 2 4))')).toThrowError(ReturnSignal)
      expect(() => lispish('(return)')).not.toThrowError(ReturnFromSignal)
      expect(() => lispish('(return)')).toThrow()
      try {
        lispish('(return (* 2 4))')
        throw 'Not expecting this'
      } catch (e) {
        expect(e).toBeInstanceOf(ReturnSignal)
        expect((e as ReturnSignal).name).toBe('ReturnSignal')
        expect((e as ReturnSignal).value).toBe(8)
      }
    })
    test('in action', () => {
      const program = `
      (defun x () (write "Hej") (return "Kalle") (write "san"))
      (x)
      `
      expect(lispish(program)).toBe('Kalle')
    })
  })
  describe('block', () => {
    test('samples', () => {
      expect(
        lispish(`(block x
        (write "hej")
        (return-from x 10)
        (write "XXX")
      )`),
      ).toBe(10)
      expect(
        lispish(`(block x
        (write "hej")
        (write "XXX")
      )`),
      ).toBe('XXX')
      expect(() =>
        lispish(`(block x
        (write "hej")
        (return-from "x" 10)
        (write "XXX")
      )`),
      ).toThrow()
      expect(() =>
        lispish(`(block "x"
        (write "hej")
        (return-from x 10)
        (write "XXX")
      )`),
      ).toThrow()
      expect(() =>
        lispish(`(block x
        (write hej)
        (return-from x 10)
        (write "XXX")
      )`),
      ).toThrow()
    })
    test('asd', () => {
      expect(() =>
        lispish(`(block x
        (write hej)
        (return-from x 10)
        (write "XXX")
      )`),
      ).toThrow()
    })
  })

  describe('try', () => {
    test('samples', () => {
      expect(lispish('(try (/ 2 4) ((error) 1))')).toBe(0.5)
      expect(lispish('(try (/ 2 0) ((error) 1))')).toBe(1)
      expect(lispish('(try (/ 2 0) ((error) error))')).toBeInstanceOf(Error)
      expect(() => lispish('(try (/ 2 4) 1)')).toThrow()
      expect(() => lispish('(try (/ 2 4) (1))')).toThrow()
      expect(() => lispish('(try (/ 2 4) (("error") 1))')).toThrow()
      expect(() => lispish('(try (/ 2 4) ((error1 error2) 1))')).toThrow()
      expect(() => lispish('(try (/ 2 4) ((error) 1 2))')).toThrow()
      expect(() => lispish('(try (/ 2 4) ((error) 1 )2)')).toThrow()
    })
    test('return-from should not trigger catchBlock', () => {
      const program = `
        (block b 
          (try 
            (progn
              (write "One")
              (return-from b "Two")
              (write "Three")
            )
            ((error) "Four")
          )
        )
      `
      expect(lispish(program)).toBe('Two')
    })
    test('return-from should not trigger catchBlock', () => {
      const program = `
        (defun fn ()
          (try 
            (progn
              (write "One")
              (return "Two")
              (write "Three")
            )
            ((error) "Four")
          )
        )
        (fn)
      `
      expect(lispish(program)).toBe('Two')
    })
  })
})
