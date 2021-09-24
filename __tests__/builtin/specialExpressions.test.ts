/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lispish } from '../../src'
import { ReturnFromSignal, ReturnSignal, UserDefinedError } from '../../src/errors'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

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
      expect(lispish.run(`(setq a 10) a`)).toBe(10)
      expect(lispish.run(`(setq a 10) (setq a 20) a`)).toBe(20)
      expect(() => lispish.run(`(setq true false)`)).toThrow()
      expect(() => lispish.run(`(setq a)`)).toThrow()
      expect(() => lispish.run(`(setq a 10 10)`)).toThrow()
      expect(() => lispish.run(`(setq 1 10)`)).toThrow()
      expect(() => lispish.run(`(setq null 10)`)).toThrow()
      expect(() => lispish.run(`(setq undefined 10)`)).toThrow()
      expect(() => lispish.run(`(setq false 10)`)).toThrow()
      expect(() => lispish.run(`(setq true 10)`)).toThrow()
      expect(() => lispish.run(`(setq '() 10)`)).toThrow()
      expect(() => lispish.run(`(setq (object) 10)`)).toThrow()
      expect(() => lispish.run(`(setq "a" 10)`)).toThrow()
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
      lispish.run(program)
      expect(logSpy).toHaveBeenNthCalledWith(1, 'A')
      expect(logSpy).toHaveBeenNthCalledWith(2, 'B')
      expect(logSpy).toHaveBeenNthCalledWith(3, 'C')
      expect(logSpy).toHaveBeenNthCalledWith(4, 'A')
    })
  })
  describe('setq-constant', () => {
    test('samples', () => {
      expect(lispish.run(`(setq-constant a 10) a`)).toBe(10)
      expect(() => lispish.run(`(setq a 10) (setq-constant a 20)`)).toThrow()
      expect(() => lispish.run(`(setq-constant a 10) (setq-constant a 20)`)).toThrow()
      expect(() => lispish.run(`(setq-constant a 10) (setq a 20)`)).toThrow()
      expect(() => lispish.run(`(setq-constant true false)`)).toThrow()
      expect(() => lispish.run(`(setq-constant a)`)).toThrow()
      expect(() => lispish.run(`(setq-constant a 10 10)`)).toThrow()
      expect(() => lispish.run(`(setq-constant 1 10)`)).toThrow()
      expect(() => lispish.run(`(setq-constant null 10)`)).toThrow()
      expect(() => lispish.run(`(setq-constant undefined 10)`)).toThrow()
      expect(() => lispish.run(`(setq-constant false 10)`)).toThrow()
      expect(() => lispish.run(`(setq-constant true 10)`)).toThrow()
      expect(() => lispish.run(`(setq-constant '() 10)`)).toThrow()
      expect(() => lispish.run(`(setq-constant (object) 10)`)).toThrow()
      expect(() => lispish.run(`(setq-constant "a" 10)`)).toThrow()
    })
  })

  describe('setq-local.', () => {
    test('samples', () => {
      expect(lispish.run(`(setq a 10) (progn (setq-local a 20)) a`)).toBe(10)
      expect(lispish.run(`(setq a 10) (setq-local a 20)`)).toBe(20)
      expect(lispish.run(`(setq-local a 10) (setq-local a 20) a`)).toBe(20)
      expect(() => lispish.run(`(setq-local true false)`)).toThrow()
      expect(() => lispish.run(`(setq-local a)`)).toThrow()
      expect(() => lispish.run(`(setq-local a 10 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local 1 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local null 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local undefined 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local false 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local true 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local '() 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local (object) 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local "a" 10)`)).toThrow()
    })
  })

  describe('setq-local-constant.', () => {
    test('samples', () => {
      expect(lispish.run(`(setq a 10) (progn (setq-local-constant a 20)) (setq a 30) a`)).toBe(30)
      expect(() => lispish.run(`(setq a 10) (setq-local-constant a 20)`)).toThrow()
      expect(() => lispish.run(`(setq a 10) (progn (setq-local-constant a 20) (setq a 30))`)).toThrow()
      expect(() => lispish.run(`(setq-local-constant a 10) (setq a 20) a`)).toThrow()
      expect(() => lispish.run(`(setq-local-constant true false)`)).toThrow()
      expect(() => lispish.run(`(setq-local-constant a)`)).toThrow()
      expect(() => lispish.run(`(setq-local-constant a 10 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local-constant 1 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local-constant null 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local-constant undefined 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local-constant false 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local-constant true 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local-constant '() 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local-constant (object) 10)`)).toThrow()
      expect(() => lispish.run(`(setq-local-constant "a" 10)`)).toThrow()
    })
  })
  describe('if', () => {
    test('samples', () => {
      expect(lispish.run(`(if true "A" "B")`)).toBe('A')
      expect(lispish.run(`(if false "A" "B")`)).toBe('B')
      expect(lispish.run(`(if null "A" "B")`)).toBe('B')
      expect(lispish.run(`(if undefined "A" "B")`)).toBe('B')
      expect(lispish.run(`(if "" "A" "B")`)).toBe('B')
      expect(lispish.run(`(if "x" "A" "B")`)).toBe('A')
      expect(lispish.run(`(if 0 "A" "B")`)).toBe('B')
      expect(lispish.run(`(if 1 "A" "B")`)).toBe('A')
      expect(lispish.run(`(if -1 "A" "B")`)).toBe('A')
      expect(lispish.run(`(if '() "A" "B")`)).toBe('A')
      expect(lispish.run(`(if (object) "A" "B")`)).toBe('A')
      expect(() => lispish.run(`(if)`)).toThrow()
      expect(() => lispish.run(`(if true)`)).toThrow()
      expect(() => lispish.run(`(if true "A")`)).toThrow()
      expect(() => lispish.run(`(if true "A" "B" "Q")`)).toThrow()
    })
    test('That special form "if" only evaluate the correct path (true)', () => {
      lispish.run(`(if true (write "A") (write "B"))`)
      expect(logSpy).toHaveBeenCalledWith('A')
      expect(logSpy).not.toHaveBeenCalledWith('B')
    })
    test('That special form "if" only evaluate the correct path (false)', () => {
      lispish.run(`(if false (write "A") (write "B"))`)
      expect(logSpy).not.toHaveBeenCalledWith('A')
      expect(logSpy).toHaveBeenCalledWith('B')
    })
  })

  describe('let', () => {
    test('samples', () => {
      expect(lispish.run(`(let ((a "A")) a)`)).toBe('A')
      expect(lispish.run(`(let ((a "A") (b "B")) a b)`)).toBe('B')
      expect(lispish.run(`(let ((a "A") (b "B")) a b)`)).toBe('B')
      expect(lispish.run(`(let ((a (+ 10 20)) (b "B")) b a)`)).toBe(30)
      expect(() => lispish.run(`(let)`)).toThrow()
      expect(() => lispish.run(`(let ())`)).toThrow()
      expect(() => lispish.run(`(let (()))`)).toThrow()
      expect(() => lispish.run(`(let ((let ((b "B")) b)))`)).toThrow()
      expect(() => lispish.run(`(let ((a "A") b) a`)).toThrow()
      expect(() => lispish.run(`(let (a "A"))`)).toThrow()
      expect(() => lispish.run(`(let (a "A") a)`)).toThrow()
      expect(() => lispish.run(`(let ((a (lambda () 1))) a)`)).toThrow()
    })
    test('local and global variables', () => {
      expect(() =>
        lispish.run(`
          (let (
            (a "A")
            (b a)     ;Cannot access local variable a here. This is what let* whould be for
          )
            b
          )
        `),
      ).toThrow()
      expect(
        lispish.run(`
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
      expect(lispish.run(`(and)`)).toBe(true)
      expect(lispish.run(`(and 0)`)).toBe(0)
      expect(lispish.run(`(and 0 1)`)).toBe(0)
      expect(lispish.run(`(and 2 0)`)).toBe(0)
      expect(lispish.run(`(and 2 0 1)`)).toBe(0)
      expect(lispish.run(`(and 2 3 0)`)).toBe(0)
      expect(lispish.run(`(and 2 3 "")`)).toBe('')
      expect(lispish.run(`(and 2 3 "x")`)).toBe('x')
      expect(lispish.run(`(and false 1)`)).toBe(false)
      expect(lispish.run(`(and 1 false)`)).toBe(false)
      expect(lispish.run(`(and 1 undefined)`)).toBe(undefined)
      expect(lispish.run(`(and 1 null)`)).toBe(null)
      expect(lispish.run(`(and 2 2 false)`)).toBe(false)
      expect(lispish.run(`(and 3 true 3)`)).toBe(3)
    })
    describe('short circuit', () => {
      test('true, false', () => {
        expect(lispish.run(`(and (write true) (write false))`)).toBe(false)
        expect(logSpy).toHaveBeenNthCalledWith(1, true)
        expect(logSpy).toHaveBeenNthCalledWith(2, false)
      })
      test('true, 1', () => {
        expect(lispish.run(`(and (write true) (write 1))`)).toBe(1)
        expect(logSpy).toHaveBeenNthCalledWith(1, true)
        expect(logSpy).toHaveBeenNthCalledWith(2, 1)
      })
      test('false, true', () => {
        expect(lispish.run(`(and (write false) (write true))`)).toBe(false)
        expect(logSpy).toHaveBeenCalledWith(false)
        expect(logSpy).not.toHaveBeenCalledWith(true)
      })
      test('false, true', () => {
        expect(lispish.run(`(and (write false) (write 0))`)).toBe(false)
        expect(logSpy).toHaveBeenCalledWith(false)
        expect(logSpy).not.toHaveBeenCalledWith(0)
      })
    })
  })

  describe('or', () => {
    test('samples', () => {
      expect(lispish.run(`(or)`)).toBe(false)
      expect(lispish.run(`(or 0)`)).toBe(0)
      expect(lispish.run(`(or 0 1)`)).toBe(1)
      expect(lispish.run(`(or 2 0)`)).toBe(2)
      expect(lispish.run(`(or null 0 false undefined)`)).toBe(undefined)
      expect(lispish.run(`(or null 0 1 undefined)`)).toBe(1)
    })
    describe('short circuit', () => {
      test('true, false', () => {
        expect(lispish.run(`(or (write true) (write false))`)).toBe(true)
        expect(logSpy).toHaveBeenCalledWith(true)
        expect(logSpy).not.toHaveBeenCalledWith(false)
      })
      test('true, 1', () => {
        expect(lispish.run(`(or (write true) (write 1))`)).toBe(true)
        expect(logSpy).toHaveBeenCalledWith(true)
        expect(logSpy).not.toHaveBeenCalledWith(1)
      })
      test('false, true', () => {
        expect(lispish.run(`(or (write false) (write true))`)).toBe(true)
        expect(logSpy).toHaveBeenNthCalledWith(1, false)
        expect(logSpy).toHaveBeenNthCalledWith(2, true)
      })
      test('false, true', () => {
        expect(lispish.run(`(or (write false) (write 0))`)).toBe(0)
        expect(logSpy).toHaveBeenNthCalledWith(1, false)
        expect(logSpy).toHaveBeenNthCalledWith(2, 0)
      })
    })
  })

  describe('cond', () => {
    test('samples', () => {
      expect(lispish.run(`(cond)`)).toBeUndefined()
      expect(lispish.run(`(cond (true 10) (true 20))`)).toBe(10)
      expect(lispish.run(`(cond (true 10))`)).toBe(10)
      expect(lispish.run(`(cond (false 20) (true (+ 5 5)) )`)).toBe(10)
      expect(
        lispish.run(`(cond ((> 5 10) 20) ((> 10 10) (write "Hej") (+ 5 5)) ((>= 10 10) "This will work" (+ 5 5 5)))`),
      ).toBe(15)
      expect(() => lispish.run(`(cond (true 123) )`)).not.toThrow()
      expect(() => lispish.run(`(cond true 123) )`)).toThrow()
    })
    test('middle condition true', () => {
      expect(
        lispish.run(
          '(cond ((> 5 10) (write 20)) ((>= 10 10) (+ 5 5)) ((write (>= 10 10)) "This will work" (+ 5 5 5)))',
        ),
      ).toBe(10)
      expect(logSpy).not.toHaveBeenCalled()
    })
  })

  describe('defun', () => {
    test('samples', () => {
      expect(lispish.run('(defun add (a b) (+ a b)) (add 1 2)')).toBe(3)
      expect(() => lispish.run(`(defun add () 10)`)).not.toThrow()
      expect(() => lispish.run(`(defun x (a a) 10)`)).toThrow()
      expect(() => lispish.run(`(defun true () 10)`)).toThrow()
      expect(() => lispish.run(`(defun false () 10)`)).toThrow()
      expect(() => lispish.run(`(defun null () 10)`)).toThrow()
      expect(() => lispish.run(`(defun undefined () 10)`)).toThrow()
      expect(() => lispish.run(`(defun add ("s") 10)`)).toThrow()
      expect(() => lispish.run(`(defun "add" (a b) (+ a b))`)).toThrow()
      expect(() => lispish.run(`(defun add 1 (+ a b))`)).toThrow()
      expect(() => lispish.run(`(defun add (a b))`)).toThrow()
    })
    test('call defun function', () => {
      // expect(lispish.run(`(defun sumOneToN (n) (if (<= n 1) n (+ n (sumOneToN (- n 1))))) (sumOneToN 10)`)).toBe(55)
      expect(lispish.run("(defun applyWithVal (fn val) (fn val)) (applyWithVal #'1+ 10)")).toBe(11)
    })
  })

  describe('function', () => {
    test('samples', () => {
      lispish.run('(function +)')
      expect(() => lispish.run(`(function)`)).toThrow()
      expect(() => lispish.run(`(function "k")`)).toThrow()
      expect(() => lispish.run(`(function k s)`)).toThrow()
      expect(() => lispish.run(`(function add)`)).toThrow()
      expect(() => lispish.run(`(function true)`)).toThrow()
      expect(() => lispish.run(`(function false)`)).toThrow()
      expect(() => lispish.run(`(function null)`)).toThrow()
      expect(() => lispish.run(`(function undefined)`)).toThrow()
      expect(() => lispish.run(`(defun add (x y) (+ x y)) (function add)`)).not.toThrow()
    })
  })

  describe('lambda', () => {
    test('samples', () => {
      lispish.run('(lambda (x) (+ x 1))')
      lispish.run('(lambda () 1)')
      expect(() => lispish.run(`((lambda (x) (+ y 1)) 10)`)).toThrow()
      expect(() => lispish.run(`(lambda (false) 1)`)).toThrow()
      expect(() => lispish.run(`(lambda (true) 1)`)).toThrow()
      expect(() => lispish.run(`(lambda (null) 1)`)).toThrow()
      expect(() => lispish.run(`(lambda (undefined) 1)`)).toThrow()
      expect(() => lispish.run(`(lambda)`)).toThrow()
      expect(() => lispish.run(`(lambda (x))`)).toThrow()
      expect(() => lispish.run(`(lambda "k")`)).toThrow()
      expect(() => lispish.run(`(lambda k s)`)).toThrow()
      expect(() => lispish.run(`(lambda add)`)).toThrow()
    })
  })

  describe('return-from', () => {
    test('samples', () => {
      expect(() => lispish.run(`(return-from x (* 2 4))`)).toThrowError(ReturnFromSignal)
      expect(() => lispish.run(`(return-from x)`)).not.toThrowError(ReturnFromSignal)
      expect(() => lispish.run(`(return-from x)`)).toThrow()
      expect(() => lispish.run(`(return-from)`)).not.toThrowError(ReturnFromSignal)
      expect(() => lispish.run(`(return-from)`)).toThrow()
      expect(() => lispish.run(`(return-from "x" 10)`)).not.toThrowError(ReturnFromSignal)
      expect(() => lispish.run(`(return-from "x" 10)`)).toThrow()
      try {
        lispish.run('(return-from x (* 2 4))')
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
      expect(lispish.run(program)).toBe('Kalle')
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
      expect(lispish.run(program)).toBe(2)
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
      expect(lispish.run(program)).toBe(1)
    })
    test('return from lambda', () => {
      const program = `
        ((lambda ()
          (return "A")
          "B"
        ))
      `
      expect(lispish.run(program)).toBe('A')
    })
  })

  describe('return', () => {
    test('samples', () => {
      expect(() => lispish.run(`(return (* 2 4))`)).toThrowError(ReturnSignal)
      expect(() => lispish.run(`(return)`)).not.toThrowError(ReturnFromSignal)
      expect(() => lispish.run(`(return)`)).toThrow()
      try {
        lispish.run('(return (* 2 4))')
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
      expect(lispish.run(program)).toBe('Kalle')
    })
  })
  describe('block', () => {
    test('samples', () => {
      expect(
        lispish.run(`(block x
        (write "hej")
        (return-from x 10)
        (write "XXX")
      )`),
      ).toBe(10)
      expect(
        lispish.run(`(block x
        (write "hej")
        (write "XXX")
      )`),
      ).toBe('XXX')
      expect(() =>
        lispish.run(`(block x
        (write "hej")
        (return-from "x" 10)
        (write "XXX")
      )`),
      ).toThrow()
      expect(() =>
        lispish.run(`(block "x"
        (write "hej")
        (return-from x 10)
        (write "XXX")
      )`),
      ).toThrow()
      expect(() =>
        lispish.run(`(block x
        (write hej)
        (return-from x 10)
        (write "XXX")
      )`),
      ).toThrow()
    })
    test('asd', () => {
      expect(() =>
        lispish.run(`(block x
        (write hej)
        (return-from x 10)
        (write "XXX")
      )`),
      ).toThrow()
    })
  })

  describe('try', () => {
    test('samples', () => {
      expect(lispish.run(`(try (/ 2 4) ((error) 1))`)).toBe(0.5)
      expect(lispish.run(`(try (/ 2 0) ((error) 1))`)).toBe(1)
      expect(lispish.run(`(try (/ 2 0) ((error) error))`)).toBeInstanceOf(Error)
      expect(() => lispish.run(`(try (/ 2 4) 1)`)).toThrow()
      expect(() => lispish.run(`(try (/ 2 4) (1))`)).toThrow()
      expect(() => lispish.run(`(try (/ 2 4) (("error") 1))`)).toThrow()
      expect(() => lispish.run(`(try (/ 2 4) ((error1 error2) 1))`)).toThrow()
      expect(() => lispish.run(`(try (/ 2 4) ((error) 1 2))`)).toThrow()
      expect(() => lispish.run(`(try (/ 2 4) ((error) 1 )2)`)).toThrow()
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
      expect(lispish.run(program)).toBe('Two')
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
      expect(lispish.run(program)).toBe('Two')
    })
  })

  describe('throw', () => {
    test('samples', () => {
      expect(() => lispish.run(`(throw "An error")`)).toThrowError(UserDefinedError)
      expect(() => lispish.run(`(throw (substring "An error" 3))`)).toThrowError(UserDefinedError)
      expect(() => lispish.run(`(throw "An error" 10)`)).not.toThrowError(UserDefinedError)
      expect(() => lispish.run(`(throw "An error" 10)`)).toThrow()
      try {
        lispish.run('(throw (substring "An error" 3))')
        throw Error()
      } catch (error) {
        expect((error as UserDefinedError).message).toBe('error')
      }
    })
  })

  describe('when', () => {
    test('samples', () => {
      expect(lispish.run(`(when true (write 10) (write 20))`)).toBe(20)
      expect(lispish.run(`(when "Charles" (write 10) (write 20))`)).toBe(20)
      expect(lispish.run(`(when (> 10 20) (write 10) (write 20))`)).toBe(undefined)
      expect(lispish.run(`(when false)`)).toBe(undefined)
      expect(lispish.run(`(when true)`)).toBe(undefined)
      expect(() => lispish.run(`(when)`)).toThrow()
    })
  })

  describe('unless', () => {
    test('samples', () => {
      expect(lispish.run(`(unless true (write 10) (write 20))`)).toBeUndefined()
      expect(lispish.run(`(unless (> 10 20) (write 10) (write 20))`)).toBe(20)
      expect(lispish.run(`(unless null (write 10) (write 20))`)).toBe(20)
      expect(lispish.run(`(unless false)`)).toBe(undefined)
      expect(lispish.run(`(unless true)`)).toBe(undefined)
      expect(() => lispish.run(`(unless)`)).toThrow()
    })
  })

  describe('loop', () => {
    test('simple loop', () => {
      expect(
        lispish.run(`
        (let ((x 0))
          (loop
            (setq x (1+ x))
            (when (> x 5) (return x))
          )
        )`),
      ).toBe(6)
    })
    test('simple loop with throw', () => {
      expect(
        lispish.run(`
        (try
          (let ((x 0))
            (loop
              (setq x (1+ x))
              (when (> x 5) (throw "x is bigger than 5"))
            )
          )
          ((error) 10)
        )`),
      ).toBe(10)
    })
  })

  describe('dolist', () => {
    test('samples', () => {
      expect(() => lispish.run(`(setq l '(1 2 3)) (dolist el l)`)).toThrow()
    })

    test('dolist without result value', () => {
      expect(
        lispish.run(`
          (setq l '(1 2 3))
          (setq x 0)

          (dolist (el l)
            (setq x (+ x el))
          )

          x
        `),
      ).toBe(6)
    })

    test('dolist returns undefined if no return expression', () => {
      expect(
        lispish.run(`
          (setq l '(1 2 3))
          (dolist (el l))
        `),
      ).toBeUndefined()
    })

    test('dolist with result value', () => {
      expect(
        lispish.run(`
          (setq l '(1 2 3))

          (let ((x 0))
            (dolist (el l x)
              (setq x (+ x el))
            )
          )
        `),
      ).toBe(6)
    })

    test('dolist with return', () => {
      expect(
        lispish.run(`
          (setq l '(1 2 3))

          (let ((x 0))
            (dolist (el l x)
              (setq x (+ x el))
              (return x)
            )
          )
        `),
      ).toBe(1)
    })

    test('dolist with throw', () => {
      expect(() =>
        lispish.run(`
          (setq l '(1 2 3))

          (let ((x 0))
            (dolist (el l x)
              (setq x (+ x el))
              (throw "Oops")
            )
          )
        `),
      ).toThrow()
    })
  })

  describe('dotimes', () => {
    test('samples', () => {
      expect(() => lispish.run(`(dotimes x 5 x)`)).toThrow()
    })

    test('dotimes without result value', () => {
      expect(
        lispish.run(`
          (setq x 0)

          (dotimes (i 4)
            (setq x (+ x i))
          )

          x
        `),
      ).toBe(6)
    })

    test('dotimes returns undefined if no return expression', () => {
      expect(
        lispish.run(`
          (dotimes (el 5))
        `),
      ).toBeUndefined()
    })

    test('dotimes with result value', () => {
      expect(
        lispish.run(`
          (let ((x 0))
            (dotimes (i (+ 1 2 3) x)
              (setq x (+ x i))
            )
          )
        `),
      ).toBe(15)
    })

    test('dotimes with return', () => {
      expect(
        lispish.run(`
          (let ((x 0))
            (dotimes (i 5 x)
              (setq x (+ x i))
              (return 10)
            )
          )
        `),
      ).toBe(10)
    })

    test('dotimes with throw', () => {
      expect(() =>
        lispish.run(`
          (let ((x 0))
            (dotimes (i 100 x)
              (setq x (+ x i))
              (throw "Oops")
            )
          )
        `),
      ).toThrow()
    })
  })

  describe('while', () => {
    test('simple while', () => {
      expect(
        lispish.run(`
          (setq x 0)
          (while (<= x 10) (setq x (1+ x)))
          x
        `),
      ).toBe(11)
    })
  })
  describe('progn', () => {
    test('samples', () => {
      expect(lispish.run(`(progn '(1 2 3) "[1]" (+ 1 2))`)).toBe(3)
      expect(lispish.run(`(progn (object "a" 1) "a")`)).toBe('a')
      expect(lispish.run(`(progn)`)).toBeUndefined()
    })
  })
})
