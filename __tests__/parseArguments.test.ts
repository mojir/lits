/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lispish } from '../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`parseArguments`, () => {
  describe(`defun`, () => {
    test(`&rest params`, () => {
      expect(() => lispish.run(`(defun foo (&rest a) a)`)).not.toThrow()
      expect(() => lispish.run(`(defun foo (&rest 1) a)`)).toThrow()
      expect(() => lispish.run(`(defun foo (x &rest a) a)`)).not.toThrow()
      expect(() => lispish.run(`(defun foo (x &rest) a)`)).toThrow()
      expect(() => lispish.run(`(defun foo (x &rest a b) a)`)).toThrow()
      expect(() => lispish.run(`(defun foo (x &rest a) a) (foo 1)`)).not.toThrow()
      expect(() => lispish.run(`(defun foo (x &rest a) a) (foo)`)).toThrow()
      expect(() => lispish.run(`(defun foo (&rest a) a) (foo +)`)).toThrow()
      const program = `
        (defun foo (first &rest rest) rest)
        (foo 1 2 3)
      `
      expect(lispish.run(program)).toEqual([2, 3])
    })
    test(`&optional params`, () => {
      expect(() => lispish.run(`(defun test (&optional a) a)`)).not.toThrow()
      expect(lispish.run(`(defun foo (&optional (a 10)) a) (foo)`)).toBe(10)
      expect(lispish.run(`(defun foo (&optional (a 10)) a) (foo 5)`)).toBe(5)
      expect(() => lispish.run(`(defun foo (&optional (a 10 20)) a)`)).toThrow()
      expect(() => lispish.run(`(defun foo (&optional ("a" 10)) a)`)).toThrow()
      expect(() => lispish.run(`(defun foo (x &optional a) a)`)).not.toThrow()
      expect(() => lispish.run(`(defun foo (x &optional) a)`)).toThrow()
      expect(() => lispish.run(`(defun foo (x &optional a b) a)`)).not.toThrow()
      expect(() => lispish.run(`(defun foo (x &optional a) a) (foo 1)`)).not.toThrow()
      expect(() => lispish.run(`(defun foo (x &optional a) a) (foo 1 2)`)).not.toThrow()
      expect(() => lispish.run(`(defun foo (x &optional a) a) (foo 1 2 3)`)).toThrow()
      expect(() => lispish.run(`(defun foo (x &optional a) a) (foo)`)).toThrow()
      expect(() => lispish.run(`(defun foo (&optional a) a) (foo +)`)).not.toThrow()
      expect(lispish.run(`(defun foo (first &optional o) o) (foo 1 2)`)).toEqual(2)
      expect(lispish.run(`(defun foo (first &optional o) o) (foo 1)`)).toEqual(undefined)
      expect(lispish.run(`(defun foo (first &optional (o 10)) o) (foo 1)`)).toEqual(10)
    })

    test(`modifier combinations`, () => {
      expect(lispish.run(`((lambda (&optional a &rest b) [a b]))`)).toEqual([undefined, []])
      expect(lispish.run(`((lambda (&optional a &rest b) [a b]) 1)`)).toEqual([1, []])
      expect(lispish.run(`((lambda (&optional a &rest b) [a b]) 1 2)`)).toEqual([1, [2]])
      expect(lispish.run(`((lambda (&optional a &rest b) [a b]) 1 2 3)`)).toEqual([1, [2, 3]])
      expect(lispish.run(`((lambda (a &optional b c &rest d) [a b c d]) 1)`)).toEqual([1, undefined, undefined, []])
      expect(lispish.run(`((lambda (a &optional b c &rest d) [a b c d]) 1 2)`)).toEqual([1, 2, undefined, []])
      expect(lispish.run(`((lambda (a &optional b c &rest d) [a b c d]) 1 2 3)`)).toEqual([1, 2, 3, []])
      expect(lispish.run(`((lambda (a &optional b c &rest d) [a b c d]) 1 2 3 4)`)).toEqual([1, 2, 3, [4]])
      expect(lispish.run(`((lambda (a &optional b c &rest d) [a b c d]) 1 2 3 4 5)`)).toEqual([1, 2, 3, [4, 5]])
      expect(lispish.run(`((lambda (&optional a &rest b &bind ((x (+ 5 5)))) [a b x]))`)).toEqual([undefined, [], 10])
      expect(lispish.run(`((lambda (&optional a &rest b &bind ((x (+ 5 5)))) [a b x]) 1)`)).toEqual([1, [], 10])
      expect(lispish.run(`((lambda (&optional a &rest b &bind ((x (+ 5 5)))) [a b x]) 1 2)`)).toEqual([1, [2], 10])
      expect(lispish.run(`((lambda (&optional a &rest b &bind ((x (+ 5 5)))) [a b x]) 1 2 3)`)).toEqual([1, [2, 3], 10])
      expect(lispish.run(`((lambda (a &optional b c &rest d &bind ((x (+ 5 5)))) [a b c d x]) 1)`)).toEqual([
        1,
        undefined,
        undefined,
        [],
        10,
      ])
      expect(lispish.run(`((lambda (a &optional b c &rest d &bind ((x (+ 5 5)))) [a b c d x]) 1 2)`)).toEqual([
        1,
        2,
        undefined,
        [],
        10,
      ])
      expect(lispish.run(`((lambda (a &optional b c &rest d &bind ((x (+ 5 5)))) [a b c d x]) 1 2 3)`)).toEqual([
        1,
        2,
        3,
        [],
        10,
      ])
      expect(lispish.run(`((lambda (a &optional b c &rest d &bind ((x (+ 5 5)))) [a b c d x]) 1 2 3 4)`)).toEqual([
        1,
        2,
        3,
        [4],
        10,
      ])
      expect(lispish.run(`((lambda (a &optional b c &rest d &bind ((x (+ 5 5)))) [a b c d x]) 1 2 3 4 5)`)).toEqual([
        1,
        2,
        3,
        [4, 5],
        10,
      ])
      expect(() => lispish.run(`(lambda (&optional a &rest b &bind ()) a)`)).not.toThrow()
      expect(() => lispish.run(`(lambda (&bind (a 1)) a)`)).toThrow()
      expect(() => lispish.run(`(lambda (&optional a &rest b &bind () &bind ()) a)`)).toThrow()
      expect(() => lispish.run(`(lambda (&optional a &bind () &rest b) a)`)).toThrow()
      expect(() => lispish.run(`(lambda (&optional &bind ()) null)`)).toThrow()
      expect(() => lispish.run(`(lambda (&rest &bind ()) null)`)).toThrow()
      expect(() => lispish.run(`(lambda (&bind () &optional a &rest b) a)`)).toThrow()
      expect(() => lispish.run(`(lambda (&optional &rest a) a)`)).toThrow()
      expect(() => lispish.run(`(lambda (&optional &optional a) a)`)).toThrow()
      expect(() => lispish.run(`(lambda (&rest &rest a) a)`)).toThrow()
      expect(() => lispish.run(`(lambda ((a 10) &optional b) a)`)).toThrow()
      expect(() => lispish.run(`(lambda (a &optional a) a)`)).toThrow()
      expect(() => lispish.run(`(lambda (&rest &optional a) a)`)).toThrow()
      expect(() => lispish.run(`(lambda (&rest a &optional b) a)`)).toThrow()
      expect(() => lispish.run(`(lambda (&rst a) a)`)).toThrow()
    })
  })
})
