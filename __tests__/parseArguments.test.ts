/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { lispish } from '../src'

describe('parseArguments', () => {
  describe('defun', () => {
    test('&rest params', () => {
      expect(() => lispish(`(defun test (&rest a) a)`)).not.toThrow()
      expect(() => lispish(`(defun test (&rest 1) a)`)).toThrow()
      expect(() => lispish(`(defun test (x &rest a) a)`)).not.toThrow()
      expect(() => lispish(`(defun test (x &rest) a)`)).toThrow()
      expect(() => lispish(`(defun test (x &rest a b) a)`)).toThrow()
      expect(() => lispish(`(defun test (x &rest a) a) (test 1)`)).not.toThrow()
      expect(() => lispish(`(defun test (x &rest a) a) (test)`)).toThrow()
      expect(() => lispish("(defun test (&rest a) a) (test #'+)")).toThrow()
      const program = `
        (defun test (first &rest rest) rest)
        (test 1 2 3)
      `
      expect(lispish(program)).toEqual([2, 3])
    })
    test('&optional params', () => {
      expect(() => lispish(`(defun test (&optional a) a)`)).not.toThrow()
      expect(lispish(`(defun test (&optional (a 10)) a) (test)`)).toBe(10)
      expect(lispish(`(defun test (&optional (a 10)) a) (test 5)`)).toBe(5)
      expect(() => lispish(`(defun test (&optional (a 10 20)) a)`)).toThrow()
      expect(() => lispish(`(defun test (&optional ("a" 10)) a)`)).toThrow()
      expect(() => lispish(`(defun test (x &optional a) a)`)).not.toThrow()
      expect(() => lispish(`(defun test (x &optional) a)`)).toThrow()
      expect(() => lispish(`(defun test (x &optional a b) a)`)).not.toThrow()
      expect(() => lispish(`(defun test (x &optional a) a) (test 1)`)).not.toThrow()
      expect(() => lispish(`(defun test (x &optional a) a) (test 1 2)`)).not.toThrow()
      expect(() => lispish(`(defun test (x &optional a) a) (test 1 2 3)`)).toThrow()
      expect(() => lispish(`(defun test (x &optional a) a) (test)`)).toThrow()
      expect(() => lispish("(defun test (&optional a) #'a) (test #'+)")).not.toThrow()
      expect(lispish(`(defun test (first &optional o) o) (test 1 2)`)).toEqual(2)
      expect(lispish(`(defun test (first &optional o) o) (test 1)`)).toEqual(undefined)
      expect(lispish(`(defun test (first &optional (o 10)) o) (test 1)`)).toEqual(10)
    })
    // TODO spread ...x ?
    test('modifier combinations', () => {
      expect(lispish("((lambda (&optional a &rest b) '(a b)))")).toEqual([undefined, []])
      expect(lispish("((lambda (&optional a &rest b) '(a b)) 1)")).toEqual([1, []])
      expect(lispish("((lambda (&optional a &rest b) '(a b)) 1 2)")).toEqual([1, [2]])
      expect(lispish("((lambda (&optional a &rest b) '(a b)) 1 2 3)")).toEqual([1, [2, 3]])
      expect(lispish("((lambda (a &optional b c &rest d) '(a b c d)) 1)")).toEqual([1, undefined, undefined, []])
      expect(lispish("((lambda (a &optional b c &rest d) '(a b c d)) 1 2)")).toEqual([1, 2, undefined, []])
      expect(lispish("((lambda (a &optional b c &rest d) '(a b c d)) 1 2 3)")).toEqual([1, 2, 3, []])
      expect(lispish("((lambda (a &optional b c &rest d) '(a b c d)) 1 2 3 4)")).toEqual([1, 2, 3, [4]])
      expect(lispish("((lambda (a &optional b c &rest d) '(a b c d)) 1 2 3 4 5)")).toEqual([1, 2, 3, [4, 5]])
      expect(() => lispish(`(lambda (&optional &rest a) a)`)).toThrow()
      expect(() => lispish(`(lambda (&optional &optional a) a)`)).toThrow()
      expect(() => lispish(`(lambda (&rest &rest a) a)`)).toThrow()
      expect(() => lispish(`(lambda ((a 10) &optional b) a)`)).toThrow()
      expect(() => lispish(`(lambda (a &optional a) a)`)).toThrow()
      expect(() => lispish(`(lambda (&rest &optional a) a)`)).toThrow()
      expect(() => lispish(`(lambda (&rest a &optional b) a)`)).toThrow()
      expect(() => lispish(`(lambda (&rst a) a)`)).toThrow()
      // expect(lispish(`(lambda (first &rest &optional (o 10)) o) (test 1)`)).toEqual(10)
    })
  })
})
