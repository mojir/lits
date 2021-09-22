import { lispish } from '../src'

describe('shorthand', () => {
  describe('function shorthand ', () => {
    test('samples', () => {
      lispish(`#'+`)
      expect(() => lispish(`#'`)).toThrow()
      expect(() => lispish(`#'"k"`)).toThrow()
      expect(() => lispish(`#'k s`)).toThrow()
      expect(() => lispish(`#'add`)).toThrow()
      expect(() => lispish(`#'true`)).toThrow()
      expect(() => lispish(`#'false`)).toThrow()
      expect(() => lispish(`#'null`)).toThrow()
      expect(() => lispish(`#'undefined`)).toThrow()
      expect(() => lispish(`(defun add (x y) (+ x y)) #'add`)).not.toThrow()
    })
  })

  describe('list shorthand ', () => {
    test('samples', () => {
      expect(lispish(`'(1 2 3)`)).toEqual([1, 2, 3])
      expect(lispish(`'()`)).toEqual([])
      expect(lispish(`'((+ 1 2) "Text" null false undefined)`)).toEqual([3, 'Text', null, false, undefined])
    })
  })
})
