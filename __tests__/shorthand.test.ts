import { Lispish } from '../src'

const lispish = new Lispish()

describe('shorthand', () => {
  describe('function shorthand ', () => {
    test('samples', () => {
      lispish.run(`#'+`)
      expect(() => lispish.run(`#'`)).toThrow()
      expect(() => lispish.run(`#'"k"`)).toThrow()
      expect(() => lispish.run(`#'k s`)).toThrow()
      expect(() => lispish.run(`#'add`)).toThrow()
      expect(() => lispish.run(`#'true`)).toThrow()
      expect(() => lispish.run(`#'false`)).toThrow()
      expect(() => lispish.run(`#'null`)).toThrow()
      expect(() => lispish.run(`#'undefined`)).toThrow()
      expect(() => lispish.run(`(defun add (x y) (+ x y)) #'add`)).not.toThrow()
    })
  })

  describe('list shorthand ', () => {
    test('samples', () => {
      expect(lispish.run(`'(1 2 3)`)).toEqual([1, 2, 3])
      expect(lispish.run(`'()`)).toEqual([])
      expect(lispish.run(`'((+ 1 2) "Text" null false undefined)`)).toEqual([3, 'Text', null, false, undefined])
    })
  })
})
