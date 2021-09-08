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
})
