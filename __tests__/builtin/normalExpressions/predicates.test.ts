import { lispish } from '../../../src'

describe('predicates', () => {
  describe('stringp', () => {
    test('samples', () => {
      expect(lispish(`(stringp "")`)).toBe(true)
      expect(lispish(`(stringp "x")`)).toBe(true)
      expect(lispish(`(stringp 1)`)).toBe(false)
      expect(lispish(`(stringp 0)`)).toBe(false)
      expect(lispish(`(stringp (list))`)).toBe(false)
      expect(lispish(`(stringp (object))`)).toBe(false)
      expect(lispish(`(stringp null)`)).toBe(false)
      expect(lispish(`(stringp true)`)).toBe(false)
      expect(lispish(`(stringp false)`)).toBe(false)
      expect(lispish(`(stringp undefined)`)).toBe(false)
      expect(() => lispish(`(stringp)`)).toThrow()
      expect(() => lispish(`(stringp "k" "k")`)).toThrow()
    })
  })

  describe('numberp', () => {
    test('samples', () => {
      expect(lispish(`(numberp 1)`)).toBe(true)
      expect(lispish(`(numberp 0)`)).toBe(true)
      expect(lispish(`(numberp -1)`)).toBe(true)
      expect(lispish(`(numberp -1.123)`)).toBe(true)
      expect(lispish(`(numberp 0.123)`)).toBe(true)
      expect(lispish(`(numberp "")`)).toBe(false)
      expect(lispish(`(numberp "x")`)).toBe(false)
      expect(lispish(`(numberp (list))`)).toBe(false)
      expect(lispish(`(numberp (object))`)).toBe(false)
      expect(lispish(`(numberp null)`)).toBe(false)
      expect(lispish(`(numberp false)`)).toBe(false)
      expect(lispish(`(numberp true)`)).toBe(false)
      expect(lispish(`(numberp undefined)`)).toBe(false)
      expect(() => lispish(`(numberp)`)).toThrow()
      expect(() => lispish(`(numberp 1 2)`)).toThrow()
    })
  })

  describe('booleanp', () => {
    test('samples', () => {
      expect(lispish(`(booleanp 1)`)).toBe(false)
      expect(lispish(`(booleanp 0)`)).toBe(false)
      expect(lispish(`(booleanp -1)`)).toBe(false)
      expect(lispish(`(booleanp -1.123)`)).toBe(false)
      expect(lispish(`(booleanp 0.123)`)).toBe(false)
      expect(lispish(`(booleanp "")`)).toBe(false)
      expect(lispish(`(booleanp "x")`)).toBe(false)
      expect(lispish(`(booleanp (list))`)).toBe(false)
      expect(lispish(`(booleanp (object))`)).toBe(false)
      expect(lispish(`(booleanp null)`)).toBe(false)
      expect(lispish(`(booleanp false)`)).toBe(true)
      expect(lispish(`(booleanp true)`)).toBe(true)
      expect(lispish(`(booleanp undefined)`)).toBe(false)
      expect(() => lispish(`(booleanp)`)).toThrow()
      expect(() => lispish(`(booleanp true false)`)).toThrow()
    })
  })

  describe('undefinedp', () => {
    test('samples', () => {
      expect(lispish(`(undefinedp 1)`)).toBe(false)
      expect(lispish(`(undefinedp 0)`)).toBe(false)
      expect(lispish(`(undefinedp -1)`)).toBe(false)
      expect(lispish(`(undefinedp -1.123)`)).toBe(false)
      expect(lispish(`(undefinedp 0.123)`)).toBe(false)
      expect(lispish(`(undefinedp "")`)).toBe(false)
      expect(lispish(`(undefinedp "x")`)).toBe(false)
      expect(lispish(`(undefinedp (list))`)).toBe(false)
      expect(lispish(`(undefinedp (object))`)).toBe(false)
      expect(lispish(`(undefinedp null)`)).toBe(false)
      expect(lispish(`(undefinedp false)`)).toBe(false)
      expect(lispish(`(undefinedp true)`)).toBe(false)
      expect(lispish(`(undefinedp undefined)`)).toBe(true)
      expect(() => lispish(`(undefinedp)`)).toThrow()
      expect(() => lispish(`(undefinedp true false)`)).toThrow()
    })
  })

  describe('nullp', () => {
    test('samples', () => {
      expect(lispish(`(nullp 1)`)).toBe(false)
      expect(lispish(`(nullp 0)`)).toBe(false)
      expect(lispish(`(nullp -1)`)).toBe(false)
      expect(lispish(`(nullp -1.123)`)).toBe(false)
      expect(lispish(`(nullp 0.123)`)).toBe(false)
      expect(lispish(`(nullp "")`)).toBe(false)
      expect(lispish(`(nullp "x")`)).toBe(false)
      expect(lispish(`(nullp (list))`)).toBe(false)
      expect(lispish(`(nullp (object))`)).toBe(false)
      expect(lispish(`(nullp null)`)).toBe(true)
      expect(lispish(`(nullp false)`)).toBe(false)
      expect(lispish(`(nullp true)`)).toBe(false)
      expect(lispish(`(nullp undefined)`)).toBe(false)
      expect(() => lispish(`(nullp)`)).toThrow()
      expect(() => lispish(`(nullp true false)`)).toThrow()
    })
  })

  describe('zerop', () => {
    test('samples', () => {
      expect(lispish(`(zerop 1)`)).toBe(false)
      expect(lispish(`(zerop 0)`)).toBe(true)
      expect(lispish(`(zerop -0)`)).toBe(true)
      expect(lispish(`(zerop (/ 0 -1))`)).toBe(true)
      expect(lispish(`(zerop -1)`)).toBe(false)
      expect(() => lispish(`(zerop)`)).toThrow()
      expect(() => lispish(`(zerop "")`)).toThrow()
      expect(() => lispish(`(zerop true)`)).toThrow()
      expect(() => lispish(`(zerop false)`)).toThrow()
      expect(() => lispish(`(zerop null)`)).toThrow()
      expect(() => lispish(`(zerop undefined)`)).toThrow()
      expect(() => lispish(`(zerop (object))`)).toThrow()
      expect(() => lispish(`(zerop (list))`)).toThrow()
    })
  })

  describe('evenp', () => {
    test('samples', () => {
      expect(lispish(`(evenp 1)`)).toBe(false)
      expect(lispish(`(evenp 0)`)).toBe(true)
      expect(lispish(`(evenp -0)`)).toBe(true)
      expect(lispish(`(evenp (/ 0 -1))`)).toBe(true)
      expect(lispish(`(evenp -1)`)).toBe(false)
      expect(lispish(`(evenp -10)`)).toBe(true)
      expect(lispish(`(evenp -2.001)`)).toBe(false)
      expect(lispish(`(evenp 4)`)).toBe(true)
      expect(() => lispish(`(evenp)`)).toThrow()
      expect(() => lispish(`(evenp "")`)).toThrow()
      expect(() => lispish(`(evenp true)`)).toThrow()
      expect(() => lispish(`(evenp false)`)).toThrow()
      expect(() => lispish(`(evenp null)`)).toThrow()
      expect(() => lispish(`(evenp undefined)`)).toThrow()
      expect(() => lispish(`(evenp (object))`)).toThrow()
      expect(() => lispish(`(evenp (list))`)).toThrow()
    })
  })

  describe('oddp', () => {
    test('samples', () => {
      expect(lispish(`(oddp 1)`)).toBe(true)
      expect(lispish(`(oddp 0)`)).toBe(false)
      expect(lispish(`(oddp -0)`)).toBe(false)
      expect(lispish(`(oddp (/ 0 -1))`)).toBe(false)
      expect(lispish(`(oddp -1)`)).toBe(true)
      expect(lispish(`(oddp -10)`)).toBe(false)
      expect(lispish(`(oddp -2.001)`)).toBe(false)
      expect(lispish(`(oddp 4)`)).toBe(false)
      expect(lispish(`(oddp 5)`)).toBe(true)
      expect(() => lispish(`(oddp)`)).toThrow()
      expect(() => lispish(`(oddp "")`)).toThrow()
      expect(() => lispish(`(oddp true)`)).toThrow()
      expect(() => lispish(`(oddp false)`)).toThrow()
      expect(() => lispish(`(oddp null)`)).toThrow()
      expect(() => lispish(`(oddp undefined)`)).toThrow()
      expect(() => lispish(`(oddp (object))`)).toThrow()
      expect(() => lispish(`(oddp (list))`)).toThrow()
    })
  })

  describe('arrayp', () => {
    test('samples', () => {
      expect(lispish(`(arrayp 1)`)).toBe(false)
      expect(lispish(`(arrayp 0)`)).toBe(false)
      expect(lispish(`(arrayp -1)`)).toBe(false)
      expect(lispish(`(arrayp -1.123)`)).toBe(false)
      expect(lispish(`(arrayp 0.123)`)).toBe(false)
      expect(lispish(`(arrayp "")`)).toBe(false)
      expect(lispish(`(arrayp "x")`)).toBe(false)
      expect(lispish(`(arrayp (list))`)).toBe(true)
      expect(lispish(`(arrayp (object))`)).toBe(false)
      expect(lispish(`(arrayp null)`)).toBe(false)
      expect(lispish(`(arrayp false)`)).toBe(false)
      expect(lispish(`(arrayp true)`)).toBe(false)
      expect(lispish(`(arrayp undefined)`)).toBe(false)
      expect(() => lispish(`(arrayp)`)).toThrow()
      expect(() => lispish(`(arrayp true false)`)).toThrow()
    })
  })

  describe('objectp', () => {
    test('samples', () => {
      expect(lispish(`(objectp 1)`)).toBe(false)
      expect(lispish(`(objectp 0)`)).toBe(false)
      expect(lispish(`(objectp -1)`)).toBe(false)
      expect(lispish(`(objectp -1.123)`)).toBe(false)
      expect(lispish(`(objectp 0.123)`)).toBe(false)
      expect(lispish(`(objectp "")`)).toBe(false)
      expect(lispish(`(objectp "x")`)).toBe(false)
      expect(lispish(`(objectp (list))`)).toBe(false)
      expect(lispish(`(objectp (object "x" 10))`)).toBe(true)
      expect(lispish(`(objectp null)`)).toBe(false)
      expect(lispish(`(objectp false)`)).toBe(false)
      expect(lispish(`(objectp true)`)).toBe(false)
      expect(lispish(`(objectp undefined)`)).toBe(false)
      expect(() => lispish(`(objectp)`)).toThrow()
      expect(() => lispish(`(objectp true false)`)).toThrow()
    })
  })
})