import { lispish } from '../../../src'

describe('predicates', () => {
  describe('lognot', () => {
    test('samples', () => {
      expect(lispish(`(lognot 0)`)).toBe(-1)
      expect(lispish(`(lognot 255)`)).toBe(-256)
      expect(lispish(`(lognot 0b1111)`)).toBe(~Number('0b1111'))
      expect(lispish(`(lognot 0xffff)`)).toBe(~Number('0xffff'))
      expect(() => lispish(`(lognot)`)).toThrow()
      expect(() => lispish(`(lognot 1 2)`)).toThrow()
    })
  })
  describe('logand', () => {
    test('samples', () => {
      expect(lispish(`(logand)`)).toBe(-1)
      expect(lispish(`(logand 12)`)).toBe(12)
      expect(lispish(`(logand 0b0011 0b1010)`)).toBe(0b0010)
      expect(lispish(`(logand 0b1111 0b1010 0b0101)`)).toBe(0b0000)
      expect(lispish(`(logand 0b1111 0b0111 0b0011)`)).toBe(0b0011)
      expect(() => lispish(`(logand 1 2.1)`)).toThrow()
    })
  })
  describe('logor', () => {
    test('samples', () => {
      expect(lispish(`(logor)`)).toBe(0)
      expect(lispish(`(logor 12)`)).toBe(12)
      expect(lispish(`(logor 0b0011 0b1010)`)).toBe(0b1011)
      expect(lispish(`(logor 0b0001 0b0010 0b0100)`)).toBe(0b0111)
      expect(lispish(`(logor 0b0001 0b0010 0b1111)`)).toBe(0b1111)
      expect(() => lispish(`(logor 1 2.1)`)).toThrow()
    })
  })
  describe('logxor', () => {
    test('samples', () => {
      expect(lispish(`(logxor 0b0011 0b1010)`)).toBe(0b1001)
      expect(() => lispish(`(logxor)`)).toThrow()
      expect(() => lispish(`(logxor 1)`)).toThrow()
      expect(() => lispish(`(logxor 1 1 1)`)).toThrow()
      expect(() => lispish(`(logxor 1 2.1)`)).toThrow()
    })
  })
})
