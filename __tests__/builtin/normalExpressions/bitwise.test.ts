import { Lispish } from '../../../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe('predicates', () => {
  describe('lognot', () => {
    test('samples', () => {
      expect(lispish.run(`(lognot 0)`)).toBe(-1)
      expect(lispish.run(`(lognot 255)`)).toBe(-256)
      expect(lispish.run(`(lognot 0b1111)`)).toBe(~Number('0b1111'))
      expect(lispish.run(`(lognot 0xffff)`)).toBe(~Number('0xffff'))
      expect(() => lispish.run(`(lognot)`)).toThrow()
      expect(() => lispish.run(`(lognot 1 2)`)).toThrow()
    })
  })
  describe('logand', () => {
    test('samples', () => {
      expect(lispish.run(`(logand)`)).toBe(-1)
      expect(lispish.run(`(logand 12)`)).toBe(12)
      expect(lispish.run(`(logand 0b0011 0b1010)`)).toBe(0b0010)
      expect(lispish.run(`(logand 0b1111 0b1010 0b0101)`)).toBe(0b0000)
      expect(lispish.run(`(logand 0b1111 0b0111 0b0011)`)).toBe(0b0011)
      expect(() => lispish.run(`(logand 1 2.1)`)).toThrow()
    })
  })
  describe('logor', () => {
    test('samples', () => {
      expect(lispish.run(`(logor)`)).toBe(0)
      expect(lispish.run(`(logor 12)`)).toBe(12)
      expect(lispish.run(`(logor 0b0011 0b1010)`)).toBe(0b1011)
      expect(lispish.run(`(logor 0b0001 0b0010 0b0100)`)).toBe(0b0111)
      expect(lispish.run(`(logor 0b0001 0b0010 0b1111)`)).toBe(0b1111)
      expect(() => lispish.run(`(logor 1 2.1)`)).toThrow()
    })
  })
  describe('logxor', () => {
    test('samples', () => {
      expect(lispish.run(`(logxor 0b0011 0b1010)`)).toBe(0b1001)
      expect(() => lispish.run(`(logxor)`)).toThrow()
      expect(() => lispish.run(`(logxor 1)`)).toThrow()
      expect(() => lispish.run(`(logxor 1 1 1)`)).toThrow()
      expect(() => lispish.run(`(logxor 1 2.1)`)).toThrow()
    })
  })
})
