import { Lispish } from '../../../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`collection functions`, () => {
  describe(`count`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(count [])`)).toBe(0)
      expect(lispish.run(`(count [1])`)).toBe(1)
      expect(lispish.run(`(count [1 2 3])`)).toBe(3)
      expect(lispish.run(`(count (object))`)).toBe(0)
      expect(lispish.run(`(count (object "a" 1 "b" 2))`)).toBe(2)
      expect(() => lispish.run(`(count "")`)).toThrow()
      expect(() => lispish.run(`(count "1")`)).toThrow()
      expect(() => lispish.run(`(count "123")`)).toThrow()
      expect(() => lispish.run(`(count)`)).toThrow()
      expect(() => lispish.run(`(count [] [])`)).toThrow()
      expect(() => lispish.run(`(count 12)`)).toThrow()
      expect(() => lispish.run(`(count false)`)).toThrow()
      expect(() => lispish.run(`(count true)`)).toThrow()
      expect(() => lispish.run(`(count null)`)).toThrow()
      expect(() => lispish.run(`(count undefined)`)).toThrow()
    })
  })
})
