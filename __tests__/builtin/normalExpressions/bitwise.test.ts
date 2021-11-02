import { Lits } from '../../../src'

let lits: Lits

beforeEach(() => {
  lits = new Lits({ debug: true })
})

describe(`predicates`, () => {
  describe(`bit-not`, () => {
    test(`samples`, () => {
      expect(lits.run(`(bit-not 0)`)).toBe(-1)
      expect(lits.run(`(bit-not 255)`)).toBe(-256)
      expect(lits.run(`(bit-not 0b1111)`)).toBe(~Number(`0b1111`))
      expect(lits.run(`(bit-not 0xffff)`)).toBe(~Number(`0xffff`))
      expect(() => lits.run(`(bit-not)`)).toThrow()
      expect(() => lits.run(`(bit-not 1 2)`)).toThrow()
    })
  })
  describe(`bit-shift-left`, () => {
    test(`samples`, () => {
      expect(lits.run(`(bit-shift-left 16 2)`)).toBe(64)
      expect(lits.run(`(bit-shift-left -16 2)`)).toBe(-64)
      expect(() => lits.run(`(bit-shift-left)`)).toThrow()
      expect(() => lits.run(`(bit-shift-left 1)`)).toThrow()
      expect(() => lits.run(`(bit-shift-left 1 -2)`)).toThrow()
    })
  })
  describe(`bit-shift-right`, () => {
    test(`samples`, () => {
      expect(lits.run(`(bit-shift-right 16 2)`)).toBe(4)
      expect(lits.run(`(bit-shift-right -16 2)`)).toBe(-4)
      expect(() => lits.run(`(bit-shift-right)`)).toThrow()
      expect(() => lits.run(`(bit-shift-right 1)`)).toThrow()
      expect(() => lits.run(`(bit-shift-right 1 -2)`)).toThrow()
    })
  })
  describe(`bit-and`, () => {
    test(`samples`, () => {
      expect(lits.run(`(bit-and 0b0011 0b1010)`)).toBe(0b0010)
      expect(lits.run(`(bit-and 0b1111 0b1010 0b0101)`)).toBe(0b0000)
      expect(lits.run(`(bit-and 0b1111 0b0111 0b0011)`)).toBe(0b0011)
      expect(() => lits.run(`(bit-and)`)).toThrow()
      expect(() => lits.run(`(bit-and 12)`)).toThrow()
      expect(() => lits.run(`(bit-and 1 2.1)`)).toThrow()
    })
  })

  describe(`bit-and-not`, () => {
    test(`samples`, () => {
      expect(lits.run(`(bit-and-not 0b1100 0b1001)`)).toBe(0b0100)
      expect(lits.run(`(bit-and-not 0b1111 0b1010 0b1010)`)).toBe(0b0101)
      expect(lits.run(`(bit-and-not 0b1111 0b0111 0b0011)`)).toBe(0b1000)
      expect(() => lits.run(`(bit-and-not)`)).toThrow()
      expect(() => lits.run(`(bit-and-not 12)`)).toThrow()
      expect(() => lits.run(`(bit-and-not 1 2.1)`)).toThrow()
    })
  })

  describe(`bit-or`, () => {
    test(`samples`, () => {
      expect(lits.run(`(bit-or 0b0011 0b1010)`)).toBe(0b1011)
      expect(lits.run(`(bit-or 0b0001 0b0010 0b0100)`)).toBe(0b0111)
      expect(lits.run(`(bit-or 0b0001 0b0010 0b1111)`)).toBe(0b1111)
      expect(() => lits.run(`(bit-or)`)).toThrow()
      expect(() => lits.run(`(bit-or 12)`)).toThrow()
      expect(() => lits.run(`(bit-or 1 2.1)`)).toThrow()
    })
  })
  describe(`bit-xor`, () => {
    test(`samples`, () => {
      expect(lits.run(`(bit-xor 0b0011 0b1010)`)).toBe(0b1001)
      expect(lits.run(`(bit-xor 0b11110000 0b00111100 0b10101010)`)).toBe(0b01100110)
      expect(() => lits.run(`(bit-xor)`)).toThrow()
      expect(() => lits.run(`(bit-xor 1)`)).toThrow()
    })
  })
  describe(`bit-clear`, () => {
    test(`samples`, () => {
      expect(lits.run(`(bit-clear 0b1111 2)`)).toBe(0b1011)
      expect(lits.run(`(bit-clear 0b1111 5)`)).toBe(0b1111)
    })
  })
  describe(`bit-flip`, () => {
    test(`samples`, () => {
      expect(lits.run(`(bit-flip 0b1111 2)`)).toBe(0b1011)
      expect(lits.run(`(bit-flip 0 2)`)).toBe(0b100)
    })
  })
  describe(`bit-set`, () => {
    test(`samples`, () => {
      expect(lits.run(`(bit-set 0b1001 2)`)).toBe(0b1101)
      expect(lits.run(`(bit-set 0 2)`)).toBe(0b100)
    })
  })
  describe(`bit-test`, () => {
    test(`samples`, () => {
      expect(lits.run(`(bit-test 0b1001 2)`)).toBe(false)
      expect(lits.run(`(bit-test 0b1111 2)`)).toBe(true)
    })
  })
})
