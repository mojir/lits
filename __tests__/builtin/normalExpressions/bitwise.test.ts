import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src'

describe('predicates', () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    describe('~', () => {
      it('samples', () => {
        expect(lits.run('(~ 0)')).toBe(-1)
        expect(lits.run('(~ 255)')).toBe(-256)
        expect(lits.run('(~ 0b1111)')).toBe(~Number('0b1111'))
        expect(lits.run('(~ 0xffff)')).toBe(~Number('0xffff'))
        expect(() => lits.run('(~)')).toThrow()
        expect(() => lits.run('(~ 1 2)')).toThrow()
      })
    })
    describe('<<', () => {
      it('samples', () => {
        expect(lits.run('(<< 16 2)')).toBe(64)
        expect(lits.run('(<< -16 2)')).toBe(-64)
        expect(() => lits.run('(<<)')).toThrow()
        expect(() => lits.run('(<< 1)')).toThrow()
        expect(() => lits.run('(<< 1 -2)')).toThrow()
      })
    })
    describe('>>', () => {
      it('samples', () => {
        expect(lits.run('(>> 16 2)')).toBe(4)
        expect(lits.run('(>> -16 2)')).toBe(-4)
        expect(() => lits.run('(>>)')).toThrow()
        expect(() => lits.run('(>> 1)')).toThrow()
        expect(() => lits.run('(>> 1 -2)')).toThrow()
      })
    })
    describe('>>>', () => {
      it('samples', () => {
        expect(lits.run('(>>> 16 2)')).toBe(4)
        expect(lits.run('(>>> -16 2)')).toBe(0x3FFFFFFC)
        expect(() => lits.run('(>>>)')).toThrow()
        expect(() => lits.run('(>>> 1)')).toThrow()
        expect(() => lits.run('(>>> 1 -2)')).toThrow()
      })
    })
    describe('&', () => {
      it('samples', () => {
        expect(lits.run('(& 0b0011 0b1010)')).toBe(0b0010)
        expect(lits.run('(& 0b1111 0b1010 0b0101)')).toBe(0b0000)
        expect(lits.run('(& 0b1111 0b0111 0b0011)')).toBe(0b0011)
        expect(() => lits.run('(&)')).toThrow()
        expect(() => lits.run('(& 12)')).toThrow()
        expect(() => lits.run('(& 1 2.1)')).toThrow()
      })
    })

    describe('&!', () => {
      it('samples', () => {
        expect(lits.run('(&! 0b1100 0b1001)')).toBe(0b0100)
        expect(lits.run('(&! 0b1111 0b1010 0b1010)')).toBe(0b0101)
        expect(lits.run('(&! 0b1111 0b0111 0b0011)')).toBe(0b1000)
        expect(() => lits.run('(&!)')).toThrow()
        expect(() => lits.run('(&! 12)')).toThrow()
        expect(() => lits.run('(&! 1 2.1)')).toThrow()
      })
    })

    describe('|', () => {
      it('samples', () => {
        expect(lits.run('(| 0b0011 0b1010)')).toBe(0b1011)
        expect(lits.run('(| 0b0001 0b0010 0b0100)')).toBe(0b0111)
        expect(lits.run('(| 0b0001 0b0010 0b1111)')).toBe(0b1111)
        expect(() => lits.run('(|)')).toThrow()
        expect(() => lits.run('(| 12)')).toThrow()
        expect(() => lits.run('(| 1 2.1)')).toThrow()
      })
    })
    describe('^', () => {
      it('samples', () => {
        expect(lits.run('(^ 0b0011 0b1010)')).toBe(0b1001)
        expect(lits.run('(^ 0b11110000 0b00111100 0b10101010)')).toBe(0b01100110)
        expect(() => lits.run('(^)')).toThrow()
        expect(() => lits.run('(^ 1)')).toThrow()
      })
    })
    describe('bit_clear', () => {
      it('samples', () => {
        expect(lits.run('(bit_clear 0b1111 2)')).toBe(0b1011)
        expect(lits.run('(bit_clear 0b1111 5)')).toBe(0b1111)
      })
    })
    describe('bit_flip', () => {
      it('samples', () => {
        expect(lits.run('(bit_flip 0b1111 2)')).toBe(0b1011)
        expect(lits.run('(bit_flip 0 2)')).toBe(0b100)
      })
    })
    describe('bit_set', () => {
      it('samples', () => {
        expect(lits.run('(bit_set 0b1001 2)')).toBe(0b1101)
        expect(lits.run('(bit_set 0 2)')).toBe(0b100)
      })
    })
    describe('bit_test', () => {
      it('samples', () => {
        expect(lits.run('(bit_test 0b1001 2)')).toBe(false)
        expect(lits.run('(bit_test 0b1111 2)')).toBe(true)
      })
    })
  }
})
