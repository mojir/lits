import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src/Lits/Lits'
import { LitsError } from '../../../src/errors'
import { bitwiseUtilsModule } from '../../../src/builtin/modules/bitwise'

describe('bitwise', () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    describe('<<', () => {
      it('samples', () => {
        expect(lits.run('16 << 2')).toBe(64)
        expect(lits.run('<<(16, 2)')).toBe(64)
        expect(lits.run('<<(-16, 2)')).toBe(-64)
        expect(() => lits.run('<<()')).toThrow(LitsError)
        expect(() => lits.run('<<(1)')).toThrow(LitsError)
        expect(() => lits.run('<<(1, -2)')).toThrow(LitsError)
      })
    })
    describe('>>', () => {
      it('samples', () => {
        expect(lits.run('16 >> 2')).toBe(4)
        expect(lits.run('>>(16, 2)')).toBe(4)
        expect(lits.run('>>(-16, 2)')).toBe(-4)
        expect(() => lits.run('>>()')).toThrow(LitsError)
        expect(() => lits.run('>>(1)')).toThrow(LitsError)
        expect(() => lits.run('>>(1, -2)')).toThrow(LitsError)
      })
    })
    describe('>>>', () => {
      it('samples', () => {
        expect(lits.run('16 >>> 2')).toBe(4)
        expect(lits.run('>>>(16, 2)')).toBe(4)
        expect(lits.run('>>>(-16, 2)')).toBe(0x3FFFFFFC)
        expect(() => lits.run('>>>()')).toThrow(LitsError)
        expect(() => lits.run('>>>(1)')).toThrow(LitsError)
        expect(() => lits.run('>>>(1, -2)')).toThrow(LitsError)
      })
    })
    describe('&', () => {
      it('samples', () => {
        expect(lits.run('0b0011 & 0b1010')).toBe(0b0010)
        expect(lits.run('&(0b0011, 0b1010)')).toBe(0b0010)
        expect(lits.run('&(0b1111, 0b1010, 0b0101)')).toBe(0b0000)
        expect(lits.run('&(0b1111, 0b0111, 0b0011)')).toBe(0b0011)
        expect(() => lits.run('&()')).toThrow(LitsError)
        expect(() => lits.run('&(12)')).toThrow(LitsError)
        expect(() => lits.run('&(1, 2.1)')).toThrow(LitsError)
      })
    })
    describe('|', () => {
      it('samples', () => {
        expect(lits.run('0b0011 | 0b1010')).toBe(0b1011)
        expect(lits.run('|(0b0011, 0b1010)')).toBe(0b1011)
        expect(lits.run('|(0b0001, 0b0010, 0b0100)')).toBe(0b0111)
        expect(lits.run('|(0b0001, 0b0010, 0b1111)')).toBe(0b1111)
        expect(() => lits.run('|()')).toThrow(LitsError)
        expect(() => lits.run('|(12)')).toThrow(LitsError)
        expect(() => lits.run('|(1, 2.1)')).toThrow(LitsError)
      })
    })
    describe('xor', () => {
      it('samples', () => {
        expect(lits.run('0b0011 xor 0b1010')).toBe(0b1001)
        expect(lits.run('xor(0b0011, 0b1010)')).toBe(0b1001)
        expect(lits.run('xor(0b11110000, 0b00111100, 0b10101010)')).toBe(0b01100110)
        expect(() => lits.run('xor()')).toThrow(LitsError)
        expect(() => lits.run('xor(1)')).toThrow(LitsError)
      })
    })
  }

  for (const lits of [new Lits({ modules: [bitwiseUtilsModule] }), new Lits({ modules: [bitwiseUtilsModule], debug: true })]) {
    describe('bit-not', () => {
      it('samples', () => {
        expect(lits.run('let { bit-not } = import(bitwise); bit-not(0)')).toBe(-1)
        expect(lits.run('let { bit-not } = import(bitwise); bit-not(255)')).toBe(-256)
        expect(lits.run('let { bit-not } = import(bitwise); bit-not(0b1111)')).toBe(~Number('0b1111'))
        expect(lits.run('let { bit-not } = import(bitwise); bit-not(0xffff)')).toBe(~Number('0xffff'))
        expect(() => lits.run('let { bit-not } = import(bitwise); bit-not()')).toThrow(LitsError)
        expect(() => lits.run('let { bit-not } = import(bitwise); bit-not(1, 2)')).toThrow(LitsError)
      })
    })
    describe('bit-and-not', () => {
      it('samples', () => {
        expect(lits.run('let { bit-and-not } = import(bitwise); bit-and-not(0b1100, 0b1001)')).toBe(0b0100)
        expect(lits.run('let { bit-and-not } = import(bitwise); bit-and-not(0b1111, 0b1010, 0b1010)')).toBe(0b0101)
        expect(lits.run('let { bit-and-not } = import(bitwise); bit-and-not(0b1111, 0b0111, 0b0011)')).toBe(0b1000)
        expect(() => lits.run('let { bit-and-not } = import(bitwise); bit-and-not()')).toThrow(LitsError)
        expect(() => lits.run('let { bit-and-not } = import(bitwise); bit-and-not(12)')).toThrow(LitsError)
        expect(() => lits.run('let { bit-and-not } = import(bitwise); bit-and-not(1, 2.1)')).toThrow(LitsError)
      })
    })
    describe('bit-clear', () => {
      it('samples', () => {
        expect(lits.run('let { bit-clear } = import(bitwise); 0b1111 bit-clear 2')).toBe(0b1011)
        expect(lits.run('let { bit-clear } = import(bitwise); bit-clear(0b1111, 2)')).toBe(0b1011)
        expect(lits.run('let { bit-clear } = import(bitwise); bit-clear(0b1111, 5)')).toBe(0b1111)
      })
    })
    describe('bit-flip', () => {
      it('samples', () => {
        expect(lits.run('let { bit-flip } = import(bitwise); 0b1111 bit-flip 2')).toBe(0b1011)
        expect(lits.run('let { bit-flip } = import(bitwise); bit-flip(0b1111, 2)')).toBe(0b1011)
        expect(lits.run('let { bit-flip } = import(bitwise); bit-flip(0, 2)')).toBe(0b100)
      })
    })
    describe('bit-set', () => {
      it('samples', () => {
        expect(lits.run('let { bit-set } = import(bitwise); 0b1001 bit-set 2')).toBe(0b1101)
        expect(lits.run('let { bit-set } = import(bitwise); bit-set(0b1001, 2)')).toBe(0b1101)
        expect(lits.run('let { bit-set } = import(bitwise); bit-set(0, 2)')).toBe(0b100)
      })
    })
    describe('bit-test', () => {
      it('samples', () => {
        expect(lits.run('let { bit-test } = import(bitwise); 0b1001 bit-test 2')).toBe(false)
        expect(lits.run('let { bit-test } = import(bitwise); 0b1111 bit-test 2')).toBe(true)
        expect(lits.run('let { bit-test } = import(bitwise); bit-test(0b1001, 2)')).toBe(false)
        expect(lits.run('let { bit-test } = import(bitwise); bit-test(0b1111, 2)')).toBe(true)
      })
    })
  }
})
