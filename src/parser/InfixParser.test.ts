import { describe, expect, test } from 'vitest'
import { Lits } from '../'

const lits = new Lits({ infix: true })

describe('infix operators', () => {
  describe('**', () => {
    test('samples', () => {
      expect(lits.run('2 ** 3')).toBe(8)
      expect(lits.run('2 ** 3 ** 2')).toBe(512)
    })
  })
  describe('*', () => {
    test('samples', () => {
      expect(lits.run('-2 * 3')).toBe(-6)
      expect(lits.run('2 * 3 * 2')).toBe(12)
    })
  })
  describe('/', () => {
    test('samples', () => {
      expect(lits.run('6 / 3')).toBe(2)
      expect(lits.run('6 / 3 / 2')).toBe(1)
    })
  })
  describe('%', () => {
    test('samples', () => {
      expect(lits.run('6 % 3')).toBe(0)
      expect(lits.run('6 % 4')).toBe(2)
      expect(lits.run('-6 % 4')).toBe(-2)
      expect(lits.run('6 % -4')).toBe(2)
      expect(lits.run('-6 % -4')).toBe(-2)
    })
  })
  describe('+', () => {
    test('samples', () => {
      expect(lits.run('2 + 3')).toBe(5)
      expect(lits.run('2 + 3 + 2')).toBe(7)
    })
  })
  describe('-', () => {
    test('samples', () => {
      expect(lits.run('2 - 3')).toBe(-1)
      expect(lits.run('2 - 3 - 2')).toBe(-3)
    })
  })
  describe('<<', () => {
    test('samples', () => {
      expect(lits.run('2 << 3')).toBe(16)
      expect(lits.run('2 << 3 << 2')).toBe(64)
    })
  })
  describe('>>', () => {
    test('samples', () => {
      expect(lits.run('16 >> 3')).toBe(2)
      expect(lits.run('64 >> 3 >> 2')).toBe(2)
      expect(lits.run('-16 >> 2')).toBe(-4)
    })
  })
  describe('>>>', () => {
    test('samples', () => {
      expect(lits.run('16 >>> 3')).toBe(2)
      expect(lits.run('1 >>> 1')).toBe(0)
      expect(lits.run('1 >>> 2')).toBe(0)
      expect(lits.run('-16 >>> 2')).toBe(0x3FFFFFFC)
      expect(lits.run('64 >>> 3 >>> 2')).toBe(2)
    })
  })
  describe('<', () => {
    test('samples', () => {
      expect(lits.run('2 < 3')).toBe(true)
      expect(lits.run('2 < 2')).toBe(false)
      expect(lits.run('2 < 1')).toBe(false)
    })
  })
  describe('<=', () => {
    test('samples', () => {
      expect(lits.run('2 <= 3')).toBe(true)
      expect(lits.run('2 <= 2')).toBe(true)
      expect(lits.run('2 <= 1')).toBe(false)
    })
  })
  describe('>', () => {
    test('samples', () => {
      expect(lits.run('2 > 3')).toBe(false)
      expect(lits.run('2 > 2')).toBe(false)
      expect(lits.run('2 > 1')).toBe(true)
    })
  })
  describe('>=', () => {
    test('samples', () => {
      expect(lits.run('2 >= 3')).toBe(false)
      expect(lits.run('2 >= 2')).toBe(true)
      expect(lits.run('2 >= 1')).toBe(true)
    })
  })
  describe('==', () => {
    test('samples', () => {
      expect(lits.run('2 == 3')).toBe(false)
      expect(lits.run('2 == 2')).toBe(true)
      expect(lits.run('2 == 1')).toBe(false)
    })
  })
  describe('!=', () => {
    test('samples', () => {
      expect(lits.run('2 != 3')).toBe(true)
      expect(lits.run('2 != 2')).toBe(false)
      expect(lits.run('2 != 1')).toBe(true)
    })
  })
  describe('&', () => {
    test('samples', () => {
      expect(lits.run('0b1001 & 0b1000')).toBe(0b1000)
      expect(lits.run('0b1001 & 0b1000 & 0b0001')).toBe(0b0000)
    })
  })
  describe('|', () => {
    test('samples', () => {
      expect(lits.run('0b1001 | 0b0100')).toBe(0b1101)
      expect(lits.run('0b1001 | 0b0100 | 0b0010')).toBe(0b1111)
    })
  })
  describe('^', () => {
    test('samples', () => {
      expect(lits.run('0b1001 ^ 0b0100')).toBe(0b1101)
      expect(lits.run('0b1001 ^ 0b0100 ^ 0b0010')).toBe(0b1111)
    })
  })
  describe('&&', () => {
    test('samples', () => {
      expect(lits.run('true && true')).toBe(true)
      expect(lits.run('true && false')).toBe(false)
      expect(lits.run('false && true')).toBe(false)
      expect(lits.run('false && false')).toBe(false)
    })
  })
  describe('||', () => {
    test('samples', () => {
      expect(lits.run('true || true')).toBe(true)
      expect(lits.run('true || false')).toBe(true)
      expect(lits.run('false || true')).toBe(true)
      expect(lits.run('false || false')).toBe(false)
    })
  })
  describe('??', () => {
    test('samples', () => {
      expect(lits.run('1 ?? 2')).toBe(1)
      expect(lits.run('nil ?? 2')).toBe(2)
    })
  })
  describe('!', () => {
    test('samples', () => {
      expect(lits.run('!true')).toBe(false)
      expect(lits.run('!false')).toBe(true)
      expect(lits.run('!500')).toBe(false)
      expect(lits.run('!0')).toBe(true)
      expect(lits.run('!!500')).toBe(true)
      expect(lits.run('!!0')).toBe(false)
    })
  })
  describe('parenthises', () => {
    test('samples', () => {
      expect(lits.run('-(2 + 3) * 2')).toBe(-10)
      expect(lits.run('2 + (3 * 2)')).toBe(8)
    })
  })
  describe('objects', () => {
    test('samples', () => {
      expect(lits.run('{ a=2+3 }')).toEqual({ a: 5 })
      expect(lits.run('{ a=10 }')).toEqual({ a: 10 })
      expect(lits.run('{ a=10, b=2+3 }')).toEqual({ a: 10, b: 5 })
      expect(lits.run('{ a=10, b=20, c = 2 * (1 - 2) }')).toEqual({ a: 10, b: 20, c: -2 })
    })
  })
  describe('arrays', () => {
    test('samples', () => {
      expect(lits.run('[2+3]')).toEqual([5])
      expect(lits.run('[10]')).toEqual([10])
      expect(lits.run('[10, 2+3]')).toEqual([10, 5])
      expect(lits.run('[10, 20, 2 * (1 - 2)]')).toEqual([10, 20, -2])
    })
  })
})
