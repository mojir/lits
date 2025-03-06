import { describe, expect, it } from 'vitest'
import { Lits } from '../../..'
import { tokenizeP_Number } from '../polishTokenizers'

describe('parse numbers', () => {
  for (const lits of [new Lits({ polish: true }), new Lits({ debug: true, polish: true })]) {
    describe('valid numbers', () => {
      const numberSamples = [
        '0',
        '-0',
        '.0',
        '-.0',
        '123',
        '-123',
        '.12',
        '0.12',
        '-.12',
        '-0.12',
        '123.123',
        '-123.123',
        '0x1234567890abcdefABCDEF',
        '0o12345670',
        '0b10',
        '0X1234567890abcdefABCDEF',
        '0O12345670',
        '0B10',
      ]

      numberSamples.forEach((sample) => {
        it(`number sample: ${sample}`, () => {
          expect(tokenizeP_Number(sample, 0)).toEqual([sample.length, ['P_Number', sample]])
          expect(lits.run(sample) === Number(sample)).toBe(true)
        })
      })
    })
    describe('invalid numbers', () => {
      const numberSamples = [
        '.',
        '0x',
        '0X',
        '0b',
        '0B',
        '0o',
        '0O',
        '1.1.',
        '0x1.1',
        '0b1.1',
        '0o1.1',
        '-0x1234567890abcdefABCDEF',
        '-0o12345670',
        '-0b10',
        '-0X1234567890abcdefABCDEF',
        '-0O12345670',
        '-0B10',
      ]

      numberSamples.forEach((sample) => {
        it(`number sample: ${sample}`, () => {
          expect(tokenizeP_Number(sample, 0)).toEqual([0])
        })
      })
    })
  }
})
