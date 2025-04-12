import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()

describe('variance functions', () => {
  describe('vec:variance', () => {
    it('should calculate the variance of a vector', () => {
      expect(lits.run('vec:variance([1, 2, 3])')).toEqual(0.6666666666666666)
      expect(lits.run('vec:variance([1, 2, 3, 4, 5, 6])')).toEqual(2.9166666666666665)
      expect(lits.run('vec:variance([1, -3, 2])')).toEqual(4.666666666666667)
      expect(lits.run('vec:variance([-1, -2, -3])')).toEqual(0.6666666666666666)
      expect(lits.run('vec:variance([0])')).toEqual(0)
      expect(() => lits.run('vec:variance([])')).toThrowError(LitsError)
    })
    it('should calculate the moving variance of a vector', () => {
      expect(lits.run('vec:moving-variance([1, 2, 4, 7, 11, 16], 1)')).toEqual([0, 0, 0, 0, 0, 0])
      expect(lits.run('vec:moving-variance([1, 2, 4, 7, 11, 16], 4)')).toEqual([5.25, 11.5, 20.25])
      expect(lits.run('vec:moving-variance([1, 2, 4, 7, 11, 16], 6)')).toEqual([27.805555555555557])
    })
    it('should calculate the centered moving variance of a vector with padding', () => {
      expect(lits.run('vec:centered-moving-variance([1, 2, 4, 7, 11, 16], 4)')).toEqual([null, null, 5.25, 11.5, 20.25, null])
      expect(lits.run('vec:centered-moving-variance([1, 2, 4, 7, 11, 16], 6)')).toEqual([null, null, null, 27.805555555555557, null, null])
    })
    it('should calculate the running variance of a vector', () => {
      expect(lits.run('vec:running-variance([1, 2, 3, 4, 5, 6])')).toEqual([0, 0.25, 0.6666666666666666, 1.25, 2, 2.9166666666666665])
      expect(lits.run('vec:running-variance([0])')).toEqual([0])
      expect(() => lits.run('vec:running-variance([])')).toThrowError(LitsError)
    })
  })

  describe('vec:sample-variance', () => {
    it('should calculate the sample variance of a vector', () => {
      expect(lits.run('vec:sample-variance([1, 2, 3])')).toEqual(1)
      expect(lits.run('vec:sample-variance([1, 2, 2, 3])')).toEqual(0.6666666666666666)
      expect(() => lits.run('vec:sample-variance([0])')).toThrowError(LitsError)
      expect(() => lits.run('vec:sample-variance([])')).toThrowError(LitsError)
    })
    it('should calculate the moving sample variance of a vector', () => {
      expect(lits.run('vec:moving-sample-variance([1, 2, 4, 7, 11, 16], 2)')).toEqual([0.5, 2, 4.5, 8, 12.5])
      expect(lits.run('vec:moving-sample-variance([1, 2, 4, 7, 11, 16], 6)')).toEqual([33.366666666666667])
    })
    it('should calculate the centered moving sample variance of a vector with padding', () => {
      expect(lits.run('vec:centered-moving-sample-variance([1, 2, 4], 3)')).toEqual([null, 2.333333333333333, null])
      expect(lits.run('vec:centered-moving-sample-variance([1, 2, 4], 3, 0, 5)')).toEqual([1, 2.333333333333333, 2.333333333333333])
      expect(lits.run('vec:centered-moving-sample-variance([1, 2, 4], 3, null, 5)')).toEqual([null, 2.333333333333333, 2.333333333333333])
      expect(lits.run('vec:centered-moving-sample-variance([1, 2, 4], 3, 0)')).toEqual([1, 2.333333333333333, null])
      expect(() => lits.run('vec:centered-moving-sample-variance([1, 2, 4], 1)')).toThrowError(LitsError)
    })
    it('should calculate the running sample variance of a vector', () => {
      expect(lits.run('vec:running-sample-variance([1, 2, 3])')).toEqual([null, 0.5, 1])
      expect(lits.run('vec:running-sample-variance([0, 1])')).toEqual([null, 0.5])
      expect(() => lits.run('vec:running-sample-variance([2])')).toThrowError(LitsError)
      expect(() => lits.run('vec:running-sample-variance([1])')).toThrowError(LitsError)
    })
  })
})
