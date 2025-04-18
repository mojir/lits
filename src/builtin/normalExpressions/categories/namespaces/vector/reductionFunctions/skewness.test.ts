import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()

describe('skewness functions', () => {
  describe('vec:skewness', () => {
    it('should calculate the skewness of a vector', () => {
      expect(lits.run('vec:skewness([1, 2, 3, 6])')).toBeCloseTo(0.687243193)
      expect(lits.run('vec:skewness([1, 2, 2, 3])')).toEqual(0)
      expect(() => lits.run('vec:skewness([1, 1, 1, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:skewness([0, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:skewness([])')).toThrowError(LitsError)
    })
    it('should calculate the moving skewness of a vector', () => {
      expect(lits.run('vec:moving-skewness([1, 2, 3, 4, 5], 3)')).toEqual([0, 0, 0])
      expect(lits.run('vec:moving-skewness([1, 2, 3, 4, 5], 5)')).toEqual([0])
      expect(() => lits.run('vec:moving-skewness([1, 2], 2)')).toThrowError(LitsError)
    })
    it('should calculate the centered moving skewness of a vector with padding', () => {
      expect(lits.run('vec:centered-moving-skewness([1, 2, 4, 7, 11], 4)')).toEqual([null, null, 0.4987837491108398, 0.3461680709723672, null])
      expect(() => lits.run('vec:centered-moving-skewness([1, 2], 2)')).toThrowError(LitsError)
    })
    it('should calculate the running skewness of a vector', () => {
      expect(lits.run('vec:running-skewness([1, 2, 4, 7, 11])')).toEqual([null, null, 0.38180177416060584, 0.4987837491108398, 0.5504818825631803])
      expect(lits.run('vec:running-skewness([-1, -2, -3])')).toEqual([null, null, 0])
      expect(() => lits.run('vec:running-skewness([1, 2])')).toThrowError(LitsError)
    })
  })
  describe('vec:sample-skewness', () => {
    it('should calculate the skewness of a vector', () => {
      expect(lits.run('vec:sample-skewness([1, 2, 3, 6])')).toBeCloseTo(1.19034013)
      expect(lits.run('vec:sample-skewness([1, 2, 2, 3])')).toEqual(0)
      expect(() => lits.run('vec:sample-skewness([1, 1, 1, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:sample-skewness([0, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:sample-skewness([])')).toThrowError(LitsError)
    })
    it('should calculate the moving sample skewness of a vector', () => {
      expect(lits.run('vec:moving-sample-skewness([1, 2, 3, 4, 5], 3)')).toEqual([0, 0, 0])
      expect(lits.run('vec:moving-sample-skewness([1, 2, 3, 4, 5], 5)')).toEqual([0])
      expect(() => lits.run('vec:moving-sample-skewness([1, 2], 2)')).toThrowError(LitsError)
    })
    it('should calculate the centered moving sample skewness of a vector with padding', () => {
      expect(lits.run('vec:centered-moving-sample-skewness([1, 2, 4, 7, 11, 16], 4)')).toEqual([null, null, 0.8639187954496621, 0.5995806868822491, 0.4561779904708154, null])
      expect(() => lits.run('vec:centered-moving-sample-skewness([1, 2], 2)')).toThrowError(LitsError)
    })
    it('should calculate the running sample skewness of a vector', () => {
      expect(lits.run('vec:running-sample-skewness([1, 2, 4, 7, 11])')).toEqual([null, null, 0.9352195295828237, 0.8639187954496621, 0.8206099398622181])
      expect(lits.run('vec:running-sample-skewness([-1, -2, -3])')).toEqual([null, null, 0])
      expect(() => lits.run('vec:running-sample-skewness([1, 2])')).toThrowError(LitsError)
    })
  })
})
