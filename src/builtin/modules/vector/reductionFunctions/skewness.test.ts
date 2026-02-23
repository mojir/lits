import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { vectorModule } from '..'
import { LitsError } from '../../../../errors'

const lits = new Lits({ modules: [vectorModule] })

// Helper to run vec module functions with the new import syntax
function runVec(code: string): unknown {
  // Add module import prefix to function calls
  const modifiedCode = `let v = import(vector); v.${code}`
  return lits.run(modifiedCode)
}

describe('skewness functions', () => {
  describe('skewness', () => {
    it('should calculate the skewness of a vector', () => {
      expect(runVec('skewness([1, 2, 3, 6])')).toBeCloseTo(0.687243193)
      expect(runVec('skewness([1, 2, 2, 3])')).toEqual(0)
      expect(() => runVec('skewness([1, 1, 1, 1])')).toThrowError(LitsError)
      expect(() => runVec('skewness([0, 1])')).toThrowError(LitsError)
      expect(() => runVec('skewness([])')).toThrowError(LitsError)
    })
    it('should calculate the moving skewness of a vector', () => {
      expect(runVec('moving-skewness([1, 2, 3, 4, 5], 3)')).toEqual([0, 0, 0])
      expect(runVec('moving-skewness([1, 2, 3, 4, 5], 5)')).toEqual([0])
      expect(() => runVec('moving-skewness([1, 2], 2)')).toThrowError(LitsError)
    })
    it('should calculate the centered moving skewness of a vector with padding', () => {
      expect(runVec('centered-moving-skewness([1, 2, 4, 7, 11], 4)')).toEqual([null, null, 0.4987837491108398, 0.3461680709723672, null])
      expect(() => runVec('centered-moving-skewness([1, 2], 2)')).toThrowError(LitsError)
    })
    it('should calculate the running skewness of a vector', () => {
      expect(runVec('running-skewness([1, 2, 4, 7, 11])')).toEqual([null, null, 0.38180177416060584, 0.4987837491108398, 0.5504818825631803])
      expect(runVec('running-skewness([-1, -2, -3])')).toEqual([null, null, 0])
      expect(() => runVec('running-skewness([1, 2])')).toThrowError(LitsError)
    })
  })
  describe('sample-skewness', () => {
    it('should calculate the skewness of a vector', () => {
      expect(runVec('sample-skewness([1, 2, 3, 6])')).toBeCloseTo(1.19034013)
      expect(runVec('sample-skewness([1, 2, 2, 3])')).toEqual(0)
      expect(() => runVec('sample-skewness([1, 1, 1, 1])')).toThrowError(LitsError)
      expect(() => runVec('sample-skewness([0, 1])')).toThrowError(LitsError)
      expect(() => runVec('sample-skewness([])')).toThrowError(LitsError)
    })
    it('should calculate the moving sample skewness of a vector', () => {
      expect(runVec('moving-sample-skewness([1, 2, 3, 4, 5], 3)')).toEqual([0, 0, 0])
      expect(runVec('moving-sample-skewness([1, 2, 3, 4, 5], 5)')).toEqual([0])
      expect(() => runVec('moving-sample-skewness([1, 2], 2)')).toThrowError(LitsError)
    })
    it('should calculate the centered moving sample skewness of a vector with padding', () => {
      expect(runVec('centered-moving-sample-skewness([1, 2, 4, 7, 11, 16], 4)')).toEqual([null, null, 0.8639187954496621, 0.5995806868822491, 0.4561779904708154, null])
      expect(() => runVec('centered-moving-sample-skewness([1, 2], 2)')).toThrowError(LitsError)
    })
    it('should calculate the running sample skewness of a vector', () => {
      expect(runVec('running-sample-skewness([1, 2, 4, 7, 11])')).toEqual([null, null, 0.9352195295828237, 0.8639187954496621, 0.8206099398622181])
      expect(runVec('running-sample-skewness([-1, -2, -3])')).toEqual([null, null, 0])
      expect(() => runVec('running-sample-skewness([1, 2])')).toThrowError(LitsError)
    })
  })
})
