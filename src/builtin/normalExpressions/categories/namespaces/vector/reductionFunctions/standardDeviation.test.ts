import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()

describe('standard deviation functions', () => {
  describe('vec:stdev', () => {
    it('should calculate the standard deviation of a vector', () => {
      expect(lits.run('vec:stdev([1, 2, 3])')).toEqual(0.816496580927726)
      expect(lits.run('vec:stdev([1, 2, 2, 3])')).toEqual(0.7071067811865476)
      expect(lits.run('vec:stdev([0])')).toEqual(0)
      expect(() => lits.run('vec:stdev([])')).toThrowError(LitsError)
    })
    it('should calculate the moving standard deviation of a vector', () => {
      expect(lits.run('vec:moving-stdev([1, 2, 4, 7, 11, 16], 1)')).toEqual([0, 0, 0, 0, 0, 0])
      expect(lits.run('vec:moving-stdev([1, 2, 4, 7, 11, 16], 4)')).toEqual([2.29128784747792, 3.391164991562634, 4.5])
      expect(lits.run('vec:moving-stdev([1, 2, 4, 7, 11, 16], 6)')).toEqual([5.273097339852125])
    })
    it('should calculate the centered moving standard deviation of a vector with padding', () => {
      expect(lits.run('vec:centered-moving-stdev([1, 2, 4, 7], 4)')).toEqual([null, null, 2.29128784747792, null])
    })
    it('should calculate the running standard deviation of a vector', () => {
      expect(lits.run('vec:running-stdev([1, 2, 4, 7])')).toEqual([0, 0.5, 1.247219128924647, 2.29128784747792])
      expect(lits.run('vec:running-stdev([0])')).toEqual([0])
      expect(() => lits.run('vec:running-stdev([])')).toThrowError(LitsError)
    })
  })

  describe('vec:sample-stdev', () => {
    it('should calculate the sample standard deviation of a vector', () => {
      expect(lits.run('vec:sample-stdev([1, 2, 3])')).toEqual(1)
      expect(lits.run('vec:sample-stdev([1, 2, 2, 3])')).toEqual(0.816496580927726)
      expect(() => lits.run('vec:sample-stdev([0])')).toThrowError(LitsError)
      expect(() => lits.run('vec:sample-stdev([])')).toThrowError(LitsError)
    })
    it('should calculate the moving sample standard deviation of a vector', () => {
      expect(lits.run('vec:moving-sample-stdev([1, 2, 4, 7], 2)')).toEqual([0.7071067811865476, 1.4142135623730951, 2.1213203435596424])
      expect(lits.run('vec:moving-sample-stdev([1, 2, 4, 7], 4)')).toEqual([2.6457513110645907])
    })
    it('should calculate the centered moving sample standard deviation of a vector with padding', () => {
      expect(lits.run('vec:centered-moving-sample-stdev([1, 2, 4], 3)')).toEqual([null, 1.5275252316519465, null])
      expect(lits.run('vec:centered-moving-sample-stdev([1, 2, 4], 3, 0, 5)')).toEqual([1, 1.5275252316519465, 1.5275252316519465])
      expect(() => lits.run('vec:centered-moving-sample-stdev([1, 2, 4], 1)')).toThrowError(LitsError)
    })
    it('should calculate the running sample standard deviation of a vector', () => {
      expect(lits.run('vec:running-sample-stdev([1, 2, 3])')).toEqual([null, 0.7071067811865476, 1])
      expect(lits.run('vec:running-sample-stdev([0, 1])')).toEqual([null, 0.7071067811865476])
      expect(() => lits.run('vec:running-sample-stdev([2])')).toThrowError(LitsError)
      expect(() => lits.run('vec:running-sample-stdev([])')).toThrowError(LitsError)
    })
  })
})
