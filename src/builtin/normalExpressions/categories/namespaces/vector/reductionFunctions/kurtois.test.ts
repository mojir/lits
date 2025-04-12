import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()

describe('kurtios functions', () => {
  describe('vec:kurtosis', () => {
    it('should calculate the kurtosis of a vector', () => {
      expect(lits.run('vec:kurtosis([1, 2, 3, 6, 12, 50])')).toBeCloseTo(3.87632753)
      expect(lits.run('vec:kurtosis([1, 2, 2, 3])')).toBeCloseTo(2)
      expect(lits.run('vec:kurtosis([0, 1, 1, 2, 2, 2, 2, 2, 1, 1, 0])')).toBeCloseTo(1.91955017)
      expect(() => lits.run('vec:kurtosis([1, 1, 1, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:kurtosis([0, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:kurtosis([])')).toThrowError(LitsError)
    })
    it('should calculate the moving kurtosis of a vector', () => {
      expect(lits.run('vec:moving-kurtosis([1, 2, 3, 6, 12, 50], 4)')).toEqual([2, 1.9301427627902248, 2.251903931956012])
      expect(() => lits.run('vec:moving-kurtosis([1, 2, 3], 3)')).toThrowError(LitsError)
    })
    it('should calculate the centered moving kurtosis of a vector with padding', () => {
      expect(lits.run('vec:centered-moving-kurtosis([1, 2, 3, 6, 12, 50], 4, 0, 100)')).toEqual([1.628099173553719, 1.6399999999999995, 2, 1.9301427627902248, 2.251903931956012, 1.7465040094373023])
      expect(lits.run('vec:centered-moving-kurtosis([1, 2, 3, 6, 12, 50], 4)')).toEqual([null, null, 2, 1.9301427627902248, 2.251903931956012, null])
      expect(() => lits.run('vec:centered-moving-kurtosis([1, 2, 3], 3)')).toThrowError(LitsError)
    })
    it('should calculate the running kurtosis of a vector', () => {
      expect(lits.run('vec:running-kurtosis([1, 2, 3, 6, 12, 50])')).toEqual([null, null, null, 2, 2.391468473807622, 3.876327528326581])
      expect(() => lits.run('vec:running-kurtosis([1, 2, 3], 3)')).toThrowError(LitsError)
    })
  })
  describe('vec:sample-kurtosis', () => {
    it('should calculate the sample kurtosis of a vector', () => {
      expect(lits.run('vec:sample-kurtosis([1, 2, 3, 6, 12, 50])')).toBeCloseTo(11.3059553)
      expect(lits.run('vec:sample-kurtosis([1, 2, 2, 3])')).toBeCloseTo(15)
      expect(lits.run('vec:sample-kurtosis([0, 1, 1, 2, 2, 2, 2, 2, 1, 1, 0])')).toBeCloseTo(3.19925029)
      expect(() => lits.run('vec:sample-kurtosis([1, 1, 1, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:sample-kurtosis([0, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:sample-kurtosis([])')).toThrowError(LitsError)
    })
    it('should calculate the moving sample kurtosis of a vector', () => {
      expect(lits.run('vec:moving-sample-kurtosis([1, 2, 3, 6, 12, 50], 4)')).toEqual([14.999999999999998, 14.476070720926687, 16.88927948967009])
      expect(() => lits.run('vec:moving-sample-kurtosis([1, 2, 3], 3)')).toThrowError(LitsError)
    })
    it('should calculate the centered moving sample kurtosis of a vector with padding', () => {
      expect(lits.run('vec:centered-moving-sample-kurtosis([1, 2, 3, 6, 12, 50], 4, 0, 100)')).toEqual([12.210743801652894, 12.299999999999999, 14.999999999999998, 14.476070720926687, 16.88927948967009, 13.098780070779771])
      expect(lits.run('vec:centered-moving-sample-kurtosis([1, 2, 3, 6, 12, 50], 4)')).toEqual([null, null, 14.999999999999998, 14.476070720926687, 16.88927948967009, null])
      expect(() => lits.run('vec:centered-moving-sample-kurtosis([1, 2, 3], 3)')).toThrowError(LitsError)
    })
    it('should calculate the running sample kurtosis of a vector', () => {
      expect(lits.run('vec:running-sample-kurtosis([1, 2, 3, 6, 12, 50])')).toEqual([null, null, null, 14.999999999999998, 9.56587389523049, 11.305955290952525])
      expect(() => lits.run('vec:running-sample-kurtosis([1, 2, 3], 3)')).toThrowError(LitsError)
    })
  })
  describe('vec:excess-kurtosis', () => {
    it('should calculate the excess kurtosis of a vector', () => {
      expect(lits.run('vec:excess-kurtosis([1, 2, 3, 6, 12, 50])')).toBeCloseTo(0.87632753)
      expect(lits.run('vec:excess-kurtosis([1, 2, 2, 3])')).toBeCloseTo(-1)
      expect(lits.run('vec:excess-kurtosis([0, 1, 1, 2, 2, 2, 2, 2, 1, 1, 0])')).toBeCloseTo(-1.08044983)
      expect(() => lits.run('vec:excess-kurtosis([1, 1, 1, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:excess-kurtosis([0, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:excess-kurtosis([])')).toThrowError(LitsError)
    })
    it('should calculate the moving excess kurtosis of a vector', () => {
      expect(lits.run('vec:moving-excess-kurtosis([1, 2, 3, 6, 12, 50], 4)')).toEqual([-1, -1.0698572372097752, -0.7480960680439881])
      expect(() => lits.run('vec:moving-excess-kurtosis([1, 2, 3], 3)')).toThrowError(LitsError)
    })
    it('should calculate the centered moving excess kurtosis of a vector with padding', () => {
      expect(lits.run('vec:centered-moving-excess-kurtosis([1, 2, 3, 6, 12, 50], 4, 0, 100)')).toEqual([-1.371900826446281, -1.3600000000000005, -1, -1.0698572372097752, -0.7480960680439881, -1.2534959905626977])
      expect(lits.run('vec:centered-moving-excess-kurtosis([1, 2, 3, 6, 12, 50], 4)')).toEqual([null, null, -1, -1.0698572372097752, -0.7480960680439881, null])
      expect(() => lits.run('vec:centered-moving-excess-kurtosis([1, 2, 3], 3)')).toThrowError(LitsError)
    })
    it('should calculate the running excess kurtosis of a vector', () => {
      expect(lits.run('vec:running-excess-kurtosis([1, 2, 3, 6, 12, 50])')).toEqual([null, null, null, -1, -0.6085315261923778, 0.876327528326581])
      expect(() => lits.run('vec:running-excess-kurtosis([1, 2, 3], 3)')).toThrowError(LitsError)
    })
  })
  describe('vec:sample-excess-kurtosis', () => {
    it('should calculate the sample excess kurtosis of a vector', () => {
      expect(lits.run('vec:sample-excess-kurtosis([1, 2, 3, 6, 12, 50])')).toBeCloseTo(5.05595529)
      expect(lits.run('vec:sample-excess-kurtosis([1, 2, 2, 3])')).toBeCloseTo(1.5)
      expect(lits.run('vec:sample-excess-kurtosis([0, 1, 1, 2, 2, 2, 2, 2, 1, 1, 0])')).toBeCloseTo(-0.967416378)
      expect(() => lits.run('vec:sample-excess-kurtosis([1, 1, 1, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:sample-excess-kurtosis([0, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:sample-excess-kurtosis([])')).toThrowError(LitsError)
    })
    it('should calculate the moving sample excess kurtosis of a vector', () => {
      expect(lits.run('vec:moving-sample-excess-kurtosis([1, 2, 3, 6, 12, 50], 4)')).toEqual([1.4999999999999982, 0.9760707209266872, 3.3892794896700913])
      expect(() => lits.run('vec:moving-sample-excess-kurtosis([1, 2, 3], 3)')).toThrowError(LitsError)
    })
    it('should calculate the centered moving sample excess kurtosis of a vector with padding', () => {
      expect(lits.run('vec:centered-moving-sample-excess-kurtosis([1, 2, 3, 6, 12, 50], 4, 0, 100)')).toEqual([-1.2892561983471058, -1.200000000000001, 1.4999999999999982, 0.9760707209266872, 3.3892794896700913, -0.401219929220229])
      expect(lits.run('vec:centered-moving-sample-excess-kurtosis([1, 2, 3, 6, 12, 50], 4)')).toEqual([null, null, 1.4999999999999982, 0.9760707209266872, 3.3892794896700913, null])
      expect(() => lits.run('vec:centered-moving-sample-excess-kurtosis([1, 2, 3], 3)')).toThrowError(LitsError)
    })
    it('should calculate the running sample excess kurtosis of a vector', () => {
      expect(lits.run('vec:running-sample-excess-kurtosis([1, 2, 3, 6, 12, 50])')).toEqual([null, null, null, 1.4999999999999982, 1.5658738952304905, 5.055955290952525])
      expect(() => lits.run('vec:running-sample-excess-kurtosis([1, 2, 3], 3)')).toThrowError(LitsError)
    })
  })
})
