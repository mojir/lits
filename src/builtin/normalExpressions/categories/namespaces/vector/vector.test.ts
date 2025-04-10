import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'
import { LitsError } from '../../../../../errors'

const lits = new Lits()
describe('vector functions', () => {
  describe('vec:vector?', () => {
    it('should determine if a value is a vector', () => {
      expect(lits.run('vec:vector?([])')).toEqual(true)
      expect(lits.run('vec:vector?([1, 2, 3])')).toEqual(true)
      expect(lits.run('vec:vector?([1, 2, [3]])')).toEqual(false)
      expect(lits.run('vec:vector?([1, 2, 3.0])')).toEqual(true)
      expect(lits.run('vec:vector?([1, 2, "3"])')).toEqual(false)
      expect(lits.run('vec:vector?([1, 2, true])')).toEqual(false)
      expect(lits.run('vec:vector?([1, 2, {}])')).toEqual(false)
      expect(lits.run('vec:vector?(12)')).toEqual(false)
      expect(lits.run('vec:vector?({})')).toEqual(false)
    })
  })
  describe('vec:monotonic?', () => {
    it('should determine if a vector is monotonic', () => {
      expect(lits.run('vec:monotonic?([1, 2, 3])')).toEqual(true)
      expect(lits.run('vec:monotonic?([1, 2, 3, 2])')).toEqual(false)
      expect(lits.run('vec:monotonic?([1, 2, 3, 4])')).toEqual(true)
      expect(lits.run('vec:monotonic?([4, 3, 2, 1])')).toEqual(true)
      expect(lits.run('vec:monotonic?([4, 3, 2, 1, 1, 1])')).toEqual(true)
      expect(lits.run('vec:monotonic?([])')).toEqual(true)
    })
  })
  describe('vec:strictly-monotonic?', () => {
    it('should determine if a vector is monotonic', () => {
      expect(lits.run('vec:strictly-monotonic?([1, 2, 3])')).toEqual(true)
      expect(lits.run('vec:strictly-monotonic?([1, 2, 3, 2])')).toEqual(false)
      expect(lits.run('vec:strictly-monotonic?([1, 2, 3, 4])')).toEqual(true)
      expect(lits.run('vec:strictly-monotonic?([4, 3, 2, 1])')).toEqual(true)
      expect(lits.run('vec:strictly-monotonic?([4, 3, 2, 1, 1, 1])')).toEqual(false)
      expect(lits.run('vec:strictly-monotonic?([])')).toEqual(true)
    })
  })
  describe('vec:increasing?', () => {
    it('should determine if a vector is monotonic increasing', () => {
      expect(lits.run('vec:increasing?([1, 2, 3])')).toEqual(true)
      expect(lits.run('vec:increasing?([1, 2, 2, 3, 2])')).toEqual(false)
      expect(lits.run('vec:increasing?([1, 2, 2, 3, 4])')).toEqual(true)
      expect(lits.run('vec:increasing?([4, 3, 2, 1])')).toEqual(false)
      expect(lits.run('vec:increasing?([])')).toEqual(true)
    })
  })
  describe('vec:decreasing?', () => {
    it('should determine if a vector is monotonic decreasing', () => {
      expect(lits.run('vec:decreasing?([1, 2, 2, 3])')).toEqual(false)
      expect(lits.run('vec:decreasing?([1, 2, 3, 2])')).toEqual(false)
      expect(lits.run('vec:decreasing?([1, 2, 3, 4])')).toEqual(false)
      expect(lits.run('vec:decreasing?([4, 3, 2, 2, 1])')).toEqual(true)
      expect(lits.run('vec:decreasing?([4, 4, 3, 2, 2, 1])')).toEqual(true)
      expect(lits.run('vec:decreasing?([])')).toEqual(true)
    })
  })
  describe('vec:strictly-increasing?', () => {
    it('should determine if a vector is strictly monotonic increasing', () => {
      expect(lits.run('vec:strictly-increasing?([1, 2, 3])')).toEqual(true)
      expect(lits.run('vec:strictly-increasing?([1, 2, 3, 2])')).toEqual(false)
      expect(lits.run('vec:strictly-increasing?([1, 2, 3, 4])')).toEqual(true)
      expect(lits.run('vec:strictly-increasing?([1, 1, 2, 3, 4])')).toEqual(false)
      expect(lits.run('vec:strictly-increasing?([4, 3, 2, 1])')).toEqual(false)
      expect(lits.run('vec:strictly-increasing?([])')).toEqual(true)
    })
  })
  describe('vec:strictly-decreasing?', () => {
    it('should determine if a vector is strictly monotonic decreasing', () => {
      expect(lits.run('vec:strictly-decreasing?([1, 2, 3])')).toEqual(false)
      expect(lits.run('vec:strictly-decreasing?([1, 2, 3, 2])')).toEqual(false)
      expect(lits.run('vec:strictly-decreasing?([1, 2, 3, 4])')).toEqual(false)
      expect(lits.run('vec:strictly-decreasing?([4, 3, 2, 1])')).toEqual(true)
      expect(lits.run('vec:strictly-decreasing?([4, 3, 3, 2, 1])')).toEqual(false)
      expect(lits.run('vec:strictly-decreasing?([])')).toEqual(true)
    })
  })
  describe('vec:sum', () => {
    it('should sum the elements of a vector', () => {
      expect(lits.run('vec:sum([1, 2, 3])')).toEqual(6)
      expect(lits.run('vec:sum([1, -2, 3])')).toEqual(2)
      expect(lits.run('vec:sum([-1, -2, -3])')).toEqual(-6)
      expect(lits.run('vec:sum([0])')).toEqual(0)
      expect(lits.run('vec:sum([])')).toEqual(0)
    })
  })
  describe('vec:prod', () => {
    it('should multiply the elements of a vector', () => {
      expect(lits.run('vec:prod([1, 2, 3])')).toEqual(6)
      expect(lits.run('vec:prod([1, -2, 3])')).toEqual(-6)
      expect(lits.run('vec:prod([-1, -2, -3])')).toEqual(-6)
      expect(lits.run('vec:prod([0])')).toEqual(0)
      expect(lits.run('vec:prod([])')).toEqual(1)
    })
  })
  describe('vec:mean', () => {
    it('should calculate the mean of a vector', () => {
      expect(lits.run('vec:mean([1, 2, 3])')).toEqual(2)
      expect(lits.run('vec:mean([1, -3, 2])')).toEqual(0)
      expect(lits.run('vec:mean([-1, -2, -3])')).toEqual(-2)
      expect(lits.run('vec:mean([0])')).toEqual(0)
      expect(() => lits.run('vec:mean([])')).toThrowError(LitsError)
    })
  })
  describe('vec:median', () => {
    it('should calculate the median of a vector', () => {
      expect(lits.run('vec:median([1, 2, 3])')).toEqual(2)
      expect(lits.run('vec:median([1, -2, 3])')).toEqual(1)
      expect(lits.run('vec:median([-1, -2, -3])')).toEqual(-2)
      expect(lits.run('vec:median([0])')).toEqual(0)
      expect(() => lits.run('vec:median([])')).toThrowError(LitsError)
    })
  })
  describe('vec:mode', () => {
    it('should calculate the mode of a vector', () => {
      expect(lits.run('vec:mode([1, 2, 3])')).toEqual([1, 2, 3])
      expect(lits.run('vec:mode([1, 2, 2, 3])')).toEqual([2])
      expect(lits.run('vec:mode([0])')).toEqual([0])
      expect(() => lits.run('vec:mode([])')).toThrowError(LitsError)
    })
  })
  describe('vec:variance', () => {
    it('should calculate the variance of a vector', () => {
      expect(lits.run('vec:variance([1, 2, 3])')).toEqual(0.6666666666666666)
      expect(lits.run('vec:variance([1, 2, 2, 3])')).toEqual(0.5)
      expect(lits.run('vec:variance([0])')).toEqual(0)
      expect(() => lits.run('vec:variance([])')).toThrowError(LitsError)
    })
  })
  describe('vec:sample-variance', () => {
    it('should calculate the sample variance of a vector', () => {
      expect(lits.run('vec:sample-variance([1, 2, 3])')).toEqual(1)
      expect(lits.run('vec:sample-variance([1, 2, 2, 3])')).toEqual(0.6666666666666666)
      expect(() => lits.run('vec:sample-variance([0])')).toThrowError(LitsError)
      expect(() => lits.run('vec:sample-variance([])')).toThrowError(LitsError)
    })
  })
  describe('vec:stdev', () => {
    it('should calculate the standard deviation of a vector', () => {
      expect(lits.run('vec:stdev([1, 2, 3])')).toEqual(0.816496580927726)
      expect(lits.run('vec:stdev([1, 2, 2, 3])')).toEqual(0.7071067811865476)
      expect(lits.run('vec:stdev([0])')).toEqual(0)
      expect(() => lits.run('vec:stdev([])')).toThrowError(LitsError)
    })
  })
  describe('vec:sample-stdev', () => {
    it('should calculate the sample standard deviation of a vector', () => {
      expect(lits.run('vec:sample-stdev([1, 2, 3])')).toEqual(1)
      expect(lits.run('vec:sample-stdev([1, 2, 2, 3])')).toEqual(0.816496580927726)
      expect(() => lits.run('vec:sample-stdev([0])')).toThrowError(LitsError)
      expect(() => lits.run('vec:sample-stdev([])')).toThrowError(LitsError)
    })
  })
  describe('vec:min', () => {
    it('should find the minimum value in a vector', () => {
      expect(lits.run('vec:min([1, 2, 3])')).toEqual(1)
      expect(lits.run('vec:min([3, 2, 1])')).toEqual(1)
      expect(lits.run('vec:min([0])')).toEqual(0)
      expect(() => lits.run('vec:min([])')).toThrowError(LitsError)
    })
  })
  describe('vec:max', () => {
    it('should find the maximum value in a vector', () => {
      expect(lits.run('vec:max([1, 2, 3])')).toEqual(3)
      expect(lits.run('vec:max([3, 2, 1])')).toEqual(3)
      expect(lits.run('vec:max([0])')).toEqual(0)
      expect(() => lits.run('vec:max([])')).toThrowError(LitsError)
    })
  })
  describe('vec:min-index', () => {
    it('should find the index of the minimum value in a vector', () => {
      expect(lits.run('vec:min-index([1, 2, 3])')).toEqual(0)
      expect(lits.run('vec:min-index([3, 2, 1])')).toEqual(2)
      expect(lits.run('vec:min-index([0])')).toEqual(0)
      expect(() => lits.run('vec:min-index([])')).toThrowError(LitsError)
    })
  })
  describe('vec:max-index', () => {
    it('should find the index of the maximum value in a vector', () => {
      expect(lits.run('vec:max-index([1, 2, 3])')).toEqual(2)
      expect(lits.run('vec:max-index([3, 2, 1])')).toEqual(0)
      expect(lits.run('vec:max-index([0])')).toEqual(0)
      expect(() => lits.run('vec:max-index([])')).toThrowError(LitsError)
    })
  })
  describe('vec:sort-indices', () => {
    it('should sort the indices of a vector', () => {
      expect(lits.run('vec:sort-indices([1, 2, 3])')).toEqual([0, 1, 2])
      expect(lits.run('vec:sort-indices([3, 2, 1])')).toEqual([2, 1, 0])
      expect(lits.run('vec:sort-indices([0])')).toEqual([0])
      expect(lits.run('vec:sort-indices([])')).toEqual([])
    })
  })
  describe('vec:count-values', () => {
    it('should count the occurrences of each value in a vector', () => {
      expect(lits.run('vec:count-values([1, 2, 3])')).toEqual([[1, 1], [2, 1], [3, 1]])
      expect(lits.run('vec:count-values([1, 2, 2, 3])')).toEqual([[2, 2], [1, 1], [3, 1]])
      expect(lits.run('vec:count-values([0])')).toEqual([[0, 1]])
      expect(lits.run('vec:count-values([])')).toEqual([])
      expect(lits.run('vec:count-values([1, 2, 3, 1])')).toEqual([[1, 2], [2, 1], [3, 1]])
    })
  })
  describe('vec:linspace', () => {
    it('should create a linearly spaced vector', () => {
      expect(lits.run('vec:linspace(1, 10, 5)')).toEqual([1, 3.25, 5.5, 7.75, 10])
      expect(lits.run('vec:linspace(1, 10, 0)')).toEqual([])
      expect(() => lits.run('vec:linspace(1, 10, -1)')).toThrowError(LitsError)
      expect(lits.run('vec:linspace(1, 10, 1)')).toEqual([1])
      expect(() => lits.run('vec:linspace(1, 10)')).toThrowError(LitsError)
    })
  })
  describe('vec:ones', () => {
    it('should create a vector of ones', () => {
      expect(lits.run('vec:ones(5)')).toEqual([1, 1, 1, 1, 1])
      expect(lits.run('vec:ones(0)')).toEqual([])
      expect(() => lits.run('vec:ones(-1)')).toThrowError(LitsError)
      expect(() => lits.run('vec:ones()')).toThrowError(LitsError)
    })
  })
  describe('vec:zeros', () => {
    it('should create a vector of zeros', () => {
      expect(lits.run('vec:zeros(5)')).toEqual([0, 0, 0, 0, 0])
      expect(lits.run('vec:zeros(0)')).toEqual([])
      expect(() => lits.run('vec:zeros(-1)')).toThrowError(LitsError)
      expect(() => lits.run('vec:zeros()')).toThrowError(LitsError)
    })
  })
  describe('vec:fill', () => {
    it('should create a vector filled with a value', () => {
      expect(lits.run('vec:fill(5, 3)')).toEqual([3, 3, 3, 3, 3])
      expect(lits.run('vec:fill(0, 0)')).toEqual([])
      expect(() => lits.run('vec:fill(-1, 5)')).toThrowError(LitsError)
      expect(() => lits.run('vec:fill()')).toThrowError(LitsError)
    })
  })
  describe('vec:generate', () => {
    it('should create a vector generated by a function', () => {
      expect(lits.run('vec:generate(5, -> $ * 2)')).toEqual([0, 2, 4, 6, 8])
      expect(lits.run('vec:generate(0, -> $ * 2)')).toEqual([])
      expect(() => lits.run('vec:generate(-1, -> $ * 2)')).toThrowError(LitsError)
      expect(() => lits.run('vec:generate(5)')).toThrowError(LitsError)
    })
  })
  describe('vec:cumsum', () => {
    it('should calculate the cumulative sum of a vector', () => {
      expect(lits.run('vec:cumsum([1, 2, 3])')).toEqual([1, 3, 6])
      expect(lits.run('vec:cumsum([1, -2, 3])')).toEqual([1, -1, 2])
      expect(lits.run('vec:cumsum([-1, -2, -3])')).toEqual([-1, -3, -6])
      expect(lits.run('vec:cumsum([0])')).toEqual([0])
      expect(lits.run('vec:cumsum([])')).toEqual([])
    })
  })
  describe('vec:cumprod', () => {
    it('should calculate the cumulative product of a vector', () => {
      expect(lits.run('vec:cumprod([1, 2, 3])')).toEqual([1, 2, 6])
      expect(lits.run('vec:cumprod([1, -2, 3])')).toEqual([1, -2, -6])
      expect(lits.run('vec:cumprod([1, 0, 1])')).toEqual([1, 0, 0])
      expect(lits.run('vec:cumprod([-1, -2, -3])')).toEqual([-1, 2, -6])
      expect(lits.run('vec:cumprod([0])')).toEqual([0])
      expect(lits.run('vec:cumprod([])')).toEqual([])
    })
  })
  describe('vec:quartiles', () => {
    it('should calculate the quartiles of a vector', () => {
      expect(lits.run('vec:quartiles([1, 2, 3, 4])')).toEqual([1.5, 2.5, 3.5])
      expect(lits.run('vec:quartiles([1, 2, 3, 4, 5])')).toEqual([1.5, 3, 4.5])
      expect(() => lits.run('vec:quartiles([1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:quartiles([1, 2, 3])')).toThrowError(LitsError)
      expect(() => lits.run('vec:quartiles([])')).toThrowError(LitsError)
    })
  })
  describe('vec:iqr', () => {
    it('should calculate the interquartile range of a vector', () => {
      expect(lits.run('vec:iqr([1, 2, 3, 4])')).toEqual(2)
      expect(lits.run('vec:iqr([1, 2, 3, 4, 5])')).toEqual(3)
      expect(() => lits.run('vec:iqr([1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:iqr([1, 2, 3])')).toThrowError(LitsError)
      expect(() => lits.run('vec:iqr([])')).toThrowError(LitsError)
    })
  })
  describe('vec:percentile', () => {
    it('should calculate the percentile of a vector', () => {
      expect(lits.run('vec:percentile([10, 20, 30, 40, 50, 60, 70, 80, 90, 100], 0)')).toEqual(10)
      expect(lits.run('vec:percentile([10, 20, 30, 40, 50, 60, 70, 80, 90, 100], 25)')).toEqual(32.5)
      expect(lits.run('vec:percentile([10, 20, 30, 40, 50, 60, 70, 80, 90, 100], 50)')).toEqual(55)
      expect(lits.run('vec:percentile([10, 20, 30, 40, 50, 60, 70, 80, 90, 100], 75)')).toEqual(77.5)
      expect(lits.run('vec:percentile([10, 20, 30, 40, 50, 60, 70, 80, 90, 100], 100)')).toEqual(100)

      expect(lits.run('vec:percentile([1], 0.5)')).toBe(1)
      expect(lits.run('vec:percentile([1], 50)')).toBe(1)
      expect(lits.run('vec:percentile([1], 0)')).toBe(1)
      expect(lits.run('vec:percentile([1], 100)')).toBe(1)

      expect(lits.run('vec:percentile([42, 15, 87, 23, 56, 91, 34, 68, 72, 29], 0)')).toEqual(15)
      expect(lits.run('vec:percentile([42, 15, 87, 23, 56, 91, 34, 68, 72, 29], 25)')).toEqual(30.25)
      expect(lits.run('vec:percentile([42, 15, 87, 23, 56, 91, 34, 68, 72, 29], 50)')).toEqual(49)
      expect(lits.run('vec:percentile([42, 15, 87, 23, 56, 91, 34, 68, 72, 29], 75)')).toEqual(71)
      expect(lits.run('vec:percentile([42, 15, 87, 23, 56, 91, 34, 68, 72, 29], 100)')).toEqual(91)

      expect(lits.run('vec:percentile([5, 10, 15, 20, 25], 0)')).toEqual(5)
      expect(lits.run('vec:percentile([5, 10, 15, 20, 25], 25)')).toEqual(10)
      expect(lits.run('vec:percentile([5, 10, 15, 20, 25], 50)')).toEqual(15)
      expect(lits.run('vec:percentile([5, 10, 15, 20, 25], 75)')).toEqual(20)
      expect(lits.run('vec:percentile([5, 10, 15, 20, 25], 100)')).toEqual(25)
    })
  })
  describe('vec:quantile', () => {
    it('should calculate the quantile of a vector', () => {
      expect(lits.run('vec:quantile([10, 20, 30, 40, 50, 60, 70, 80, 90, 100], 0)')).toEqual(10)
      expect(lits.run('vec:quantile([10, 20, 30, 40, 50, 60, 70, 80, 90, 100], 0.25)')).toEqual(32.5)
      expect(lits.run('vec:quantile([10, 20, 30, 40, 50, 60, 70, 80, 90, 100], 0.50)')).toEqual(55)
      expect(lits.run('vec:quantile([10, 20, 30, 40, 50, 60, 70, 80, 90, 100], 0.75)')).toEqual(77.5)
      expect(lits.run('vec:quantile([10, 20, 30, 40, 50, 60, 70, 80, 90, 100], 1)')).toEqual(100)

      expect(lits.run('vec:quantile([1], 0.5)')).toBe(1)
      expect(lits.run('vec:quantile([1], 0)')).toBe(1)
      expect(lits.run('vec:quantile([1], 1)')).toBe(1)

      expect(lits.run('vec:quantile([42, 15, 87, 23, 56, 91, 34, 68, 72, 29], 0)')).toEqual(15)
      expect(lits.run('vec:quantile([42, 15, 87, 23, 56, 91, 34, 68, 72, 29], 0.25)')).toEqual(30.25)
      expect(lits.run('vec:quantile([42, 15, 87, 23, 56, 91, 34, 68, 72, 29], 0.50)')).toEqual(49)
      expect(lits.run('vec:quantile([42, 15, 87, 23, 56, 91, 34, 68, 72, 29], 0.75)')).toEqual(71)
      expect(lits.run('vec:quantile([42, 15, 87, 23, 56, 91, 34, 68, 72, 29], 1)')).toEqual(91)

      expect(lits.run('vec:quantile([5, 10, 15, 20, 25], 0)')).toEqual(5)
      expect(lits.run('vec:quantile([5, 10, 15, 20, 25], 0.25)')).toEqual(10)
      expect(lits.run('vec:quantile([5, 10, 15, 20, 25], 0.50)')).toEqual(15)
      expect(lits.run('vec:quantile([5, 10, 15, 20, 25], 0.75)')).toEqual(20)
      expect(lits.run('vec:quantile([5, 10, 15, 20, 25], 1)')).toEqual(25)
    })
  })
  describe('vec:span', () => {
    it('should get the range of a vector', () => {
      expect(lits.run('vec:span([1, 2, 3])')).toEqual(2)
      expect(lits.run('vec:span([3, 2, 1])')).toEqual(2)
      expect(lits.run('vec:span([0])')).toEqual(0)
      expect(() => lits.run('vec:span([])')).toThrowError(LitsError)
    })
  })
  describe('vec:skewness', () => {
    it('should calculate the skewness of a vector', () => {
      expect(lits.run('vec:skewness([1, 2, 3, 6])')).toBeCloseTo(0.687243193)
      expect(lits.run('vec:skewness([1, 2, 2, 3])')).toEqual(0)
      expect(() => lits.run('vec:skewness([1, 1, 1, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:skewness([0, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:skewness([])')).toThrowError(LitsError)
    })
  })
  describe('vec:sample-skewness', () => {
    it('should calculate the skewness of a vector', () => {
      expect(lits.run('vec:sample-skewness([1, 2, 3, 6])')).toBeCloseTo(1.19034013)
      expect(lits.run('vec:sample-skewness([1, 2, 2, 3])')).toEqual(0)
      expect(() => lits.run('vec:sample-skewness([0, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:sample-skewness([])')).toThrowError(LitsError)
    })
  })
  describe('vec:kurtosis', () => {
    it('should calculate the kurtosis of a vector', () => {
      expect(lits.run('vec:kurtosis([1, 2, 3, 6, 12, 50])')).toBeCloseTo(3.87632753)
      expect(lits.run('vec:kurtosis([1, 2, 2, 3])')).toBeCloseTo(2)
      expect(lits.run('vec:kurtosis([0, 1, 1, 2, 2, 2, 2, 2, 1, 1, 0])')).toBeCloseTo(1.91955017)
      expect(() => lits.run('vec:kurtosis([1, 1, 1, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:kurtosis([0, 1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:kurtosis([])')).toThrowError(LitsError)
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
  })
  describe('vec:geometric-mean', () => {
    it('should calculate the geometric mean of a vector', () => {
      expect(lits.run('vec:geometric-mean([2, 4, 8, 16])')).toBeCloseTo(5.656854)
      expect(lits.run('vec:geometric-mean([1, 2, 2, 3])')).toBeCloseTo(1.8612097182041991)
      expect(() => lits.run('vec:geometric-mean([])')).toThrowError(LitsError)
    })
  })
  describe('vec:harmonic-mean', () => {
    it('should calculate the harmonic mean of a vector', () => {
      expect(lits.run('vec:harmonic-mean([2, 4, 8, 16])')).toBeCloseTo(4.266666666667)
      expect(lits.run('vec:harmonic-mean([1, 2, 2, 3])')).toBeCloseTo(1.7142857142857142)
      expect(() => lits.run('vec:harmonic-mean([])')).toThrowError(LitsError)
    })
  })
  describe('vec:rms', () => {
    it('should calculate the root mean square of a vector', () => {
      expect(lits.run('vec:rms([2, 4, 8, 16])')).toBeCloseTo(9.21954446)
      expect(lits.run('vec:rms([1, 2, 2, 3])')).toBeCloseTo(2.12132034)
      expect(() => lits.run('vec:rms([])')).toThrowError(LitsError)
    })
  })
  describe('vec:mad', () => {
    it('should calculate the mean absolute deviation of a vector', () => {
      expect(lits.run('vec:mad([1, 2, 3])')).toEqual(0.6666666666666666)
      expect(lits.run('vec:mad([1, 2, 2, 3])')).toEqual(0.5)
      expect(() => lits.run('vec:mad([])')).toThrowError(LitsError)
    })
  })

  describe('vec:medad', () => {
    it('should calculate the median absolute deviation of a vector', () => {
      expect(lits.run('vec:medad([1, 2, 3])')).toEqual(1.4826)
      expect(lits.run('vec:medad([1, 2, 2, 3, 5, 15, 50])')).toEqual(2.9652)
      expect(() => lits.run('vec:medad([])')).toThrowError(LitsError)
    })
  })

  describe('vec:histogram', () => {
    it('should calculate the histogram of a vector', () => {
      expect(lits.run('vec:histogram([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9], 3)')).toEqual([
        [
          1,
          3.6666666666666665,
          10,
        ],
        [
          3.6666666666666665,
          6.333333333333333,
          3,
        ],
        [
          6.333333333333333,
          9,
          1,
        ],
      ])
      expect(lits.run('vec:histogram([1, 2, 3], 5)')).toEqual([
        [
          1,
          1.4,
          1,
        ],
        [
          1.4,
          1.8,
          0,
        ],
        [
          1.8,
          2.2,
          1,
        ],
        [
          2.2,
          2.6,
          0,
        ],
        [
          2.6,
          3,
          1,
        ],
      ])
      expect(lits.run('vec:histogram([0], 5)')).toEqual([
        [
          0,
          0,
          1,
        ],
        [
          0,
          0,
          0,
        ],
        [
          0,
          0,
          0,
        ],
        [
          0,
          0,
          0,
        ],
        [
          0,
          0,
          0,
        ],
      ])
      expect(lits.run('vec:histogram([], 5)')).toEqual([
        [
          0,
          0,
          0,
        ],
        [
          0,
          0,
          0,
        ],
        [
          0,
          0,
          0,
        ],
        [
          0,
          0,
          0,
        ],
        [
          0,
          0,
          0,
        ],
      ])
    })
  })
  describe('vec:ecdf', () => {
    it('should calculate the cumulative distribution function of a vector', () => {
      expect(lits.run('vec:ecdf([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9, 10], 5)')).toBe(0.8)
      expect(lits.run('vec:ecdf([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9, 10, 12], 5)')).toEqual(0.75)
      expect(lits.run('vec:ecdf([1], 3)')).toEqual(1)
      expect(lits.run('vec:ecdf([1], 0)')).toEqual(0)
      expect(() => lits.run('vec:ecdf([], 1)')).toThrow()
    })
  })
  describe('vec:outliers?', () => {
    it('should check if a vector has no extreme outliers', () => {
      expect(lits.run('vec:outliers?([1, 2, 3])')).toEqual(false)
      expect(lits.run('vec:outliers?([1, 2, 3, 4, 2, 1, 100])')).toEqual(true)
      expect(lits.run('vec:outliers?([1, 2, 0, 2, -100])')).toEqual(true)
      expect(lits.run('vec:outliers?([1])')).toEqual(false)
      expect(lits.run('vec:outliers?([])')).toBe(false)
    })
  })
  describe('vec:outliers', () => {
    it('should find the extreme outliers in a vector', () => {
      expect(lits.run('vec:outliers([1, 2, 3])')).toEqual([])
      expect(lits.run('vec:outliers([1, 2, 3, 4, 2, 1, 100])')).toEqual([100])
      expect(lits.run('vec:outliers([1, 2, 0, 2, -100])')).toEqual([-100])
      expect(lits.run('vec:outliers([1])')).toEqual([])
      expect(lits.run('vec:outliers([])')).toEqual([])
    })
  })
})
