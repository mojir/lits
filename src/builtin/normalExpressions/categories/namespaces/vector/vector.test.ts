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
  describe('vec:sorted?', () => {
    it('should determine if a vector is sorted', () => {
      expect(lits.run('vec:sorted?([1, 2, 3])')).toEqual(true)
      expect(lits.run('vec:sorted?([1, 2, 3, 2])')).toEqual(false)
      expect(lits.run('vec:sorted?([1, 2, 3, 4])')).toEqual(true)
      expect(lits.run('vec:sorted?([4, 3, 2, 1])')).toEqual(false)
      expect(lits.run('vec:sorted?([])')).toEqual(true)
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
  describe('vec:+', () => {
    it('should add vectors element-wise', () => {
      expect(lits.run('vec:+([1, 2, 3])')).toEqual([1, 2, 3])
      expect(lits.run('[1, 2, 3] vec:+ [4, 5, 6]')).toEqual([5, 7, 9])
      expect(lits.run('vec:+([1, 2, 3], [4, 5, 6])')).toEqual([5, 7, 9])
      expect(lits.run('vec:+([1, 2, 3], [4, 5, 6], [7, 8, 9])')).toEqual([12, 15, 18])
      expect(lits.run('vec:+([1], [2])')).toEqual([3])
      expect(lits.run('vec:+([], [])')).toEqual([])
      expect(() => lits.run('vec:+([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('vec:+([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:+([1], [])')).toThrowError(LitsError)
    })
    it('should add vectors and scalars', () => {
      expect(lits.run('vec:+([1, 2, 3], 4)')).toEqual([5, 6, 7])
      expect(lits.run('vec:+([1], 4)')).toEqual([5])
      expect(lits.run('vec:+([], 4)')).toEqual([])
      expect(lits.run('vec:+(4, [1, 2, 3])')).toEqual([5, 6, 7])
      expect(lits.run('vec:+(4, [1])')).toEqual([5])
      expect(lits.run('vec:+(4, [])')).toEqual([])
      expect(lits.run('vec:+(4, [1, 2, 3], 5)')).toEqual([10, 11, 12])
      expect(() => lits.run('vec:+([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('vec:+([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:+([1], [])')).toThrowError(LitsError)
      expect(() => lits.run('vec:+(1, 0)')).toThrowError(LitsError)
    })
  })
  describe('vec:-', () => {
    it('should subtract vectors', () => {
      expect(lits.run('vec:-([1, 2, 3], [4, 5, 6])')).toEqual([-3, -3, -3])
      expect(lits.run('vec:-([1, 2, 3], [4, 5, 6], -3)')).toEqual([0, 0, 0])
      expect(lits.run('vec:-(10, [1, 2, 3])')).toEqual([9, 8, 7])
      expect(lits.run('vec:-([1], [2])')).toEqual([-1])
      expect(lits.run('vec:-([], [])')).toEqual([])
      expect(() => lits.run('vec:-([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('vec:-([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('vec:-([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:-([1], [])')).toThrowError(LitsError)
      expect(() => lits.run('vec:-(1, 0)')).toThrowError(LitsError)
    })
  })
  describe('vec:*', () => {
    it('should multiply two vectors element-wise', () => {
      expect(lits.run('vec:*([1, 2, 3], [4, 5, 6])')).toEqual([4, 10, 18])
      expect(lits.run('vec:*(2, [1, 2, 3])')).toEqual([2, 4, 6])
      expect(lits.run('vec:*(2, [1, 2, 3], 2)')).toEqual([4, 8, 12])
      expect(lits.run('vec:*(2, [1, 2, 3], [2, 2, 2])')).toEqual([4, 8, 12])
      expect(lits.run('vec:*([1], [2])')).toEqual([2])
      expect(lits.run('vec:*([], [])')).toEqual([])
      expect(() => lits.run('vec:*([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('vec:*([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:*([1], [])')).toThrowError(LitsError)
      expect(() => lits.run('vec:*(1, 0)')).toThrowError(LitsError)
    })
  })
  describe('vec:/', () => {
    it('should divide two vectors element-wise', () => {
      expect(lits.run('vec:/([1, 2, 3], [4, 5, 6])')).toEqual([0.25, 0.4, 0.5])
      expect(lits.run('vec:/([1, 2, 3], 2)')).toEqual([0.5, 1, 1.5])
      expect(lits.run('vec:/(12, [1, 2, 3], 2)')).toEqual([6, 3, 2])
      expect(lits.run('vec:/([1], [2])')).toEqual([0.5])
      expect(lits.run('vec:/([], [])')).toEqual([])
      expect(() => lits.run('vec:/([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('vec:/([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:/([1], [])')).toThrowError(LitsError)
    })
  })
  describe('vec:^', () => {
    it('should exponentiate two vectors element-wise', () => {
      expect(lits.run('vec:^([1, 2, 3], [4, 5, 6])')).toEqual([1, 32, 729])
      expect(lits.run('vec:^([1], [2])')).toEqual([1])
      expect(lits.run('vec:^([], [])')).toEqual([])
      expect(() => lits.run('vec:^([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('vec:^([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:^([1], [])')).toThrowError(LitsError)
    })
    it('should exponentiate a vector by a scalar', () => {
      expect(lits.run('vec:^([1, 2, 3], 2)')).toEqual([1, 4, 9])
      expect(lits.run('vec:^([1], 2)')).toEqual([1])
      expect(lits.run('vec:^([], 2)')).toEqual([])
      expect(() => lits.run('vec:^([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('vec:^([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('vec:^([1], [])')).toThrowError(LitsError)
    })
    it('should exponentiate a scalar by a vector', () => {
      expect(lits.run('vec:^(2, [1, 2, 3])')).toEqual([2, 4, 8])
      expect(lits.run('vec:^(2, [1])')).toEqual([2])
      expect(lits.run('vec:^(2, [])')).toEqual([])
      expect(() => lits.run('vec:^([2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('vec:^(2, 2)')).toThrowError(LitsError)
    })
  })
  describe('vec:abs', () => {
    it('should take the absolute value of a vector', () => {
      expect(lits.run('vec:abs([1, -2, 3])')).toEqual([1, 2, 3])
      expect(lits.run('vec:abs([-1, -2, -3])')).toEqual([1, 2, 3])
      expect(lits.run('vec:abs([0])')).toEqual([0])
      expect(lits.run('vec:abs([])')).toEqual([])
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
  describe('vec:get-range', () => {
    it('should get the range of a vector', () => {
      expect(lits.run('vec:get-range([1, 2, 3])')).toEqual(2)
      expect(lits.run('vec:get-range([3, 2, 1])')).toEqual(2)
      expect(lits.run('vec:get-range([0])')).toEqual(0)
      expect(() => lits.run('vec:get-range([])')).toThrowError(LitsError)
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
})
