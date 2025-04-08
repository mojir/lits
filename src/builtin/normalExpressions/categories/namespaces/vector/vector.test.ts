import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

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
      expect(() => lits.run('vec:+([1, 2], [3, 4, 5])')).toThrow()
      expect(() => lits.run('vec:+([], [1])')).toThrow()
      expect(() => lits.run('vec:+([1], [])')).toThrow()
    })
    it('should add vectors and scalars', () => {
      expect(lits.run('vec:+([1, 2, 3], 4)')).toEqual([5, 6, 7])
      expect(lits.run('vec:+([1], 4)')).toEqual([5])
      expect(lits.run('vec:+([], 4)')).toEqual([])
      expect(lits.run('vec:+(4, [1, 2, 3])')).toEqual([5, 6, 7])
      expect(lits.run('vec:+(4, [1])')).toEqual([5])
      expect(lits.run('vec:+(4, [])')).toEqual([])
      expect(lits.run('vec:+(4, [1, 2, 3], 5)')).toEqual([10, 11, 12])
      expect(() => lits.run('vec:+([1, 2], [3, 4, 5])')).toThrow()
      expect(() => lits.run('vec:+([], [1])')).toThrow()
      expect(() => lits.run('vec:+([1], [])')).toThrow()
      expect(() => lits.run('vec:+(1, 0)')).toThrow()
    })
  })
  describe('vec:-', () => {
    it('should subtract vectors', () => {
      expect(lits.run('vec:-([1, 2, 3], [4, 5, 6])')).toEqual([-3, -3, -3])
      expect(lits.run('vec:-([1, 2, 3], [4, 5, 6], -3)')).toEqual([0, 0, 0])
      expect(lits.run('vec:-(10, [1, 2, 3])')).toEqual([9, 8, 7])
      expect(lits.run('vec:-([1], [2])')).toEqual([-1])
      expect(lits.run('vec:-([], [])')).toEqual([])
      expect(() => lits.run('vec:-([1, 2], [3, 4, 5])')).toThrow()
      expect(() => lits.run('vec:-([1, 2], [3, 4, 5])')).toThrow()
      expect(() => lits.run('vec:-([], [1])')).toThrow()
      expect(() => lits.run('vec:-([1], [])')).toThrow()
      expect(() => lits.run('vec:-(1, 0)')).toThrow()
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
      expect(() => lits.run('vec:*([1, 2], [3, 4, 5])')).toThrow()
      expect(() => lits.run('vec:*([], [1])')).toThrow()
      expect(() => lits.run('vec:*([1], [])')).toThrow()
      expect(() => lits.run('vec:*(1, 0)')).toThrow()
    })
  })
  describe('vec:/', () => {
    it('should divide two vectors element-wise', () => {
      expect(lits.run('vec:/([1, 2, 3], [4, 5, 6])')).toEqual([0.25, 0.4, 0.5])
      expect(lits.run('vec:/([1, 2, 3], 2)')).toEqual([0.5, 1, 1.5])
      expect(lits.run('vec:/(12, [1, 2, 3], 2)')).toEqual([6, 3, 2])
      expect(lits.run('vec:/([1], [2])')).toEqual([0.5])
      expect(lits.run('vec:/([], [])')).toEqual([])
      expect(() => lits.run('vec:/([1, 2], [3, 4, 5])')).toThrow()
      expect(() => lits.run('vec:/([], [1])')).toThrow()
      expect(() => lits.run('vec:/([1], [])')).toThrow()
    })
  })
  describe('vec:^', () => {
    it('should exponentiate two vectors element-wise', () => {
      expect(lits.run('vec:^([1, 2, 3], [4, 5, 6])')).toEqual([1, 32, 729])
      expect(lits.run('vec:^([1], [2])')).toEqual([1])
      expect(lits.run('vec:^([], [])')).toEqual([])
      expect(() => lits.run('vec:^([1, 2], [3, 4, 5])')).toThrow()
      expect(() => lits.run('vec:^([], [1])')).toThrow()
      expect(() => lits.run('vec:^([1], [])')).toThrow()
    })
    it('should exponentiate a vector by a scalar', () => {
      expect(lits.run('vec:^([1, 2, 3], 2)')).toEqual([1, 4, 9])
      expect(lits.run('vec:^([1], 2)')).toEqual([1])
      expect(lits.run('vec:^([], 2)')).toEqual([])
      expect(() => lits.run('vec:^([1, 2], [3, 4, 5])')).toThrow()
      expect(() => lits.run('vec:^([], [1])')).toThrow()
      expect(() => lits.run('vec:^([1], [])')).toThrow()
    })
    it('should exponentiate a scalar by a vector', () => {
      expect(lits.run('vec:^(2, [1, 2, 3])')).toEqual([2, 4, 8])
      expect(lits.run('vec:^(2, [1])')).toEqual([2])
      expect(lits.run('vec:^(2, [])')).toEqual([])
      expect(() => lits.run('vec:^([2], [3, 4, 5])')).toThrow()
      expect(() => lits.run('vec:^(2, 2)')).toThrow()
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
      expect(() => lits.run('vec:mean([])')).toThrow()
    })
  })
  describe('vec:median', () => {
    it('should calculate the median of a vector', () => {
      expect(lits.run('vec:median([1, 2, 3])')).toEqual(2)
      expect(lits.run('vec:median([1, -2, 3])')).toEqual(1)
      expect(lits.run('vec:median([-1, -2, -3])')).toEqual(-2)
      expect(lits.run('vec:median([0])')).toEqual(0)
      expect(() => lits.run('vec:median([])')).toThrow()
    })
  })
  describe('vec:mode', () => {
    it('should calculate the mode of a vector', () => {
      expect(lits.run('vec:mode([1, 2, 3])')).toEqual([1, 2, 3])
      expect(lits.run('vec:mode([1, 2, 2, 3])')).toEqual([2])
      expect(lits.run('vec:mode([0])')).toEqual([0])
      expect(() => lits.run('vec:mode([])')).toThrow()
    })
  })
  describe('vec:variance', () => {
    it('should calculate the variance of a vector', () => {
      expect(lits.run('vec:variance([1, 2, 3])')).toEqual(0.6666666666666666)
      expect(lits.run('vec:variance([1, 2, 2, 3])')).toEqual(0.5)
      expect(lits.run('vec:variance([0])')).toEqual(0)
      expect(() => lits.run('vec:variance([])')).toThrow()
    })
  })
  describe('vec:sample-variance', () => {
    it('should calculate the sample variance of a vector', () => {
      expect(lits.run('vec:sample-variance([1, 2, 3])')).toEqual(1)
      expect(lits.run('vec:sample-variance([1, 2, 2, 3])')).toEqual(0.6666666666666666)
      expect(() => lits.run('vec:sample-variance([0])')).toThrow()
      expect(() => lits.run('vec:sample-variance([])')).toThrow()
    })
  })
  describe('vec:stdev', () => {
    it('should calculate the standard deviation of a vector', () => {
      expect(lits.run('vec:stdev([1, 2, 3])')).toEqual(0.816496580927726)
      expect(lits.run('vec:stdev([1, 2, 2, 3])')).toEqual(0.7071067811865476)
      expect(lits.run('vec:stdev([0])')).toEqual(0)
      expect(() => lits.run('vec:stdev([])')).toThrow()
    })
  })
  describe('vec:sample-stdev', () => {
    it('should calculate the sample standard deviation of a vector', () => {
      expect(lits.run('vec:sample-stdev([1, 2, 3])')).toEqual(1)
      expect(lits.run('vec:sample-stdev([1, 2, 2, 3])')).toEqual(0.816496580927726)
      expect(() => lits.run('vec:sample-stdev([0])')).toThrow()
      expect(() => lits.run('vec:sample-stdev([])')).toThrow()
    })
  })
  describe('vec:min', () => {
    it('should find the minimum value in a vector', () => {
      expect(lits.run('vec:min([1, 2, 3])')).toEqual(1)
      expect(lits.run('vec:min([3, 2, 1])')).toEqual(1)
      expect(lits.run('vec:min([0])')).toEqual(0)
      expect(() => lits.run('vec:min([])')).toThrow()
    })
  })
  describe('vec:max', () => {
    it('should find the maximum value in a vector', () => {
      expect(lits.run('vec:max([1, 2, 3])')).toEqual(3)
      expect(lits.run('vec:max([3, 2, 1])')).toEqual(3)
      expect(lits.run('vec:max([0])')).toEqual(0)
      expect(() => lits.run('vec:max([])')).toThrow()
    })
  })
  describe('vec:min-index', () => {
    it('should find the index of the minimum value in a vector', () => {
      expect(lits.run('vec:min-index([1, 2, 3])')).toEqual(0)
      expect(lits.run('vec:min-index([3, 2, 1])')).toEqual(2)
      expect(lits.run('vec:min-index([0])')).toEqual(0)
      expect(() => lits.run('vec:min-index([])')).toThrow()
    })
  })
  describe('vec:max-index', () => {
    it('should find the index of the maximum value in a vector', () => {
      expect(lits.run('vec:max-index([1, 2, 3])')).toEqual(2)
      expect(lits.run('vec:max-index([3, 2, 1])')).toEqual(0)
      expect(lits.run('vec:max-index([0])')).toEqual(0)
      expect(() => lits.run('vec:max-index([])')).toThrow()
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
      expect(() => lits.run('vec:linspace(1, 10, -1)')).toThrow()
      expect(lits.run('vec:linspace(1, 10, 1)')).toEqual([1])
      expect(() => lits.run('vec:linspace(1, 10)')).toThrow()
    })
  })
  describe('vec:ones', () => {
    it('should create a vector of ones', () => {
      expect(lits.run('vec:ones(5)')).toEqual([1, 1, 1, 1, 1])
      expect(lits.run('vec:ones(0)')).toEqual([])
      expect(() => lits.run('vec:ones(-1)')).toThrow()
      expect(() => lits.run('vec:ones()')).toThrow()
    })
  })
  describe('vec:zeros', () => {
    it('should create a vector of zeros', () => {
      expect(lits.run('vec:zeros(5)')).toEqual([0, 0, 0, 0, 0])
      expect(lits.run('vec:zeros(0)')).toEqual([])
      expect(() => lits.run('vec:zeros(-1)')).toThrow()
      expect(() => lits.run('vec:zeros()')).toThrow()
    })
  })
  describe('vec:fill', () => {
    it('should create a vector filled with a value', () => {
      expect(lits.run('vec:fill(5, 3)')).toEqual([3, 3, 3, 3, 3])
      expect(lits.run('vec:fill(0, 0)')).toEqual([])
      expect(() => lits.run('vec:fill(-1, 5)')).toThrow()
      expect(() => lits.run('vec:fill()')).toThrow()
    })
  })
  describe('vec:generate', () => {
    it('should create a vector generated by a function', () => {
      expect(lits.run('vec:generate(5, -> $ * 2)')).toEqual([0, 2, 4, 6, 8])
      expect(lits.run('vec:generate(0, -> $ * 2)')).toEqual([])
      expect(() => lits.run('vec:generate(-1, -> $ * 2)')).toThrow()
      expect(() => lits.run('vec:generate(5)')).toThrow()
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
})
