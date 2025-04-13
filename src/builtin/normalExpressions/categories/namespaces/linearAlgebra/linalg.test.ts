import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'
import { LitsError } from '../../../../../errors'

const lits = new Lits()
describe('linalg functions', () => {
  describe('lin:dot', () => {
    it('should calculate the dot product of two vectors', () => {
      // Basic case
      expect(lits.run('lin:dot([1, 2, 3], [4, 5, 6])')).toEqual(32)
      // Case with negative numbers
      expect(lits.run('lin:dot([-1, -2, -3], [4, 5, 6])')).toEqual(-32)
      // Case with mixed numbers
      expect(lits.run('lin:dot([1, -2, 3], [4, 5, -6])')).toEqual(-24)
      // Case with empty vectors
      expect(lits.run('lin:dot([], [])')).toEqual(0)
      // Case with single element vectors
      expect(lits.run('lin:dot([42], [1])')).toEqual(42)
      // Case with different lengths (should throw an error)
      expect(() => lits.run('lin:dot([1, 2], [3])')).toThrowError('Vectors must be of the same length')
    })
  })
  describe('lin:cross', () => {
    it('should calculate the cross product of two vectors', () => {
      // Basic case
      expect(lits.run('lin:cross([1, 2, 3], [4, 5, 6])')).toEqual([-3, 6, -3])
      // Case with negative numbers
      expect(lits.run('lin:cross([-1, -2, -3], [4, 5, 6])')).toEqual([3, -6, 3])
      // Case with mixed numbers
      expect(lits.run('lin:cross([1, -2, 3], [4, 5, -6])')).toEqual([-3, 18, 13])
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:cross([], [])')).toThrowError(LitsError)
      // Case with single element vectors (should throw an error)
      expect(() => lits.run('lin:cross([42], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:normalize-minmax', () => {
    it('should normalize a vector using min-max normalization', () => {
      // Basic case with positive numbers
      expect(lits.run('lin:normalize-minmax([1, 2, 3, 4, 5])')).toEqual([0, 0.25, 0.5, 0.75, 1])

      // Case with negative and positive numbers
      expect(lits.run('lin:normalize-minmax([-10, 0, 10, 20, 30])')).toEqual([0, 0.25, 0.5, 0.75, 1])

      // Case with all same numbers (should handle division by zero)
      expect(lits.run('lin:normalize-minmax([5, 5, 5, 5])')).toEqual([0, 0, 0, 0])

      // Empty array
      expect(lits.run('lin:normalize-minmax([])')).toEqual([])

      // Single element array
      expect(lits.run('lin:normalize-minmax([42])')).toEqual([0])

      // Decimal values
      expect(lits.run('lin:normalize-minmax([1.5, 2.5, 3.5, 4.5])')).toEqual([0, 0.3333333333333333, 0.6666666666666666, 1])
    })
  })
  describe('lin:normalize-robust', () => {
    it('should normalize a vector using robust normalization', () => {
      // Basic case with positive numbers
      expect(lits.run('lin:normalize-robust([1, 2, 2, 4, 15])')).toEqual([
        -0.6744907594765952,
        0,
        0,
        1.3489815189531904,
        8.768379873195737,
      ])

      // Case with all same numbers (should handle division by zero)
      expect(lits.run('lin:normalize-robust([5, 5, 5, 5])')).toEqual([0, 0, 0, 0])

      // Empty array
      expect(lits.run('lin:normalize-robust([])')).toEqual([])

      // Single element array
      expect(lits.run('lin:normalize-robust([42])')).toEqual([0])
    })
  })
  describe('lin:normalize-zscore', () => {
    it('should normalize a vector using z-score normalization', () => {
      // Basic case with positive numbers
      expect(lits.run('lin:normalize-zscore([1, 2, 2, 4, 15])')).toEqual([
        -0.7318526549827781,
        -0.5392598510399418,
        -0.5392598510399418,
        -0.15407424315426904,
        1.9644466002169305,
      ])

      // Case with all same numbers (should handle division by zero)
      expect(lits.run('lin:normalize-zscore([5, 5, 5, 5])')).toEqual([0, 0, 0, 0])

      // Empty array
      expect(lits.run('lin:normalize-zscore([])')).toEqual([])

      // Single element array
      expect(lits.run('lin:normalize-zscore([42])')).toEqual([0])
    })
  })
  describe('lin:normalize-l1', () => {
    it('should normalize a vector using L1 normalization', () => {
      // Basic case with positive numbers
      expect(lits.run('lin:normalize-l1([1, 2, 3, 4, 5])')).toEqual([0.06666666666666667, 0.13333333333333333, 0.2, 0.26666666666666666, 0.3333333333333333])

      // Case with all same numbers (should handle division by zero)
      expect(lits.run('lin:normalize-l1([5, 5, 5, 5])')).toEqual([0.25, 0.25, 0.25, 0.25])

      // Empty array
      expect(lits.run('lin:normalize-l1([])')).toEqual([])

      // Single element array
      expect(lits.run('lin:normalize-l1([42])')).toEqual([1])
    })
  })
  describe('lin:normalize-l2', () => {
    it('should normalize a vector using L2 normalization', () => {
      // Basic case with positive numbers
      expect(lits.run('lin:normalize-l2([1, 2, 3, 4, 5])')).toEqual([0.13483997249264842, 0.26967994498529685, 0.40451991747794525, 0.5393598899705937, 0.674199862463242])
      // Case with all same numbers (should handle division by zero)
      expect(lits.run('lin:normalize-l2([5, 5, 5, 5])')).toEqual([0.5, 0.5, 0.5, 0.5])
      // Empty array
      expect(lits.run('lin:normalize-l2([])')).toEqual([])
      // Single element array
      expect(lits.run('lin:normalize-l2([42])')).toEqual([1])
    })
  })
  describe('lin:normalize-log', () => {
    it('should normalize a vector using log normalization', () => {
      // Basic case with positive numbers
      expect(lits.run('lin:normalize-log([1, 2, 3, 4, 5])')).toEqual([0, 0.6931471805599453, 1.0986122886681096, 1.3862943611198906, 1.6094379124341003])

      // Case with all same numbers (should handle division by zero)
      expect(lits.run('lin:normalize-log([5, 5, 5, 5])')).toEqual([0, 0, 0, 0])

      // Empty array
      expect(lits.run('lin:normalize-log([])')).toEqual([])

      // Single element array
      expect(lits.run('lin:normalize-log([42])')).toEqual([0])
    })
  })
  describe('lin:magnitude', () => {
    it('should calculate the magnitude of a vector', () => {
      // Basic case
      expect(lits.run('lin:magnitude([3, 4])')).toEqual(5)
      // Case with negative numbers
      expect(lits.run('lin:magnitude([-3, -4])')).toEqual(5)
      // Case with mixed numbers
      expect(lits.run('lin:magnitude([3, -4])')).toEqual(5)
      // Case with single element vector
      expect(lits.run('lin:magnitude([42])')).toEqual(42)
      // Case with empty vector
      expect(() => lits.run('lin:magnitude([])')).toThrowError(LitsError)
    })
  })
  describe('lin:angle', () => {
    it('should calculate the angle between two vectors', () => {
      // Basic case
      expect(lits.run('lin:angle([1, 0], [0, 1])')).toEqual(Math.PI / 2)
      // Case with negative numbers
      expect(lits.run('lin:angle([-1, 0], [0, -1])')).toEqual(Math.PI / 2)
      // Case with mixed numbers
      expect(lits.run('lin:angle([1, -1], [-1, 1])')).toBeCloseTo(Math.PI)
      // Case with single element vectors
      expect(lits.run('lin:angle([42], [1])')).toBeCloseTo(0)
      expect(lits.run('lin:angle([42], [-1])')).toBeCloseTo(Math.PI)
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:angle([], [])')).toThrowError(LitsError)
    })
  })
})
