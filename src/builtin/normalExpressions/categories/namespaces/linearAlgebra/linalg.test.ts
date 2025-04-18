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

      expect(lits.run('lin:normalize-l1([0, 0, 0, 0])')).toEqual([0, 0, 0, 0])

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

      expect(lits.run('lin:normalize-l2([0, 0, 0, 0])')).toEqual([0, 0, 0, 0])
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

      expect(() => lits.run('lin:normalize-log([-42])')).toThrow()
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
      expect(() => lits.run('lin:angle([0, 0], [1, 0])')).toThrowError(LitsError)
      expect(() => lits.run('lin:angle([1, 0, 1], [1, 0])')).toThrowError(LitsError)
    })
  })
  describe('lin:projection', () => {
    it('should calculate the projection of one vector onto another', () => {
      // Basic case
      expect(lits.run('lin:projection([1, 2], [3, 4])')).toEqual([1.32, 1.76])
      // Case with negative numbers
      expect(lits.run('lin:projection([-1, -2], [-3, -4])')).toEqual([-1.32, -1.76])
      // Case with mixed numbers
      expect(lits.run('lin:projection([1, -2], [-3, 4])')).toEqual([1.32, -1.76])
      // Case with single element vectors (should throw an error)
      expect(lits.run('lin:projection([42], [1])')).toEqual([42])
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:projection([], [])')).toThrowError(LitsError)
      expect(() => lits.run('lin:projection([1, 0], [0, 0])')).toThrowError(LitsError)
      expect(() => lits.run('lin:projection([1, 0, 1], [1, 0])')).toThrowError(LitsError)
    })
  })
  describe('lin:orthogonal?', () => {
    it('should check if two vectors are orthogonal', () => {
      // Basic case
      expect(lits.run('lin:orthogonal?([1, 0], [0, 1])')).toEqual(true)
      // Case with negative numbers
      expect(lits.run('lin:orthogonal?([-1, 0], [0, -1])')).toEqual(true)
      // Case with mixed numbers
      expect(lits.run('lin:orthogonal?([1, -1], [-1, 1])')).toEqual(false)
      // Case with single element vectors
      expect(lits.run('lin:orthogonal?([42], [1])')).toEqual(false)
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:orthogonal?([], [])')).toThrowError(LitsError)

      expect(() => lits.run('lin:orthogonal?([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:parallel?', () => {
    it('should check if two vectors are parallel', () => {
      // Basic case
      expect(lits.run('lin:parallel?([1, 0], [2, 0])')).toEqual(true)
      // Case with negative numbers
      expect(lits.run('lin:parallel?([-1, 0], [-2, 0])')).toEqual(true)
      // Case with mixed numbers
      expect(lits.run('lin:parallel?([1, -1], [-2, 2])')).toEqual(false) // collinear though

      expect(lits.run('lin:parallel?([0, 0], [-2, 2])')).toEqual(true)

      expect(lits.run('lin:parallel?([2, -3, 4], [6, -9, 12])')).toEqual(true)
      expect(lits.run('lin:parallel?([2, -3, 4], [-6, 9, -12])')).toEqual(false) // collinear though
      expect(lits.run('lin:parallel?([2, -3, 4], [6, 9, 12])')).toEqual(false)
      // Case with single element vectors
      expect(lits.run('lin:parallel?([42], [1])')).toEqual(true)
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:parallel?([], [])')).toThrowError(LitsError)

      expect(() => lits.run('lin:parallel?([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:collinear?', () => {
    it('should check if two vectors are collinear', () => {
      // Basic case
      expect(lits.run('lin:collinear?([1, 0], [2, 0])')).toEqual(true)
      expect(lits.run('lin:collinear?([0, 1, 0], [0, 2, 0])')).toEqual(true)

      expect(lits.run('lin:collinear?([0, 0], [2, 0])')).toEqual(true)
      expect(lits.run('lin:collinear?([2, 0], [0, 0])')).toEqual(true)
      expect(lits.run('lin:collinear?([2, 0], [2, 2])')).toEqual(false)
      // Case with negative numbers
      expect(lits.run('lin:collinear?([-1, 0], [-2, 0])')).toEqual(true)
      // Case with mixed numbers
      expect(lits.run('lin:collinear?([1, -1], [-2, 2])')).toEqual(true)

      expect(lits.run('lin:collinear?([2, -3, 4], [6, -9, 12])')).toEqual(true)
      expect(lits.run('lin:collinear?([2, -3, 4], [-6, 9, -12])')).toEqual(true)
      expect(lits.run('lin:collinear?([2, -3, 4], [6, 9, 12])')).toEqual(false)
      // Case with single element vectors
      expect(lits.run('lin:collinear?([42], [1])')).toEqual(true)
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:collinear?([], [])')).toThrowError(LitsError)

      expect(() => lits.run('lin:collinear?([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:cosine-similarity', () => {
    it('should calculate the cosine similarity between two vectors', () => {
      // Basic case
      expect(lits.run('lin:cosine-similarity([1, 0], [0, 1])')).toEqual(0)
      // Case with negative numbers
      expect(lits.run('lin:cosine-similarity([-1, 0], [0, -1])')).toEqual(0)
      // Case with mixed numbers
      expect(lits.run('lin:cosine-similarity([1, -1], [-1, 1])')).toBeCloseTo(-1)
      // Case with single element vectors
      expect(lits.run('lin:cosine-similarity([42], [1])')).toBeCloseTo(1)
      // Case with zero vector
      expect(() => lits.run('lin:cosine-similarity([0, 0, 0], [1, 2, 3])')).toThrowError(LitsError)
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:cosine-similarity([], [])')).toThrowError(LitsError)

      expect(() => lits.run('lin:cosine-similarity([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:euclidean-distance', () => {
    it('should calculate the Euclidean distance between two vectors', () => {
      // Basic case
      expect(lits.run('lin:euclidean-distance([1, 2], [4, 6])')).toEqual(5)
      // Basic case
      expect(lits.run('lin:distance([1, 2], [1, 2])')).toEqual(0)
      // Case with negative numbers
      expect(lits.run('lin:l2-distance([-1, -2], [-4, -6])')).toEqual(5)
      // Case with mixed numbers
      expect(lits.run('lin:euclidean-distance([1, -2], [-4, 6])')).toBeCloseTo(9.433981132056603)
      // Case with single element vectors
      expect(lits.run('lin:euclidean-distance([42], [1])')).toEqual(41)
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:euclidean-distance([], [])')).toThrowError(LitsError)
      expect(() => lits.run('lin:distance([], [])')).toThrowError(LitsError)

      expect(() => lits.run('lin:distance([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:euclidean-norm', () => {
    it('should calculate the L2 norm of a vector', () => {
      // Basic case
      expect(lits.run('lin:euclidean-norm([3, 4])')).toEqual(5)
      // Case with negative numbers
      expect(lits.run('lin:magnitude([-3, -4])')).toEqual(5)
      // Case with mixed numbers
      expect(lits.run('lin:l2-norm([3, -4])')).toEqual(5)
      // Case with single element vector
      expect(lits.run('lin:magnitude([42])')).toEqual(42)
      // Case with empty vector
      expect(() => lits.run('lin:magnitude([])')).toThrowError(LitsError)
    })
  })
  describe('lin:manhattan-distance', () => {
    it('should calculate the Manhattan distance between two vectors', () => {
      // Basic case
      expect(lits.run('lin:manhattan-distance([1, 2], [4, 6])')).toEqual(7)
      // Basic case
      expect(lits.run('lin:l1-distance([1, 2], [1, 2])')).toEqual(0)
      // Case with negative numbers
      expect(lits.run('lin:cityblock-distance([-1, -2], [-4, -6])')).toEqual(7)
      // Case with mixed numbers
      expect(lits.run('lin:manhattan-distance([1, -2], [-4, 6])')).toBeCloseTo(13)
      // Case with single element vectors
      expect(lits.run('lin:manhattan-distance([42], [1])')).toEqual(41)
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:manhattan-distance([], [])')).toThrowError(LitsError)

      expect(() => lits.run('lin:manhattan-distance([1, 2], [2])')).toThrowError(LitsError)
    })
  })
  describe('lin:manhattan-norm', () => {
    it('should calculate the L1 norm of a vector', () => {
      // Basic case
      expect(lits.run('lin:l1-norm([1, 2, 3])')).toEqual(6)
      // Case with negative numbers
      expect(lits.run('lin:manhattan-norm([-1, -2, -3])')).toEqual(6)
      // Case with mixed numbers
      expect(lits.run('lin:cityblock-norm([1, -2, 3])')).toEqual(6)
      // Case with single element vector
      expect(lits.run('lin:l1-norm([42])')).toEqual(42)
      // Case with empty vector
      expect(() => lits.run('lin:l1-norm([])')).toThrowError(LitsError)
    })
  })
  describe('lin:hamming-distance', () => {
    it('should calculate the Hamming distance between two vectors', () => {
      // Basic case
      expect(lits.run('lin:hamming-distance([1, 2, 3], [1, 2, 4])')).toEqual(1)
      // Basic case
      expect(lits.run('lin:hamming-distance([1, 2, 3], [1, 2, 3])')).toEqual(0)
      // Case with negative numbers
      expect(lits.run('lin:hamming-distance([-1, -2], [-4, -6])')).toEqual(2)
      // Case with mixed numbers
      expect(lits.run('lin:hamming-distance([1, -2], [-4, 6])')).toEqual(2)
      // Case with single element vectors
      expect(lits.run('lin:hamming-distance([42], [1])')).toEqual(1)
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:hamming-distance([], [])')).toThrowError(LitsError)
      expect(() => lits.run('lin:hamming-distance([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:hamming-norm', () => {
    it('should calculate the Hamming norm of a vector', () => {
      // Basic case
      expect(lits.run('lin:hamming-norm([1, 2, 3])')).toEqual(3)
      // Case with negative numbers
      expect(lits.run('lin:hamming-norm([0, -2, -3])')).toEqual(2)
      // Case with mixed numbers
      expect(lits.run('lin:hamming-norm([0, 0, 0])')).toEqual(0)
      // Case with single element vector
      expect(lits.run('lin:hamming-norm([42])')).toEqual(1)
      // Case with empty vector
      expect(() => lits.run('lin:hamming-norm([])')).toThrowError(LitsError)
    })
  })
  describe('lin:chebyshev-distance', () => {
    it('should calculate the Chebyshev distance between two vectors', () => {
      // Basic case
      expect(lits.run('lin:chebyshev-distance([1, 2], [4, 6])')).toEqual(4)
      // Basic case
      expect(lits.run('lin:chebyshev-distance([1, 2], [1, 2])')).toEqual(0)
      // Case with negative numbers
      expect(lits.run('lin:chebyshev-distance([-1, -2], [-4, -6])')).toEqual(4)
      // Case with mixed numbers
      expect(lits.run('lin:chebyshev-distance([1, -2], [-4, 6])')).toBeCloseTo(8)
      // Case with single element vectors
      expect(lits.run('lin:chebyshev-distance([42], [1])')).toEqual(41)
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:chebyshev-distance([], [])')).toThrowError(LitsError)
      expect(() => lits.run('lin:chebyshev-distance([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:chebyshev-norm', () => {
    it('should calculate the Chebyshev norm of a vector', () => {
      // Basic case
      expect(lits.run('lin:chebyshev-norm([1, 2, 3])')).toEqual(3)
      // Case with negative numbers
      expect(lits.run('lin:chebyshev-norm([-1, -2, -3])')).toEqual(3)
      // Case with mixed numbers
      expect(lits.run('lin:chebyshev-norm([1, -2, 3])')).toEqual(3)
      // Case with single element vector
      expect(lits.run('lin:chebyshev-norm([42])')).toEqual(42)
      // Case with empty vector
      expect(() => lits.run('lin:chebyshev-norm([])')).toThrowError(LitsError)
    })
  })
  describe('lin:minkowski-distance', () => {
    it('should calculate the Minkowski distance between two vectors', () => {
      // Basic case
      expect(lits.run('lin:minkowski-distance([1, 2], [4, 6], 3)')).toBeCloseTo(4.5)
      // Basic case
      expect(lits.run('lin:minkowski-distance([1, 2], [4, 6], 1)')).toBeCloseTo(7)
      // Basic case
      expect(lits.run('lin:minkowski-distance([1, 2], [4, 6], 2)')).toBeCloseTo(5)
      // Basic case
      expect(lits.run('lin:minkowski-distance([1, 2], [4, 6], 0.5)')).toBeCloseTo(13.93)
      // Basic case
      expect(lits.run('lin:minkowski-distance([1, 2], [1, 2], 3)')).toEqual(0)
      // Case with negative numbers
      expect(lits.run('lin:minkowski-distance([-1, -2], [-4, -6], 3)')).toBeCloseTo(4.5)
      // Case with mixed numbers
      expect(lits.run('lin:minkowski-distance([1, -2], [-4, 6], 3)')).toBeCloseTo(8.6)
      // Case with single element vectors
      expect(lits.run('lin:minkowski-distance([42], [1], 3)')).toBeCloseTo(41)
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:minkowski-distance([], [], 3)')).toThrowError(LitsError)
      // Case with invalid p value (should throw an error)
      expect(() => lits.run('lin:minkowski-distance([1, 2], [4, 6], 0)')).toThrowError(LitsError)
      expect(() => lits.run('lin:minkowski-distance([1, 2], [4, 6], -1)')).toThrowError(LitsError)
      expect(() => lits.run('lin:minkowski-distance([1, 2, 3], [4, 6], 2)')).toThrowError(LitsError)
    })
  })
  describe('lin:minkowski-norm', () => {
    it('should calculate the Minkowski norm of a vector', () => {
      // Basic case
      expect(lits.run('lin:minkowski-norm([1, 2, 3], 3)')).toBeCloseTo(3.3019272488946263)
      // Basic case
      expect(lits.run('lin:minkowski-norm([1, 2, 3], 1)')).toEqual(6)
      // Basic case
      expect(lits.run('lin:minkowski-norm([1, 2, 3], 2)')).toBeCloseTo(3.7416573867739413)
      // Basic case
      expect(lits.run('lin:minkowski-norm([1, 2, 3], 0.5)')).toBeCloseTo(17.19)
      // Case with negative numbers
      expect(lits.run('lin:minkowski-norm([-1, -2, -3], 3)')).toBeCloseTo(3.3019272488946263)
      // Case with mixed numbers
      expect(lits.run('lin:minkowski-norm([1, -2, 3], 3)')).toBeCloseTo(3.3019272488946263)
      // Case with single element vector
      expect(lits.run('lin:minkowski-norm([42], 3)')).toBeCloseTo(42)
      // Case with empty vector (should throw an error)
      expect(() => lits.run('lin:minkowski-norm([], 3)')).toThrowError(LitsError)
      // Case with invalid p value (should throw an error)
      expect(() => lits.run('lin:minkowski-distance([1, 2], 0)')).toThrowError(LitsError)
      expect(() => lits.run('lin:minkowski-distance([1, 2], -1)')).toThrowError(LitsError)
    })
  })
  describe('lin:cov', () => {
    it('should calculate the covariance between two vectors', () => {
      // Basic case
      expect(lits.run('lin:cov([1, 2, 3], [4, 5, 6])')).toBeCloseTo(2 / 3)
      // Case with negative numbers
      expect(lits.run('lin:cov([-1, -2, -3], [-4, -5, -6])')).toBeCloseTo(2 / 3)
      // Case with mixed numbers
      expect(lits.run('lin:cov([1, -2, 3], [-4, 5, -6])')).toBeCloseTo(-9.56)
      // Case with single element vectors
      expect(lits.run('lin:cov([42], [1])')).toBe(0)
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:cov([], [])')).toThrowError(LitsError)
      expect(() => lits.run('lin:cov([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:corr', () => {
    it('should calculate the correlation between two vectors', () => {
      // Basic case
      expect(lits.run('lin:corr([1, 2, 3], [4, 5, 6])')).toBeCloseTo(1)
      // Case with negative numbers
      expect(lits.run('lin:corr([-1, -2, -3], [-4, -5, -6])')).toBeCloseTo(1)
      // Case with mixed numbers
      expect(lits.run('lin:corr([1, -2, 3], [-4, 5, -6])')).toBeCloseTo(-0.972)
      // Case with single element vectors (should throw an error)
      expect(() => lits.run('lin:corr([42], [1])')).toThrowError(LitsError)
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:corr([], [])')).toThrowError(LitsError)
      expect(() => lits.run('lin:corr([1, 2], [2])')).toThrowError(LitsError)
    })
  })
  describe('lin:spearman-corr', () => {
    it('should calculate the Spearman correlation between two vectors', () => {
      expect(lits.run('lin:spearman-corr([1, 2, 3], [4, 5, 6])')).toBeCloseTo(1)
      expect(lits.run('lin:spearman-rho([1, 2, 3, 4, 5], [1, 2, 3, 4, 5])')).toBeCloseTo(1)
      expect(lits.run('lin:spearman-corr([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])')).toBeCloseTo(-1)
      expect(lits.run('lin:spearman-rho([1, 2, 3, 4, 5], [5, 2, 4, 1, 3])')).toBeCloseTo(-0.5)
      expect(lits.run('lin:spearman-corr([1, 2, 3, 4, 100], [1, 2, 3, 4, 5])')).toBeCloseTo(1)

      // Case with single element vectors (should throw an error)
      expect(() => lits.run('lin:spearman-corr([42], [1])')).toThrowError(LitsError)
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:spearman-corr([], [])')).toThrowError(LitsError)
      expect(() => lits.run('lin:spearman-corr([1, 2], [1])')).toThrowError(LitsError)
      expect(() => lits.run('lin:spearman-corr([1, 1], [1, 2])')).toThrowError(LitsError)
    })
  })
  describe('lin:pearson-corr', () => {
    it('should calculate the Pearson correlation between two vectors', () => {
      expect(lits.run('lin:pearson-corr([1, 2, 3], [4, 5, 6])')).toBeCloseTo(1)
      expect(lits.run('lin:pearson-corr([1, 2, 3, 4, 5], [1, 2, 3, 4, 5])')).toBeCloseTo(1)
      expect(lits.run('lin:pearson-corr([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])')).toBeCloseTo(-1)
      expect(lits.run('lin:pearson-corr([1, 2, 3, 4, 5], [5, 2, 4, 1, 3])')).toBeCloseTo(-0.5)
      expect(lits.run('lin:pearson-corr([1, 2, 3, 4, 100], [1, 2, 3, 4, 5])')).toBeCloseTo(0.725)

      // Case with single element vectors (should throw an error)
      expect(() => lits.run('lin:pearson-corr([42], [1])')).toThrowError(LitsError)
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:pearson-corr([], [])')).toThrowError(LitsError)
      expect(() => lits.run('lin:pearson-corr([1, 2], [1])')).toThrowError(LitsError)
      expect(() => lits.run('lin:pearson-corr([1, 1], [1, 2])')).toThrowError(LitsError)
    })
  })

  describe('lin:kendall-tau', () => {
    it('should handle all my test cases', () => {
      // Perfect positive correlation
      expect(lits.run('lin:kendall-tau([1, 2, 3, 4, 5], [10, 20, 30, 40, 50])')).toBeCloseTo(1.0)

      // Perfect negative correlation
      expect(lits.run('lin:kendall-tau([1, 2, 3, 4, 5], [50, 40, 30, 20, 10])')).toBeCloseTo(-1.0)

      // No correlation (random data)
      expect(lits.run('lin:kendall-tau([1, 2, 3, 4, 5], [3, 5, 1, 4, 2])')).toBeCloseTo(-0.2)

      // Ties in vector A
      expect(lits.run('lin:kendall-tau([1, 2, 2, 4, 5], [10, 20, 30, 40, 50])')).toBeCloseTo(0.9487)

      // Ties in vector B
      expect(lits.run('lin:kendall-tau([1, 2, 3, 4, 5], [10, 20, 20, 40, 50])')).toBeCloseTo(0.9487)

      // Ties in both vectors
      expect(lits.run('lin:kendall-tau([1, 2, 2, 4, 5], [10, 20, 20, 40, 50])')).toBeCloseTo(1.0)

      // Manual verification (small example with known result)
      expect(lits.run('lin:kendall-tau([1, 2, 3, 4], [2, 1, 4, 3])')).toBeCloseTo(0.3333)

      // Very close values that should be considered ties with default epsilon
      expect(lits.run('lin:kendall-tau([1.0000001, 1.0000002, 2, 3], [5, 6, 7, 8])')).toBeCloseTo(1.0)

      // Single pair
      expect(lits.run('lin:kendall-tau([1, 2], [3, 4])')).toBeCloseTo(1.0)

      // More complex rank example
      expect(lits.run('lin:kendall-tau([1, 2, 3, 4, 5, 6], [2, 3, 1, 5, 6, 4])')).toBeCloseTo(0.4667)

      // Another rank example
      expect(lits.run('lin:kendall-tau([1, 2, 3, 4, 5, 6, 7, 8], [3, 4, 1, 2, 7, 8, 5, 6])')).toBeCloseTo(0.4286)

      // Simple counterexample
      expect(lits.run('lin:kendall-tau([1, 3, 2], [1, 2, 3])')).toBeCloseTo(0.3333)

      // All ties in vector A
      expect(() => lits.run('lin:kendall-tau([5, 5, 5, 5], [1, 2, 3, 4])')).toThrow('Not enough data to calculate Kendall\'s Tau')

      // All ties in vector B
      expect(() => lits.run('lin:kendall-tau([1, 2, 3, 4], [7, 7, 7, 7])')).toThrow(LitsError)

      // Mixed ties and values
      expect(lits.run('lin:kendall-tau([1, 1, 3, 4], [5, 5, 7, 8])')).toBeCloseTo(1.0)

      // Another mixed case
      expect(lits.run('lin:kendall-tau([1, 2, 3, 3], [4, 5, 6, 6])')).toBeCloseTo(1.0)

      // Partial correlation
      expect(lits.run('lin:kendall-tau([1, 2, 3, 4, 5], [1, 3, 2, 5, 4])')).toBeCloseTo(0.6)

      // Empty arrays (should throw an error)
      expect(() => lits.run('lin:kendall-tau([], [])')).toThrow(LitsError)

      // Boundary case: arrays of length 1
      expect(() => lits.run('lin:kendall-tau([1], [2])')).toThrow(LitsError)
      expect(() => lits.run('lin:kendall-tau([1, 2, 3], [1, 2])')).toThrow(LitsError)

      // All tied pairs (corner case)
      expect(() => lits.run('lin:kendall-tau([1, 1, 1], [2, 2, 2])')).toThrow('Not enough data to calculate Kendall\'s Tau')

      // Inverse relationship with some noise
      expect(lits.run('lin:kendall-tau([1, 2, 3, 4, 5], [5, 3, 4, 2, 1])')).toBeCloseTo(-0.8)

      // Test with fractional values
      expect(lits.run('lin:kendall-tau([1.5, 2.3, 3.7, 4.2], [5.1, 4.8, 3.2, 2.9])')).toBeCloseTo(-1.0)
    })
  })
  describe('lin:autocorrelation', () => {
    it('should calculate the autocorrelation of a vector', () => {
      // Basic cases
      expect(lits.run('lin:autocorrelation([1, 2, 3, 4, 5], 0)')).toBeCloseTo(1)
      expect(lits.run('lin:autocorrelation([1, 2, 3, 4, 5], 1)')).toBeCloseTo(0.4)
      expect(lits.run('lin:autocorrelation([1, 2, 3, 4, 5], -1)')).toBeCloseTo(0.4)
      expect(lits.run('lin:autocorrelation([1, 2, 3, 4, 5], 0)')).toBeCloseTo(1)
      expect(lits.run('lin:autocorrelation([1, 2, 3, 4, 5], 2)')).toBeCloseTo(-0.1)
      expect(lits.run('lin:acf([1, 2, 3, 4, 5], 3)')).toBeCloseTo(-0.4)
      expect(lits.run('lin:autocorrelation([1, 2, 3, 4, 5], 4)')).toBeCloseTo(-0.4)
      // Case with negative numbers
      expect(lits.run('lin:autocorrelation([-1, -2, -3, -4, -5], 1)')).toBeCloseTo(0.4)
      // Case with mixed numbers
      expect(lits.run('lin:autocorrelation([1, -2, 3, -4, 5], 2)')).toBeCloseTo(0.441)

      expect(lits.run('lin:autocorrelation([1, 1, 1, 1, 1], 2)')).toBe(0)
      expect(lits.run('lin:autocorrelation([1, 1, 1, 1, 1], 0)')).toBe(1)

      // Case with zero lag
      expect(() => lits.run('lin:autocorrelation([42], 0)')).toThrowError(LitsError)
      // Case with empty vectors (should throw an error)
      expect(() => lits.run('lin:autocorrelation([], 0)')).toThrowError(LitsError)
    })
  })
  describe('lin:cross-correlation', () => {
    it('should calculate the cross correlation for two vectors and lag', () => {
      // Basic cases - identical vectors
      expect(lits.run('lin:cross-correlation([1, 2, 3, 4, 5], [1, 2, 3, 4, 5], 0)')).toBeCloseTo(1)
      expect(lits.run('lin:cross-correlation([1, 2, 3, 4, 5], [1, 2, 3, 4, 5], 1)')).toBeCloseTo(1)
      expect(lits.run('lin:cross-correlation([1, 2, 3, 4, 5], [1, 2, 3, 4, 5], 2)')).toBeCloseTo(1)

      // Perfectly negatively correlated vectors
      expect(lits.run('lin:cross-correlation([1, 2, 3, 4, 5], [5, 4, 3, 2, 1], 0)')).toBeCloseTo(-1)
      expect(lits.run('lin:cross-correlation([1, 2, 3, 4, 5], [5, 4, 3, 2, 1], 1)')).toBeCloseTo(-1)

      expect(lits.run('lin:cross-correlation([1, 1, 1], [1, 1, 1], 1)')).toBeCloseTo(1)

      // Similar patterns with offset
      expect(lits.run('lin:cross-correlation([10, 12, 15, 10, 8, 15, 20, 25, 18, 15], [8, 10, 14, 9, 7, 13, 19, 23, 17, 14], 0)')).toBeCloseTo(0.995)
      expect(lits.run('lin:cross-correlation([10, 12, 15, 10, 8, 15, 20, 25, 18, 15], [8, 10, 14, 9, 7, 13, 19, 23, 17, 14], 1)')).toBeCloseTo(0.614)

      // Uncorrelated vectors
      expect(lits.run('lin:cross-correlation([1, 2, 3, 4, 5], [5, 1, 3, 2, 4], 0)')).toBeCloseTo(-0.1)
      expect(lits.run('lin:cross-correlation([1, 2, 3, 4, 5], [5, 1, 3, 2, 4], 1)')).toBeCloseTo(0.8)

      // Constant vectors (edge cases)
      expect(lits.run('lin:cross-correlation([5, 5, 5, 5, 5], [5, 5, 5, 5, 5], 0)')).toBeCloseTo(1)
      expect(lits.run('lin:cross-correlation([5, 5, 5, 5, 5], [10, 10, 10, 10, 10], 0)')).toBeCloseTo(0)

      // Sine and cosine waves (phase-shifted signals)
      const sine = [0, 0.588, 0.951, 0.951, 0.588, 0, -0.588, -0.951, -0.951, -0.588]
      const cosine = [1, 0.809, 0.309, -0.309, -0.809, -1, -0.809, -0.309, 0.309, 0.809]
      expect(lits.run(`lin:cross-correlation([${sine.join(', ')}], [${sine.join(', ')}], 0)`)).toBeCloseTo(1)
      expect(lits.run(`lin:cross-correlation([${sine.join(', ')}], [${cosine.join(', ')}], 0)`)).toBeCloseTo(0)
      expect(lits.run(`lin:cross-correlation([${sine.join(', ')}], [${sine.join(', ')}], 5)`)).toBeCloseTo(-1)

      // Negative lags
      expect(lits.run('lin:cross-correlation([1, 2, 3, 4, 5], [1, 2, 3, 4, 5], -1)')).toBeCloseTo(1)
      expect(lits.run('lin:cross-correlation([1, 2, 3, 4, 5], [5, 4, 3, 2, 1], -1)')).toBeCloseTo(-1)

      expect(() => lits.run('lin:cross-correlation([1, 2, 3, 4, 5, 6], [5, 4, 3, 2, 1], -1)')).toThrow()
      expect(() => lits.run('lin:cross-correlation([1], [5], -1)')).toThrow()
    })
  })
  describe('lin:rref', () => {
    it('should calculate the reduced row echelon form of a matrix', () => {
      // Basic case
      expect(lits.run('lin:rref([[1, 2], [3, 4]])')).toEqual([[1, 0], [0, 1]])
      expect(lits.run('lin:rref([[3, 4], [2, 1]])')).toEqual([[1, 0], [0, 1]])
      // Case with negative numbers
      expect(lits.run('lin:rref([[-1, -2], [-3, -4]])')).toEqual([[1, 0], [0, 1]])
      // Case with mixed numbers
      expect(lits.run('lin:rref([[1, -2], [-3, 4]])')).toEqual([[1, 0], [0, 1]])
      // Case with single element matrix
      expect(lits.run('lin:rref([[42]])')).toEqual([[1]])

      expect(lits.run(`lin:rref([
  [0, 2, 3],
  [1, 2, 3],
  [4, 5, 6]
])`)).toEqual([[1, 0, 0], [0, 1, 0], [0, 0, 1]])
      // Case with empty matrix (should throw an error)
      expect(() => lits.run('lin:rref([])')).toThrowError(LitsError)
    })
  })
  describe('lin:solve', () => {
    it('should solve systems of linear equations', () => {
      // Unique solution: 2x + 3y = 8, x - y = 2 => x = 2, y = 4/3
      expect(lits.run('lin:solve([[2, 3], [1, -1]], [8, 2])')).toEqual([2.8, 0.8])

      // Identity matrix: x = b
      expect(lits.run('lin:solve([[1, 0, 0], [0, 1, 0], [0, 0, 1]], [5, -2, 3])')).toEqual([5, -2, 3])

      // Inconsistent system: x + y = 1, x + y = 2 (no solution)
      expect(lits.run('lin:solve([[1, 1], [1, 1]], [1, 2])')).toBeNull()

      // Upper triangular system
      expect(lits.run('lin:solve([[1, 2, 3], [0, 4, 5], [0, 0, 6]], [14, 23, 18])')).toEqual([1, 2, 3])

      // Lower triangular system
      expect(lits.run('lin:solve([[2, 0, 0], [3, 1, 0], [4, 5, 6]], [4, 5, 38])')).toEqual([2, -1, 35 / 6])

      // Practical example: mixture problem
      // 0.3x + 0.5y = 0.35 (blend percentage)
      // x + y = 100 (total amount)
      expect((lits.run('lin:solve([[0.3, 0.5], [1, 1]], [35, 100])') as number[])[0]).toBeCloseTo(75)
      expect((lits.run('lin:solve([[0.3, 0.5], [1, 1]], [35, 100])') as number[])[1]).toBeCloseTo(25)

      // Economic equilibrium example:
      // 10 - 2p + q = 5 (demand for product 1)
      // 15 + p - 3q = 5 (demand for product 2)
      expect(lits.run('lin:solve([[-2, 1], [1, -3]], [-5, -10])')).toEqual([5, 5])

      // Small values / precision test
      expect(lits.run('lin:solve([[1, 1], [2, 2]], [5, 7])')).toBeNull()

      expect(() => lits.run('lin:solve([[1, 1], [2, 2]], [5, 7, 3])')).toThrow()

      // Larger system (4×4)
      expect(lits.run(`lin:solve([
        [2, 1, -1, 1], 
        [4, 5, -3, 2], 
        [6, -2, 5, -3], 
        [8, 3, 2, 4]
      ], [5, 10, 2, 17])`)).toEqual([1.519607843137255, -0.17647058823529416, -0.5294117647058822, 1.6078431372549018])
    })
  })
})
