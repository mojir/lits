import { describe, expect, it } from 'vitest'
import { Lits } from '../../Lits/Lits'
import { LitsError } from '../../errors'
import { deepEqual } from '../../utils'
import { getUnit } from './helpers/getUnit'

const lits = new Lits()

// Helper to run lin namespace functions with the new import syntax
function runLin(code: string): unknown {
  // Replace all 'lin:functionName(' with 'lin.functionName(' and add import at start
  const modifiedCode = 'let lin = import("lin"); ' + code.replace(/lin:/g, 'lin.')
  return lits.run(modifiedCode)
}

describe('linalg functions', () => {
  describe('lin:rotate2d', () => {
    it('should rotate a 2D vector by a given angle', () => {
      // Basic case
      expect(deepEqual(runLin('lin:rotate2d([1, 0], PI / 2)'), [0, 1])).toBeTruthy()
      // Case with negative angle
      expect(deepEqual(runLin('lin:rotate2d([1, 0], -PI / 2)'), [0, -1])).toBeTruthy()
      // zero vector
      expect(deepEqual(runLin('lin:rotate2d([0, 0], -PI / 2)'), [0, 0])).toBeTruthy()
      // Case with zero angle
      expect(runLin('lin:rotate2d([1, 0], 0)')).toEqual([1, 0])
      // Case with empty vector (should throw an error)
      expect(() => runLin('lin:rotate2d([], PI / 2)')).toThrowError(LitsError)
      // Case with single element vector (should throw an error)
      expect(() => runLin('lin:rotate2d([42], PI / 2)')).toThrowError(LitsError)
      // Case with 3D vector (should throw an error)
      expect(() => runLin('lin:rotate2d([1, 0, 0], PI / 2)')).toThrowError(LitsError)
    })
  })
  describe('lin:rotate3d', () => {
    it('should rotate a 3D vector by a given angle around a given axis', () => {
      // Basic case
      expect(deepEqual(runLin('lin:rotate3d([1, 0, 0], [0, 1, 0], PI / 2)'), [0, 0, -1])).toBeTruthy()
      // Case with negative angle
      expect(deepEqual(runLin('lin:rotate3d([1, 0, 0], [0, 1, 0], -PI / 2)'), [0, 0, 1])).toBeTruthy()
      // Case with zero angle
      expect(runLin('lin:rotate3d([1, 0, 0], [0, 1, 0], 0)')).toEqual([1, 0, 0])
      // Case with zero vector
      expect(runLin('lin:rotate3d([0, 0, 0], [0, 1, 0], PI)')).toEqual([0, 0, 0])
      // Case with zero axis (should throw an error)
      expect(() => runLin('lin:rotate3d([1, 1, 1], [0, 0, 0], PI / 2)')).toThrowError(LitsError)
      // Case with empty vector (should throw an error)
      expect(() => runLin('lin:rotate3d([], [0, 1, 0], PI / 2)')).toThrowError(LitsError)
      // Case with single element vector (should throw an error)
      expect(() => runLin('lin:rotate3d([42], [0, 1, 0], PI / 2)')).toThrowError(LitsError)
      // Case with non-3D vector (should throw an error)
      expect(() => runLin('lin:rotate3d([1, 2], [0, 1, 0], PI / 2)')).toThrowError(LitsError)
    })
  })
  describe('lin:lerp', () => {
    it('should linearly interpolate between two vectors', () => {
      // Basic case - interpolate halfway between
      expect(deepEqual(runLin('lin:lerp([0, 0], [10, 10], 0.5)'), [5, 5])).toBeTruthy()

      // Start of interpolation (t=0)
      expect(deepEqual(runLin('lin:lerp([5, 10], [20, 30], 0)'), [5, 10])).toBeTruthy()

      // End of interpolation (t=1)
      expect(deepEqual(runLin('lin:lerp([5, 10], [20, 30], 1)'), [20, 30])).toBeTruthy()

      // Extrapolation beyond end (t>1)
      expect(deepEqual(runLin('lin:lerp([0, 0], [10, 10], 2)'), [20, 20])).toBeTruthy()

      // Extrapolation before start (t<0)
      expect(deepEqual(runLin('lin:lerp([10, 10], [20, 20], -1)'), [0, 0])).toBeTruthy()

      // 3D vectors
      expect(deepEqual(runLin('lin:lerp([0, 0, 0], [10, 20, 30], 0.5)'), [5, 10, 15])).toBeTruthy()

      // 1D vectors
      expect(deepEqual(runLin('lin:lerp([5], [10], 0.5)'), [7.5])).toBeTruthy()

      // Empty vectors (should still work with empty arrays)
      expect(deepEqual(runLin('lin:lerp([], [], 0.5)'), [])).toBeTruthy()

      // Different dimensions (should throw an error)
      expect(() => runLin('lin:lerp([1, 2], [1, 2, 3], 0.5)')).toThrowError(LitsError)
    })
  })

  describe('lin:reflect', () => {
    it('should reflect a vector about a normal vector', () => {
      // Basic case - reflection in 2D
      expect(deepEqual(runLin('lin:reflect([1, -1], [0, 1])'), [1, 1])).toBeTruthy()

      // 3D reflection
      expect(deepEqual(runLin('lin:reflect([1, 2, 3], [0, 1, 0])'), [1, -2, 3])).toBeTruthy()

      // Reflection with unnormalized normal vector
      expect(deepEqual(runLin('lin:reflect([1, 0], [0, 2])'), [1, 0])).toBeTruthy()

      // 45-degree reflection
      const normal45 = 'lin:normalize([1, 1])'
      expect(deepEqual(runLin('lin:reflect([1, 0], [1, 1])'), [0, -1])).toBeTruthy()
      // expect(runLin('lin:reflect([1, 0], [1, 1])')).toEqual([0, -1])
      expect(deepEqual(runLin(`lin:reflect([1, 0], ${normal45})`), [0, -1])).toBeTruthy()

      // 1D reflection
      expect(deepEqual(runLin('lin:reflect([5], [1])'), [-5])).toBeTruthy()
      expect(deepEqual(runLin('lin:reflect([5], [-1])'), [-5])).toBeTruthy()

      // Reflection of zero vector
      expect(deepEqual(runLin('lin:reflect([0, 0, 0], [0, 1, 0])'), [0, 0, 0])).toBeTruthy()

      // Empty vectors (should throw an error)
      expect(() => runLin('lin:reflect([], [0, 1])')).toThrowError(LitsError)

      // Different dimensions (should throw an error)
      expect(() => runLin('lin:reflect([1, 2], [0, 1, 0])')).toThrowError(LitsError)

      // Zero normal vector (should throw an error because normalization fails)
      expect(() => runLin('lin:reflect([1, 2], [0, 0])')).toThrowError(LitsError)
    })
  })

  describe('lin:refract', () => {
    it('should refract a vector through a surface with a normal and refractive index ratio', () => {
      // Basic case - refraction in 2D
      // Angle of incidence is 45 degrees, normal is [0, 1], eta is 0.75 (air to water)
      expect(deepEqual(runLin('lin:refract([1, -1], [0, 1], 0.75)'), [0.5303300858899106, -0.8477912478906585])).toBeTruthy()

      // 3D refraction
      expect(deepEqual(runLin('lin:refract([1, -1, 0], [0, 1, 0], 0.8)'), [0.565685424949238, -0.8246211251235321, 0])).toBeTruthy()

      // Total internal reflection (entering from denser medium)
      // With eta = 1.5 (water to air) and a steep enough angle
      expect(deepEqual(runLin('lin:refract([2, -1, 0], [0, 1, 0], 1.5)'), [2, -1, 0])).toBeTruthy()

      // zero vector
      expect(deepEqual(runLin('lin:refract([0, 0, 0], [0, 1, 0], 0.75)'), [0, 0, 0])).toBeTruthy()

      // Zero refraction (perpendicular incidence)
      expect(deepEqual(runLin('lin:refract([0, -1, 0], [0, 1, 0], 0.75)'), [0, -1, 0])).toBeTruthy()

      // Eta = 0 (non-physical)
      expect(() => deepEqual(runLin('lin:refract([1, -1, 0], [0, 1, 0], 0)'), [0, -1, 0])).toThrowError(LitsError)

      // Eta = 1 (no change in medium)
      const norm = 'lin:normalize([1, -1, 0])'
      expect(deepEqual(runLin(`lin:refract(${norm}, [0, 1, 0], 1)`), getUnit([1, -1, 0], undefined))).toBeTruthy()

      // 1D refraction
      expect(deepEqual(runLin('lin:refract([1], [1], 0.5)'), [-1])).toBeTruthy()

      // Empty vectors (should throw an error)
      expect(() => runLin('lin:refract([1, 1], [0, 0], 1.5)')).toThrowError(LitsError)

      // Empty vectors (should throw an error)
      expect(() => runLin('lin:refract([], [0, 1], 1.5)')).toThrowError(LitsError)

      // Different dimensions (should throw an error)
      expect(() => runLin('lin:refract([1, 2], [0, 1, 0], 1.5)')).toThrowError(LitsError)

      // Zero normal vector (should throw an error because normalization fails)
      expect(() => runLin('lin:refract([1, 2], [0, 0], 1.5)')).toThrowError(LitsError)
    })
  })

  describe('lin:dot', () => {
    it('should calculate the dot product of two vectors', () => {
      // Basic case
      expect(runLin('lin:dot([1, 2, 3], [4, 5, 6])')).toEqual(32)
      // Case with negative numbers
      expect(runLin('lin:dot([-1, -2, -3], [4, 5, 6])')).toEqual(-32)
      // Case with mixed numbers
      expect(runLin('lin:dot([1, -2, 3], [4, 5, -6])')).toEqual(-24)
      // Case with empty vectors
      expect(runLin('lin:dot([], [])')).toEqual(0)
      // Case with single element vectors
      expect(runLin('lin:dot([42], [1])')).toEqual(42)
      // Case with different lengths (should throw an error)
      expect(() => runLin('lin:dot([1, 2], [3])')).toThrowError('Vectors must be of the same length')
    })
  })
  describe('lin:cross', () => {
    it('should calculate the cross product of two vectors', () => {
      // Basic case
      expect(runLin('lin:cross([1, 2, 3], [4, 5, 6])')).toEqual([-3, 6, -3])
      // Case with negative numbers
      expect(runLin('lin:cross([-1, -2, -3], [4, 5, 6])')).toEqual([3, -6, 3])
      // Case with mixed numbers
      expect(runLin('lin:cross([1, -2, 3], [4, 5, -6])')).toEqual([-3, 18, 13])
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:cross([], [])')).toThrowError(LitsError)
      // Case with single element vectors (should throw an error)
      expect(() => runLin('lin:cross([42], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:normalize-minmax', () => {
    it('should normalize a vector using min-max normalization', () => {
      // Basic case with positive numbers
      expect(runLin('lin:normalize-minmax([1, 2, 3, 4, 5])')).toEqual([0, 0.25, 0.5, 0.75, 1])

      // Case with negative and positive numbers
      expect(runLin('lin:normalize-minmax([-10, 0, 10, 20, 30])')).toEqual([0, 0.25, 0.5, 0.75, 1])

      // Case with all same numbers (should handle division by zero)
      expect(runLin('lin:normalize-minmax([5, 5, 5, 5])')).toEqual([0, 0, 0, 0])

      // Empty array
      expect(runLin('lin:normalize-minmax([])')).toEqual([])

      // Single element array
      expect(runLin('lin:normalize-minmax([42])')).toEqual([0])

      // Decimal values
      expect(runLin('lin:normalize-minmax([1.5, 2.5, 3.5, 4.5])')).toEqual([0, 0.3333333333333333, 0.6666666666666666, 1])
    })
  })
  describe('lin:normalize-robust', () => {
    it('should normalize a vector using robust normalization', () => {
      // Basic case with positive numbers
      expect(runLin('lin:normalize-robust([1, 2, 2, 4, 15])')).toEqual([
        -0.6744907594765952,
        0,
        0,
        1.3489815189531904,
        8.768379873195737,
      ])

      // Case with all same numbers (should handle division by zero)
      expect(runLin('lin:normalize-robust([5, 5, 5, 5])')).toEqual([0, 0, 0, 0])

      // Empty array
      expect(runLin('lin:normalize-robust([])')).toEqual([])

      // Single element array
      expect(runLin('lin:normalize-robust([42])')).toEqual([0])
    })
  })
  describe('lin:normalize-zscore', () => {
    it('should normalize a vector using z-score normalization', () => {
      // Basic case with positive numbers
      expect(runLin('lin:normalize-zscore([1, 2, 2, 4, 15])')).toEqual([
        -0.7318526549827781,
        -0.5392598510399418,
        -0.5392598510399418,
        -0.15407424315426904,
        1.9644466002169305,
      ])

      // Case with all same numbers (should handle division by zero)
      expect(runLin('lin:normalize-zscore([5, 5, 5, 5])')).toEqual([0, 0, 0, 0])

      // Empty array
      expect(runLin('lin:normalize-zscore([])')).toEqual([])

      // Single element array
      expect(runLin('lin:normalize-zscore([42])')).toEqual([0])
    })
  })
  describe('lin:normalize-l1', () => {
    it('should normalize a vector using L1 normalization', () => {
      // Basic case with positive numbers
      expect(runLin('lin:normalize-l1([1, 2, 3, 4, 5])')).toEqual([0.06666666666666667, 0.13333333333333333, 0.2, 0.26666666666666666, 0.3333333333333333])

      // Case with all same numbers (should handle division by zero)
      expect(runLin('lin:normalize-l1([5, 5, 5, 5])')).toEqual([0.25, 0.25, 0.25, 0.25])

      expect(runLin('lin:normalize-l1([0, 0, 0, 0])')).toEqual([0, 0, 0, 0])

      // Empty array
      expect(runLin('lin:normalize-l1([])')).toEqual([])

      // Single element array
      expect(runLin('lin:normalize-l1([42])')).toEqual([1])
    })
  })
  describe('lin:normalize-l2', () => {
    it('should normalize a vector using L2 normalization', () => {
      // Basic case with positive numbers
      expect(runLin('lin:normalize-l2([1, 2, 3, 4, 5])')).toEqual([0.13483997249264842, 0.26967994498529685, 0.40451991747794525, 0.5393598899705937, 0.674199862463242])
      // Case with all same numbers (should handle division by zero)
      expect(runLin('lin:normalize-l2([5, 5, 5, 5])')).toEqual([0.5, 0.5, 0.5, 0.5])
      // Empty array
      expect(runLin('lin:normalize-l2([])')).toEqual([])
      // Single element array
      expect(runLin('lin:normalize-l2([42])')).toEqual([1])

      expect(() => runLin('lin:normalize-l2([0, 0, 0, 0])')).toThrow(LitsError)
    })
  })
  describe('lin:normalize-log', () => {
    it('should normalize a vector using log normalization', () => {
      // Basic case with positive numbers
      expect(runLin('lin:normalize-log([1, 2, 3, 4, 5])')).toEqual([0, 0.6931471805599453, 1.0986122886681096, 1.3862943611198906, 1.6094379124341003])

      // Case with all same numbers (should handle division by zero)
      expect(runLin('lin:normalize-log([5, 5, 5, 5])')).toEqual([0, 0, 0, 0])

      // Empty array
      expect(runLin('lin:normalize-log([])')).toEqual([])

      // Single element array
      expect(runLin('lin:normalize-log([42])')).toEqual([0])

      expect(() => runLin('lin:normalize-log([-42])')).toThrow(LitsError)
    })
  })
  describe('lin:angle', () => {
    it('should calculate the angle between two vectors', () => {
      // Basic case
      expect(runLin('lin:angle([1, 0], [0, 1])')).toEqual(Math.PI / 2)
      // Case with negative numbers
      expect(runLin('lin:angle([-1, 0], [0, -1])')).toEqual(Math.PI / 2)
      // Case with mixed numbers
      expect(runLin('lin:angle([1, -1], [-1, 1])')).toBeCloseTo(Math.PI)
      // Case with single element vectors
      expect(runLin('lin:angle([42], [1])')).toBeCloseTo(0)
      expect(runLin('lin:angle([42], [-1])')).toBeCloseTo(Math.PI)
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:angle([], [])')).toThrowError(LitsError)
      expect(() => runLin('lin:angle([0, 0], [1, 0])')).toThrowError(LitsError)
      expect(() => runLin('lin:angle([1, 0, 1], [1, 0])')).toThrowError(LitsError)
    })
  })
  describe('lin:projection', () => {
    it('should calculate the projection of one vector onto another', () => {
      // Basic case
      expect(runLin('lin:projection([1, 2], [3, 4])')).toEqual([1.32, 1.76])
      // Case with negative numbers
      expect(runLin('lin:projection([-1, -2], [-3, -4])')).toEqual([-1.32, -1.76])
      // Case with mixed numbers
      expect(runLin('lin:projection([1, -2], [-3, 4])')).toEqual([1.32, -1.76])
      // Case with single element vectors (should throw an error)
      expect(runLin('lin:projection([42], [1])')).toEqual([42])
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:projection([], [])')).toThrowError(LitsError)
      expect(() => runLin('lin:projection([1, 0], [0, 0])')).toThrowError(LitsError)
      expect(() => runLin('lin:projection([1, 0, 1], [1, 0])')).toThrowError(LitsError)
    })
  })
  describe('lin:orthogonal?', () => {
    it('should check if two vectors are orthogonal', () => {
      // Basic case
      expect(runLin('lin:orthogonal?([1, 0], [0, 1])')).toEqual(true)
      // Case with negative numbers
      expect(runLin('lin:orthogonal?([-1, 0], [0, -1])')).toEqual(true)
      // Case with mixed numbers
      expect(runLin('lin:orthogonal?([1, -1], [-1, 1])')).toEqual(false)
      // Case with single element vectors
      expect(runLin('lin:orthogonal?([42], [1])')).toEqual(false)
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:orthogonal?([], [])')).toThrowError(LitsError)

      expect(() => runLin('lin:orthogonal?([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:parallel?', () => {
    it('should check if two vectors are parallel', () => {
      // Basic case
      expect(runLin('lin:parallel?([1, 0], [2, 0])')).toEqual(true)
      // Case with negative numbers
      expect(runLin('lin:parallel?([-1, 0], [-2, 0])')).toEqual(true)
      // Case with mixed numbers
      expect(runLin('lin:parallel?([1, -1], [-2, 2])')).toEqual(false) // collinear though

      expect(runLin('lin:parallel?([0, 0], [-2, 2])')).toEqual(true)

      expect(runLin('lin:parallel?([2, -3, 4], [6, -9, 12])')).toEqual(true)
      expect(runLin('lin:parallel?([2, -3, 4], [-6, 9, -12])')).toEqual(false) // collinear though
      expect(runLin('lin:parallel?([2, -3, 4], [6, 9, 12])')).toEqual(false)
      // Case with single element vectors
      expect(runLin('lin:parallel?([42], [1])')).toEqual(true)
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:parallel?([], [])')).toThrowError(LitsError)

      expect(() => runLin('lin:parallel?([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:collinear?', () => {
    it('should check if two vectors are collinear', () => {
      // Basic case
      expect(runLin('lin:collinear?([1, 0], [2, 0])')).toEqual(true)
      expect(runLin('lin:collinear?([0, 1, 0], [0, 2, 0])')).toEqual(true)

      expect(runLin('lin:collinear?([0, 0], [2, 0])')).toEqual(true)
      expect(runLin('lin:collinear?([2, 0], [0, 0])')).toEqual(true)
      expect(runLin('lin:collinear?([2, 0], [2, 2])')).toEqual(false)
      // Case with negative numbers
      expect(runLin('lin:collinear?([-1, 0], [-2, 0])')).toEqual(true)
      // Case with mixed numbers
      expect(runLin('lin:collinear?([1, -1], [-2, 2])')).toEqual(true)

      expect(runLin('lin:collinear?([2, -3, 4], [6, -9, 12])')).toEqual(true)
      expect(runLin('lin:collinear?([2, -3, 4], [-6, 9, -12])')).toEqual(true)
      expect(runLin('lin:collinear?([2, -3, 4], [6, 9, 12])')).toEqual(false)
      // Case with single element vectors
      expect(runLin('lin:collinear?([42], [1])')).toEqual(true)
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:collinear?([], [])')).toThrowError(LitsError)

      expect(() => runLin('lin:collinear?([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:cosine-similarity', () => {
    it('should calculate the cosine similarity between two vectors', () => {
      // Basic case
      expect(runLin('lin:cosine-similarity([1, 0], [0, 1])')).toEqual(0)
      // Case with negative numbers
      expect(runLin('lin:cosine-similarity([-1, 0], [0, -1])')).toEqual(0)
      // Case with mixed numbers
      expect(runLin('lin:cosine-similarity([1, -1], [-1, 1])')).toBeCloseTo(-1)
      // Case with single element vectors
      expect(runLin('lin:cosine-similarity([42], [1])')).toBeCloseTo(1)
      // Case with zero vector
      expect(() => runLin('lin:cosine-similarity([0, 0, 0], [1, 2, 3])')).toThrowError(LitsError)
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:cosine-similarity([], [])')).toThrowError(LitsError)

      expect(() => runLin('lin:cosine-similarity([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:euclidean-distance', () => {
    it('should calculate the Euclidean distance between two vectors', () => {
      // Basic case
      expect(runLin('lin:euclidean-distance([1, 2], [4, 6])')).toEqual(5)
      // Basic case
      expect(runLin('lin:distance([1, 2], [1, 2])')).toEqual(0)
      // Case with negative numbers
      expect(runLin('lin:l2-distance([-1, -2], [-4, -6])')).toEqual(5)
      // Case with mixed numbers
      expect(runLin('lin:euclidean-distance([1, -2], [-4, 6])')).toBeCloseTo(9.433981132056603)
      // Case with single element vectors
      expect(runLin('lin:euclidean-distance([42], [1])')).toEqual(41)
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:euclidean-distance([], [])')).toThrowError(LitsError)
      expect(() => runLin('lin:distance([], [])')).toThrowError(LitsError)

      expect(() => runLin('lin:distance([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:euclidean-norm', () => {
    it('should calculate the L2 norm of a vector', () => {
      // Basic case
      expect(runLin('lin:euclidean-norm([3, 4])')).toEqual(5)
      // Case with negative numbers
      expect(runLin('lin:length([-3, -4])')).toEqual(5)
      // Case with mixed numbers
      expect(runLin('lin:l2-norm([3, -4])')).toEqual(5)
      // Case with single element vector
      expect(runLin('lin:length([42])')).toEqual(42)
      // Case with empty vector
      expect(() => runLin('lin:length([])')).toThrowError(LitsError)
    })
  })
  describe('lin:manhattan-distance', () => {
    it('should calculate the Manhattan distance between two vectors', () => {
      // Basic case
      expect(runLin('lin:manhattan-distance([1, 2], [4, 6])')).toEqual(7)
      // Basic case
      expect(runLin('lin:l1-distance([1, 2], [1, 2])')).toEqual(0)
      // Case with negative numbers
      expect(runLin('lin:cityblock-distance([-1, -2], [-4, -6])')).toEqual(7)
      // Case with mixed numbers
      expect(runLin('lin:manhattan-distance([1, -2], [-4, 6])')).toBeCloseTo(13)
      // Case with single element vectors
      expect(runLin('lin:manhattan-distance([42], [1])')).toEqual(41)
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:manhattan-distance([], [])')).toThrowError(LitsError)

      expect(() => runLin('lin:manhattan-distance([1, 2], [2])')).toThrowError(LitsError)
    })
  })
  describe('lin:manhattan-norm', () => {
    it('should calculate the L1 norm of a vector', () => {
      // Basic case
      expect(runLin('lin:l1-norm([1, 2, 3])')).toEqual(6)
      // Case with negative numbers
      expect(runLin('lin:manhattan-norm([-1, -2, -3])')).toEqual(6)
      // Case with mixed numbers
      expect(runLin('lin:cityblock-norm([1, -2, 3])')).toEqual(6)
      // Case with single element vector
      expect(runLin('lin:l1-norm([42])')).toEqual(42)
      // Case with empty vector
      expect(() => runLin('lin:l1-norm([])')).toThrowError(LitsError)
    })
  })
  describe('lin:hamming-distance', () => {
    it('should calculate the Hamming distance between two vectors', () => {
      // Basic case
      expect(runLin('lin:hamming-distance([1, 2, 3], [1, 2, 4])')).toEqual(1)
      // Basic case
      expect(runLin('lin:hamming-distance([1, 2, 3], [1, 2, 3])')).toEqual(0)
      // Case with negative numbers
      expect(runLin('lin:hamming-distance([-1, -2], [-4, -6])')).toEqual(2)
      // Case with mixed numbers
      expect(runLin('lin:hamming-distance([1, -2], [-4, 6])')).toEqual(2)
      // Case with single element vectors
      expect(runLin('lin:hamming-distance([42], [1])')).toEqual(1)
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:hamming-distance([], [])')).toThrowError(LitsError)
      expect(() => runLin('lin:hamming-distance([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:hamming-norm', () => {
    it('should calculate the Hamming norm of a vector', () => {
      // Basic case
      expect(runLin('lin:hamming-norm([1, 2, 3])')).toEqual(3)
      // Case with negative numbers
      expect(runLin('lin:hamming-norm([0, -2, -3])')).toEqual(2)
      // Case with mixed numbers
      expect(runLin('lin:hamming-norm([0, 0, 0])')).toEqual(0)
      // Case with single element vector
      expect(runLin('lin:hamming-norm([42])')).toEqual(1)
      // Case with empty vector
      expect(() => runLin('lin:hamming-norm([])')).toThrowError(LitsError)
    })
  })
  describe('lin:chebyshev-distance', () => {
    it('should calculate the Chebyshev distance between two vectors', () => {
      // Basic case
      expect(runLin('lin:chebyshev-distance([1, 2], [4, 6])')).toEqual(4)
      // Basic case
      expect(runLin('lin:chebyshev-distance([1, 2], [1, 2])')).toEqual(0)
      // Case with negative numbers
      expect(runLin('lin:chebyshev-distance([-1, -2], [-4, -6])')).toEqual(4)
      // Case with mixed numbers
      expect(runLin('lin:chebyshev-distance([1, -2], [-4, 6])')).toBeCloseTo(8)
      // Case with single element vectors
      expect(runLin('lin:chebyshev-distance([42], [1])')).toEqual(41)
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:chebyshev-distance([], [])')).toThrowError(LitsError)
      expect(() => runLin('lin:chebyshev-distance([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:chebyshev-norm', () => {
    it('should calculate the Chebyshev norm of a vector', () => {
      // Basic case
      expect(runLin('lin:chebyshev-norm([1, 2, 3])')).toEqual(3)
      // Case with negative numbers
      expect(runLin('lin:chebyshev-norm([-1, -2, -3])')).toEqual(3)
      // Case with mixed numbers
      expect(runLin('lin:chebyshev-norm([1, -2, 3])')).toEqual(3)
      // Case with single element vector
      expect(runLin('lin:chebyshev-norm([42])')).toEqual(42)
      // Case with empty vector
      expect(() => runLin('lin:chebyshev-norm([])')).toThrowError(LitsError)
    })
  })
  describe('lin:minkowski-distance', () => {
    it('should calculate the Minkowski distance between two vectors', () => {
      // Basic case
      expect(runLin('lin:minkowski-distance([1, 2], [4, 6], 3)')).toBeCloseTo(4.5)
      // Basic case
      expect(runLin('lin:minkowski-distance([1, 2], [4, 6], 1)')).toBeCloseTo(7)
      // Basic case
      expect(runLin('lin:minkowski-distance([1, 2], [4, 6], 2)')).toBeCloseTo(5)
      // Basic case
      expect(runLin('lin:minkowski-distance([1, 2], [4, 6], 0.5)')).toBeCloseTo(13.93)
      // Basic case
      expect(runLin('lin:minkowski-distance([1, 2], [1, 2], 3)')).toEqual(0)
      // Case with negative numbers
      expect(runLin('lin:minkowski-distance([-1, -2], [-4, -6], 3)')).toBeCloseTo(4.5)
      // Case with mixed numbers
      expect(runLin('lin:minkowski-distance([1, -2], [-4, 6], 3)')).toBeCloseTo(8.6)
      // Case with single element vectors
      expect(runLin('lin:minkowski-distance([42], [1], 3)')).toBeCloseTo(41)
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:minkowski-distance([], [], 3)')).toThrowError(LitsError)
      // Case with invalid p value (should throw an error)
      expect(() => runLin('lin:minkowski-distance([1, 2], [4, 6], 0)')).toThrowError(LitsError)
      expect(() => runLin('lin:minkowski-distance([1, 2], [4, 6], -1)')).toThrowError(LitsError)
      expect(() => runLin('lin:minkowski-distance([1, 2, 3], [4, 6], 2)')).toThrowError(LitsError)
    })
  })
  describe('lin:minkowski-norm', () => {
    it('should calculate the Minkowski norm of a vector', () => {
      // Basic case
      expect(runLin('lin:minkowski-norm([1, 2, 3], 3)')).toBeCloseTo(3.3019272488946263)
      // Basic case
      expect(runLin('lin:minkowski-norm([1, 2, 3], 1)')).toEqual(6)
      // Basic case
      expect(runLin('lin:minkowski-norm([1, 2, 3], 2)')).toBeCloseTo(3.7416573867739413)
      // Basic case
      expect(runLin('lin:minkowski-norm([1, 2, 3], 0.5)')).toBeCloseTo(17.19)
      // Case with negative numbers
      expect(runLin('lin:minkowski-norm([-1, -2, -3], 3)')).toBeCloseTo(3.3019272488946263)
      // Case with mixed numbers
      expect(runLin('lin:minkowski-norm([1, -2, 3], 3)')).toBeCloseTo(3.3019272488946263)
      // Case with single element vector
      expect(runLin('lin:minkowski-norm([42], 3)')).toBeCloseTo(42)
      // Case with empty vector (should throw an error)
      expect(() => runLin('lin:minkowski-norm([], 3)')).toThrowError(LitsError)
      // Case with invalid p value (should throw an error)
      expect(() => runLin('lin:minkowski-distance([1, 2], 0)')).toThrowError(LitsError)
      expect(() => runLin('lin:minkowski-distance([1, 2], -1)')).toThrowError(LitsError)
    })
  })
  describe('lin:cov', () => {
    it('should calculate the covariance between two vectors', () => {
      // Basic case
      expect(runLin('lin:cov([1, 2, 3], [4, 5, 6])')).toBeCloseTo(2 / 3)
      // Case with negative numbers
      expect(runLin('lin:cov([-1, -2, -3], [-4, -5, -6])')).toBeCloseTo(2 / 3)
      // Case with mixed numbers
      expect(runLin('lin:cov([1, -2, 3], [-4, 5, -6])')).toBeCloseTo(-9.56)
      // Case with single element vectors
      expect(runLin('lin:cov([42], [1])')).toBe(0)
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:cov([], [])')).toThrowError(LitsError)
      expect(() => runLin('lin:cov([1, 2], [1])')).toThrowError(LitsError)
    })
  })
  describe('lin:corr', () => {
    it('should calculate the correlation between two vectors', () => {
      // Basic case
      expect(runLin('lin:corr([1, 2, 3], [4, 5, 6])')).toBeCloseTo(1)
      // Case with negative numbers
      expect(runLin('lin:corr([-1, -2, -3], [-4, -5, -6])')).toBeCloseTo(1)
      // Case with mixed numbers
      expect(runLin('lin:corr([1, -2, 3], [-4, 5, -6])')).toBeCloseTo(-0.972)
      // Case with single element vectors (should throw an error)
      expect(() => runLin('lin:corr([42], [1])')).toThrowError(LitsError)
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:corr([], [])')).toThrowError(LitsError)
      expect(() => runLin('lin:corr([1, 2], [2])')).toThrowError(LitsError)
    })
  })
  describe('lin:spearman-corr', () => {
    it('should calculate the Spearman correlation between two vectors', () => {
      expect(runLin('lin:spearman-corr([1, 2, 3], [4, 5, 6])')).toBeCloseTo(1)
      expect(runLin('lin:spearman-rho([1, 2, 3, 4, 5], [1, 2, 3, 4, 5])')).toBeCloseTo(1)
      expect(runLin('lin:spearman-corr([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])')).toBeCloseTo(-1)
      expect(runLin('lin:spearman-rho([1, 2, 3, 4, 5], [5, 2, 4, 1, 3])')).toBeCloseTo(-0.5)
      expect(runLin('lin:spearman-corr([1, 2, 3, 4, 100], [1, 2, 3, 4, 5])')).toBeCloseTo(1)

      // Case with single element vectors (should throw an error)
      expect(() => runLin('lin:spearman-corr([42], [1])')).toThrowError(LitsError)
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:spearman-corr([], [])')).toThrowError(LitsError)
      expect(() => runLin('lin:spearman-corr([1, 2], [1])')).toThrowError(LitsError)
      expect(() => runLin('lin:spearman-corr([1, 1], [1, 2])')).toThrowError(LitsError)
    })
  })
  describe('lin:pearson-corr', () => {
    it('should calculate the Pearson correlation between two vectors', () => {
      expect(runLin('lin:pearson-corr([1, 2, 3], [4, 5, 6])')).toBeCloseTo(1)
      expect(runLin('lin:pearson-corr([1, 2, 3, 4, 5], [1, 2, 3, 4, 5])')).toBeCloseTo(1)
      expect(runLin('lin:pearson-corr([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])')).toBeCloseTo(-1)
      expect(runLin('lin:pearson-corr([1, 2, 3, 4, 5], [5, 2, 4, 1, 3])')).toBeCloseTo(-0.5)
      expect(runLin('lin:pearson-corr([1, 2, 3, 4, 100], [1, 2, 3, 4, 5])')).toBeCloseTo(0.725)

      // Case with single element vectors (should throw an error)
      expect(() => runLin('lin:pearson-corr([42], [1])')).toThrowError(LitsError)
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:pearson-corr([], [])')).toThrowError(LitsError)
      expect(() => runLin('lin:pearson-corr([1, 2], [1])')).toThrowError(LitsError)
      expect(() => runLin('lin:pearson-corr([1, 1], [1, 2])')).toThrowError(LitsError)
    })
  })

  describe('lin:kendall-tau', () => {
    it('should handle all my test cases', () => {
      // Perfect positive correlation
      expect(runLin('lin:kendall-tau([1, 2, 3, 4, 5], [10, 20, 30, 40, 50])')).toBeCloseTo(1.0)

      // Perfect negative correlation
      expect(runLin('lin:kendall-tau([1, 2, 3, 4, 5], [50, 40, 30, 20, 10])')).toBeCloseTo(-1.0)

      // No correlation (random data)
      expect(runLin('lin:kendall-tau([1, 2, 3, 4, 5], [3, 5, 1, 4, 2])')).toBeCloseTo(-0.2)

      // Ties in vector A
      expect(runLin('lin:kendall-tau([1, 2, 2, 4, 5], [10, 20, 30, 40, 50])')).toBeCloseTo(0.9487)

      // Ties in vector B
      expect(runLin('lin:kendall-tau([1, 2, 3, 4, 5], [10, 20, 20, 40, 50])')).toBeCloseTo(0.9487)

      // Ties in both vectors
      expect(runLin('lin:kendall-tau([1, 2, 2, 4, 5], [10, 20, 20, 40, 50])')).toBeCloseTo(1.0)

      // Manual verification (small example with known result)
      expect(runLin('lin:kendall-tau([1, 2, 3, 4], [2, 1, 4, 3])')).toBeCloseTo(0.3333)

      // Very close values that should be considered ties with default epsilon
      expect(runLin('lin:kendall-tau([1.0000001, 1.0000002, 2, 3], [5, 6, 7, 8])')).toBeCloseTo(1.0)

      // Single pair
      expect(runLin('lin:kendall-tau([1, 2], [3, 4])')).toBeCloseTo(1.0)

      // More complex rank example
      expect(runLin('lin:kendall-tau([1, 2, 3, 4, 5, 6], [2, 3, 1, 5, 6, 4])')).toBeCloseTo(0.4667)

      // Another rank example
      expect(runLin('lin:kendall-tau([1, 2, 3, 4, 5, 6, 7, 8], [3, 4, 1, 2, 7, 8, 5, 6])')).toBeCloseTo(0.4286)

      // Simple counterexample
      expect(runLin('lin:kendall-tau([1, 3, 2], [1, 2, 3])')).toBeCloseTo(0.3333)

      // All ties in vector A
      expect(() => runLin('lin:kendall-tau([5, 5, 5, 5], [1, 2, 3, 4])')).toThrow('Not enough data to calculate Kendall\'s Tau')

      // All ties in vector B
      expect(() => runLin('lin:kendall-tau([1, 2, 3, 4], [7, 7, 7, 7])')).toThrow(LitsError)

      // Mixed ties and values
      expect(runLin('lin:kendall-tau([1, 1, 3, 4], [5, 5, 7, 8])')).toBeCloseTo(1.0)

      // Another mixed case
      expect(runLin('lin:kendall-tau([1, 2, 3, 3], [4, 5, 6, 6])')).toBeCloseTo(1.0)

      // Partial correlation
      expect(runLin('lin:kendall-tau([1, 2, 3, 4, 5], [1, 3, 2, 5, 4])')).toBeCloseTo(0.6)

      // Empty arrays (should throw an error)
      expect(() => runLin('lin:kendall-tau([], [])')).toThrow(LitsError)

      // Boundary case: arrays of length 1
      expect(() => runLin('lin:kendall-tau([1], [2])')).toThrow(LitsError)
      expect(() => runLin('lin:kendall-tau([1, 2, 3], [1, 2])')).toThrow(LitsError)

      // All tied pairs (corner case)
      expect(() => runLin('lin:kendall-tau([1, 1, 1], [2, 2, 2])')).toThrow('Not enough data to calculate Kendall\'s Tau')

      // Inverse relationship with some noise
      expect(runLin('lin:kendall-tau([1, 2, 3, 4, 5], [5, 3, 4, 2, 1])')).toBeCloseTo(-0.8)

      // Test with fractional values
      expect(runLin('lin:kendall-tau([1.5, 2.3, 3.7, 4.2], [5.1, 4.8, 3.2, 2.9])')).toBeCloseTo(-1.0)
    })
  })
  describe('lin:autocorrelation', () => {
    it('should calculate the autocorrelation of a vector', () => {
      // Basic cases
      expect(runLin('lin:autocorrelation([1, 2, 3, 4, 5], 0)')).toBeCloseTo(1)
      expect(runLin('lin:autocorrelation([1, 2, 3, 4, 5], 1)')).toBeCloseTo(0.4)
      expect(runLin('lin:autocorrelation([1, 2, 3, 4, 5], -1)')).toBeCloseTo(0.4)
      expect(runLin('lin:autocorrelation([1, 2, 3, 4, 5], 0)')).toBeCloseTo(1)
      expect(runLin('lin:autocorrelation([1, 2, 3, 4, 5], 2)')).toBeCloseTo(-0.1)
      expect(runLin('lin:acf([1, 2, 3, 4, 5], 3)')).toBeCloseTo(-0.4)
      expect(runLin('lin:autocorrelation([1, 2, 3, 4, 5], 4)')).toBeCloseTo(-0.4)
      // Case with negative numbers
      expect(runLin('lin:autocorrelation([-1, -2, -3, -4, -5], 1)')).toBeCloseTo(0.4)
      // Case with mixed numbers
      expect(runLin('lin:autocorrelation([1, -2, 3, -4, 5], 2)')).toBeCloseTo(0.441)

      expect(runLin('lin:autocorrelation([1, 1, 1, 1, 1], 2)')).toBe(0)
      expect(runLin('lin:autocorrelation([1, 1, 1, 1, 1], 0)')).toBe(1)

      // Case with zero lag
      expect(() => runLin('lin:autocorrelation([42], 0)')).toThrowError(LitsError)
      // Case with empty vectors (should throw an error)
      expect(() => runLin('lin:autocorrelation([], 0)')).toThrowError(LitsError)
    })
  })
  describe('lin:cross-correlation', () => {
    it('should calculate the cross correlation for two vectors and lag', () => {
      // Basic cases - identical vectors
      expect(runLin('lin:cross-correlation([1, 2, 3, 4, 5], [1, 2, 3, 4, 5], 0)')).toBeCloseTo(1)
      expect(runLin('lin:cross-correlation([1, 2, 3, 4, 5], [1, 2, 3, 4, 5], 1)')).toBeCloseTo(1)
      expect(runLin('lin:cross-correlation([1, 2, 3, 4, 5], [1, 2, 3, 4, 5], 2)')).toBeCloseTo(1)

      // Perfectly negatively correlated vectors
      expect(runLin('lin:cross-correlation([1, 2, 3, 4, 5], [5, 4, 3, 2, 1], 0)')).toBeCloseTo(-1)
      expect(runLin('lin:cross-correlation([1, 2, 3, 4, 5], [5, 4, 3, 2, 1], 1)')).toBeCloseTo(-1)

      expect(runLin('lin:cross-correlation([1, 1, 1], [1, 1, 1], 1)')).toBeCloseTo(1)

      // Similar patterns with offset
      expect(runLin('lin:cross-correlation([10, 12, 15, 10, 8, 15, 20, 25, 18, 15], [8, 10, 14, 9, 7, 13, 19, 23, 17, 14], 0)')).toBeCloseTo(0.995)
      expect(runLin('lin:cross-correlation([10, 12, 15, 10, 8, 15, 20, 25, 18, 15], [8, 10, 14, 9, 7, 13, 19, 23, 17, 14], 1)')).toBeCloseTo(0.614)

      // Uncorrelated vectors
      expect(runLin('lin:cross-correlation([1, 2, 3, 4, 5], [5, 1, 3, 2, 4], 0)')).toBeCloseTo(-0.1)
      expect(runLin('lin:cross-correlation([1, 2, 3, 4, 5], [5, 1, 3, 2, 4], 1)')).toBeCloseTo(0.8)

      // Constant vectors (edge cases)
      expect(runLin('lin:cross-correlation([5, 5, 5, 5, 5], [5, 5, 5, 5, 5], 0)')).toBeCloseTo(1)
      expect(runLin('lin:cross-correlation([5, 5, 5, 5, 5], [10, 10, 10, 10, 10], 0)')).toBeCloseTo(0)

      // Sine and cosine waves (phase-shifted signals)
      const sine = [0, 0.588, 0.951, 0.951, 0.588, 0, -0.588, -0.951, -0.951, -0.588]
      const cosine = [1, 0.809, 0.309, -0.309, -0.809, -1, -0.809, -0.309, 0.309, 0.809]
      expect(runLin(`lin:cross-correlation([${sine.join(', ')}], [${sine.join(', ')}], 0)`)).toBeCloseTo(1)
      expect(runLin(`lin:cross-correlation([${sine.join(', ')}], [${cosine.join(', ')}], 0)`)).toBeCloseTo(0)
      expect(runLin(`lin:cross-correlation([${sine.join(', ')}], [${sine.join(', ')}], 5)`)).toBeCloseTo(-1)

      // Negative lags
      expect(runLin('lin:cross-correlation([1, 2, 3, 4, 5], [1, 2, 3, 4, 5], -1)')).toBeCloseTo(1)
      expect(runLin('lin:cross-correlation([1, 2, 3, 4, 5], [5, 4, 3, 2, 1], -1)')).toBeCloseTo(-1)

      expect(() => runLin('lin:cross-correlation([1, 2, 3, 4, 5, 6], [5, 4, 3, 2, 1], -1)')).toThrow(LitsError)
      expect(() => runLin('lin:cross-correlation([1], [5], -1)')).toThrow(LitsError)
    })
  })
  describe('lin:rref', () => {
    it('should calculate the reduced row echelon form of a matrix', () => {
      // Basic case
      expect(runLin('lin:rref([[1, 2], [3, 4]])')).toEqual([[1, 0], [0, 1]])
      expect(runLin('lin:rref([[3, 4], [2, 1]])')).toEqual([[1, 0], [0, 1]])
      // Case with negative numbers
      expect(runLin('lin:rref([[-1, -2], [-3, -4]])')).toEqual([[1, 0], [0, 1]])
      // Case with mixed numbers
      expect(runLin('lin:rref([[1, -2], [-3, 4]])')).toEqual([[1, 0], [0, 1]])
      // Case with single element matrix
      expect(runLin('lin:rref([[42]])')).toEqual([[1]])

      expect(runLin(`lin:rref([
  [0, 2, 3],
  [1, 2, 3],
  [4, 5, 6]
])`)).toEqual([[1, 0, 0], [0, 1, 0], [0, 0, 1]])
      // Case with empty matrix (should throw an error)
      expect(() => runLin('lin:rref([])')).toThrowError(LitsError)
    })
  })
  describe('lin:solve', () => {
    it('should solve systems of linear equations', () => {
      // Unique solution: 2x + 3y = 8, x - y = 2 => x = 2, y = 4/3
      expect(runLin('lin:solve([[2, 3], [1, -1]], [8, 2])')).toEqual([2.8, 0.8])

      // Identity matrix: x = b
      expect(runLin('lin:solve([[1, 0, 0], [0, 1, 0], [0, 0, 1]], [5, -2, 3])')).toEqual([5, -2, 3])

      // Inconsistent system: x + y = 1, x + y = 2 (no solution)
      expect(runLin('lin:solve([[1, 1], [1, 1]], [1, 2])')).toBeNull()

      // Upper triangular system
      expect(runLin('lin:solve([[1, 2, 3], [0, 4, 5], [0, 0, 6]], [14, 23, 18])')).toEqual([1, 2, 3])

      // Lower triangular system
      expect(runLin('lin:solve([[2, 0, 0], [3, 1, 0], [4, 5, 6]], [4, 5, 38])')).toEqual([2, -1, 35 / 6])

      // Practical example: mixture problem
      // 0.3x + 0.5y = 0.35 (blend percentage)
      // x + y = 100 (total amount)
      expect((runLin('lin:solve([[0.3, 0.5], [1, 1]], [35, 100])') as number[])[0]).toBeCloseTo(75)
      expect((runLin('lin:solve([[0.3, 0.5], [1, 1]], [35, 100])') as number[])[1]).toBeCloseTo(25)

      // Economic equilibrium example:
      // 10 - 2p + q = 5 (demand for product 1)
      // 15 + p - 3q = 5 (demand for product 2)
      expect(runLin('lin:solve([[-2, 1], [1, -3]], [-5, -10])')).toEqual([5, 5])

      // Small values / precision test
      expect(runLin('lin:solve([[1, 1], [2, 2]], [5, 7])')).toBeNull()

      expect(() => runLin('lin:solve([[1, 1], [2, 2]], [5, 7, 3])')).toThrow(LitsError)

      // Larger system (4×4)
      expect(runLin(`lin:solve([
        [2, 1, -1, 1], 
        [4, 5, -3, 2], 
        [6, -2, 5, -3], 
        [8, 3, 2, 4]
      ], [5, 10, 2, 17])`)).toEqual([1.519607843137255, -0.17647058823529416, -0.5294117647058822, 1.6078431372549018])
    })
  })
  describe('lin:to-polar', () => {
    it('should convert Cartesian coordinates to polar coordinates', () => {
      // Basic case
      expect(runLin('lin:to-polar([3, 4])')).toEqual([5, 0.9272952180016122])
      // Case with negative numbers
      expect(runLin('lin:to-polar([-3, -4])')).toEqual([5, -2.214297435588181])
      // Case with mixed numbers
      expect(runLin('lin:to-polar([3, -4])')).toEqual([5, -0.9272952180016122])

      expect(runLin('lin:to-polar([0, 0])')).toEqual([0, 0])
      // Case with single element vector (should throw an error)
      expect(() => runLin('lin:to-polar([42])')).toThrowError(LitsError)
      // Case with empty vector (should throw an error)
      expect(() => runLin('lin:to-polar([])')).toThrowError(LitsError)
    })
  })
  describe('lin:from-polar', () => {
    it('should convert polar coordinates to Cartesian coordinates', () => {
      // Basic case
      expect(runLin('lin:from-polar([5, PI / 4])')).toEqual([
        3.5355339059327378,
        3.5355339059327373,
      ])
      // Case with negative numbers
      expect(runLin('lin:from-polar([5, -PI / 8])')).toEqual([
        4.619397662556434,
        -1.913417161825449,
      ])

      expect(runLin('lin:from-polar([0, -PI / 8])')).toEqual([
        0,
        0,
      ])
      // Case with single element vector (should throw an error)
      expect(() => runLin('lin:from-polar([42])')).toThrowError(LitsError)
      // Case with empty vector (should throw an error)
      expect(() => runLin('lin:from-polar([])')).toThrowError(LitsError)
    })
  })
})
