import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'
import { LitsError } from '../../../../../errors'

const lits = new Lits()
describe('matrix', () => {
  describe('mat:mul', () => {
    it('should perform matrix multiplication of two matrices', () => {
      expect(lits.run('mat:mul([[1, 2], [3, 4]], [[5, 6], [7, 8]])')).toEqual([[19, 22], [43, 50]])
      expect(lits.run('mat:mul([[1, 2, 3], [4, 5, 6]], [[7, 8], [9, 10], [11, 12]])')).toEqual([[58, 64], [139, 154]])
      expect(lits.run('mat:mul([[2]], [[3]])')).toEqual([[6]])
      expect(() => lits.run('mat:mul([[2, 1]], [[3, 4]])')).toThrow(LitsError)
    })
  })
  describe('mat:det', () => {
    it('should return the determinant of a matrix', () => {
      expect(lits.run('mat:det([[1, 2], [3, 4]])')).toEqual(-2)
      expect(lits.run('mat:det([[1, 2, 3], [4, 5, 6], [7, 8, 9]])')).toBeCloseTo(0, 10)
    })
  })
  describe('mat:inv', () => {
    it('should return the inverse of a matrix', () => {
      expect(lits.run('mat:inv([[1, 2], [3, 4]])')).toEqual([[-2, 1], [1.5, -0.5]])
      expect(lits.run('mat:inv([[2, 3], [5, 7]])')).toEqual([[-7, 3], [5, -2]])
    })
    it('should throw an error for non-invertible matrices', () => {
      expect(() => lits.run('mat:inv([[1, 2], [2, 4]])')).toThrow(LitsError)
    })
  })
  describe('mat:adj', () => {
    it('should return the adjugate of a matrix', () => {
      expect(lits.run('mat:adj([[1, 2], [3, 4]])')).toEqual([[4, -2], [-3, 1]])
      expect(lits.run('mat:adj([[2, 3], [5, 7]])')).toEqual([[7, -3], [-5, 2]])
      expect(lits.run('mat:adj([[1, 2, 3], [4, 5, 6], [7,8,9]])')).toEqual([[-3, 6, -3], [6, -12, 6], [-3, 6, -3]])
    })
  })
  describe('mat:cofactor', () => {
    it('should return the cofactor of a matrix', () => {
      expect(lits.run('mat:cofactor([[1, 2], [3, 4]])')).toEqual([[4, -3], [-2, 1]])
    })
  })
  describe('mat:minor', () => {
    it('should return the minor of a matrix', () => {
      expect(lits.run('mat:minor([[1, 2], [3, 4]], 0, 1)')).toEqual([[3]])
      expect(lits.run('mat:minor([[1, 2, 3], [4, 5, 6], [7, 8, 9]], 1, 0)')).toEqual([[2, 3], [8, 9]])
      expect(lits.run('mat:minor([[1, 2, 3], [4, 5, 6], [7, 8, 9]], 2, 2)')).toEqual([[1, 2], [4, 5]])
    })
  })
  describe('mat:trace', () => {
    it('should return the trace of a matrix', () => {
      expect(lits.run('mat:trace([[1, 2], [3, 4]])')).toEqual(5)
      expect(lits.run('mat:trace([[1, 0, 0], [0, 1, 0], [0, 0, 1]])')).toEqual(3)
    })
  })
  describe('mat:symmetric?', () => {
    it('should return true for symmetric matrices', () => {
      expect(lits.run('mat:symmetric?([[1, 2], [2, 1]])')).toEqual(true)
      expect(lits.run('mat:symmetric?([[1, 0], [0, 1]])')).toEqual(true)
      expect(lits.run('mat:symmetric?([[1, 0], [0, 2]])')).toEqual(true)
    })
    it('should return false for non-symmetric matrices', () => {
      expect(lits.run('mat:symmetric?([[1, 2], [3, 4]])')).toEqual(false)
    })
    it('should return false for non-square matrices', () => {
      expect(lits.run('mat:symmetric?([[1, 2, 3], [4, 5, 6]])')).toEqual(false)
      expect(lits.run('mat:symmetric?([[1, 2], [3, 4], [5, 6]])')).toEqual(false)
    })
  })
  describe('mat:triangular?', () => {
    it('should return true for upper triangular matrices', () => {
      expect(lits.run('mat:triangular?([[1, 2], [0, 3]])')).toEqual(true)
      expect(lits.run('mat:triangular?([[1, 2, 3], [0, 4, 5], [0, 0, 6]])')).toEqual(true)
      expect(lits.run('mat:triangular?([[1, 2], [0, -3]])')).toEqual(true)
    })
    it('should return true for lower triangular matrices', () => {
      expect(lits.run('mat:triangular?([[1, 0], [2, 3]])')).toEqual(true)
      expect(lits.run('mat:triangular?([[1, 0, 0], [2, 3, 0], [4, 5, 6]])')).toEqual(true)
    })
    it('should return false for non-triangular matrices', () => {
      expect(lits.run('mat:triangular?([[1, 2], [3, 4]])')).toEqual(false)
    })
    it('should return false for non-square matrices', () => {
      expect(lits.run('mat:triangular?([[1, 2], [3, 4], [5, 6]])')).toEqual(false)
      expect(lits.run('mat:triangular?([[1], [2]])')).toEqual(false)
    })
  })
  describe('mat:upper-triangular?', () => {
    it('should return true for upper triangular matrices', () => {
      expect(lits.run('mat:upper-triangular?([[1, 2], [0, 3]])')).toEqual(true)
      expect(lits.run('mat:upper-triangular?([[1, 2, 3], [0, 4, 5], [0, 0, 6]])')).toEqual(true)
      expect(lits.run('mat:upper-triangular?([[1, 2], [0, -3]])')).toEqual(true)
    })
    it('should return false for lower triangular matrices', () => {
      expect(lits.run('mat:upper-triangular?([[1, 0], [2, 3]])')).toEqual(false)
      expect(lits.run('mat:upper-triangular?([[1, 0, 0], [2, 3, 0], [4, 5, 6]])')).toEqual(false)
    })
    it('should return false for non-square matrices', () => {
      expect(lits.run('mat:upper-triangular?([[1], [2]])')).toEqual(false)
    })
  })
  describe('mat:lower-triangular?', () => {
    it('should return true for lower triangular matrices', () => {
      expect(lits.run('mat:lower-triangular?([[1, 0], [2, 3]])')).toEqual(true)
      expect(lits.run('mat:lower-triangular?([[1, 0, 0], [2, 3, 0], [4, 5, 6]])')).toEqual(true)
    })
    it('should return false for upper triangular matrices', () => {
      expect(lits.run('mat:lower-triangular?([[1, 2], [0, 3]])')).toEqual(false)
      expect(lits.run('mat:lower-triangular?([[1, 2, 3], [0, 4, 5], [0, 0, 6]])')).toEqual(false)
    })
    it('should return false for non-square matrices', () => {
      expect(lits.run('mat:lower-triangular?([[1], [2]])')).toEqual(false)
    })
  })
  describe('mat:diagonal?', () => {
    it('should return true for diagonal matrices', () => {
      expect(lits.run('mat:diagonal?([[1, 0], [0, 2]])')).toEqual(true)
      expect(lits.run('mat:diagonal?([[3, 0, 0], [0, 4, 0], [0, 0, 5]])')).toEqual(true)
    })
    it('should return false for non-diagonal matrices', () => {
      expect(lits.run('mat:diagonal?([[1, 2], [3, 4]])')).toEqual(false)
    })
    it('should return false for non-square matrices', () => {
      expect(lits.run('mat:diagonal?([[1], [2]])')).toEqual(false)
    })
  })
  describe('mat:square?', () => {
    it('should return true for square matrices', () => {
      expect(lits.run('mat:square?([[1, 2], [3, 4]])')).toEqual(true)
      expect(lits.run('mat:square?([[1, 2, 3], [4, 5, 6], [7, 8, 9]])')).toEqual(true)
    })
    it('should return false for non-square matrices', () => {
      expect(lits.run('mat:square?([[1, 2], [3, 4], [5, 6]])')).toEqual(false)
      expect(lits.run('mat:square?([[1], [2]])')).toEqual(false)
    })
  })
  describe('mat:orthogonal?', () => {
    it('should return true for orthogonal matrices', () => {
      expect(lits.run('mat:orthogonal?([[1, 0], [0, 1.00000000001]])')).toEqual(true)
      expect(lits.run('mat:orthogonal?([[0, 1], [-1, 0]])')).toEqual(true)
      expect(lits.run('mat:orthogonal?([[0, 0], [0, 0]])')).toEqual(false)
    })
    it('should return false for non-orthogonal matrices', () => {
      expect(lits.run('mat:orthogonal?([[1, 2], [3, 4]])')).toEqual(false)
    })
    it('should return false for non-square matrices', () => {
      expect(lits.run('mat:orthogonal?([[1], [2]])')).toEqual(false)
    })
  })
  describe('mat:identity?', () => {
    it('should return true for identity matrices', () => {
      expect(lits.run('mat:identity?([[1, 0], [0, 1]])')).toEqual(true)
      expect(lits.run('mat:identity?([[1, 0, 0], [0, 1, 0], [0, 0, 1]])')).toEqual(true)
    })
    it('should return false for non-identity matrices', () => {
      expect(lits.run('mat:identity?([[1, 2], [3, 4]])')).toEqual(false)
    })
    it('should return false for non-square matrices', () => {
      expect(lits.run('mat:identity?([[1, 2]])')).toEqual(false)
    })
  })
  describe('mat:invertible?', () => {
    it('should return true for invertible matrices', () => {
      expect(lits.run('mat:invertible?([[1, 2], [3, 4]])')).toEqual(true)
    })
    it('should return false for non-invertible matrices', () => {
      expect(lits.run('mat:invertible?([[1, 2], [2, 4]])')).toEqual(false)
      expect(lits.run('mat:invertible?([[0, 0], [0, 0]])')).toEqual(false)
    })
    it('should return false for non-square matrices', () => {
      expect(lits.run('mat:invertible?([[1], [2]])')).toEqual(false)
    })
  })
  describe('mat:hilbert', () => {
    it('should return a Hilbert matrix of given size', () => {
      expect(lits.run('mat:hilbert(3)')).toEqual([
        [1, 0.5, 0.3333333333333333],
        [0.5, 0.3333333333333333, 0.25],
        [0.3333333333333333, 0.25, 0.2],
      ])
      expect(lits.run('mat:hilbert(2)')).toEqual([
        [1, 0.5],
        [0.5, 0.3333333333333333],
      ])
    })
    it('should throw an error for non-positive integer sizes', () => {
      expect(() => lits.run('mat:hilbert(-1)')).toThrow(LitsError)
      expect(() => lits.run('mat:hilbert(0)')).toThrow(LitsError)
    })
  })
  describe('mat:vandermonde', () => {
    it('should return a Vandermonde matrix of given size', () => {
      expect(lits.run('mat:vandermonde([1, 2, 3])')).toEqual([
        [1, 1, 1],
        [1, 2, 4],
        [1, 3, 9],
      ])
      expect(lits.run('mat:vandermonde([2, 3])')).toEqual([
        [1, 2],
        [1, 3],
      ])
    })
    it('should throw an error for non-positive integer sizes', () => {
      expect(() => lits.run('mat:vandermonde(-1)')).toThrow(LitsError)
      expect(() => lits.run('mat:vandermonde(0)')).toThrow(LitsError)
    })
  })
  describe('mat:band', () => {
    it('should return a band matrix of given size and bands', () => {
      expect(lits.run('mat:band(3, 1, 1)')).toEqual([
        [1, 1, 0],
        [1, 1, 1],
        [0, 1, 1],
      ])
      expect(lits.run('mat:band(4, 2, 2)')).toEqual([
        [1, 1, 1, 0],
        [1, 1, 1, 1],
        [1, 1, 1, 1],
        [0, 1, 1, 1],
      ])
    })
    it('should throw an error for non-positive integer sizes', () => {
      expect(() => lits.run('mat:band(-1)')).toThrow(LitsError)
      expect(() => lits.run('mat:band(0)')).toThrow(LitsError)
    })
    it('should throw an error for invalid bands', () => {
      expect(() => lits.run('mat:band(3, -1)')).toThrow(LitsError)
      expect(() => lits.run('mat:band(3, 4)')).toThrow(LitsError)
    })
  })
  describe('mat:banded?', () => {
    it('should return true for banded matrices', () => {
      expect(lits.run(`mat:banded?([
        [1, 1, 1, 0],
        [1, 1, 1, 1],
        [1, 1, 1, 1],
        [0, 1, 1, 1],
      ], 1, 1)`)).toEqual(false)
      expect(lits.run(`mat:banded?([
        [1, 1, 0],
        [1, 1, 1],
        [0, 1, 1],
      ], 1, 1)`)).toEqual(true)
      expect(lits.run(`mat:banded?([
        [1, 1, 1, 0],
        [1, 1, 1, 1],
        [1, 1, 1, 1],
        [0, 1, 1, 1],
      ], 2, 2)`)).toEqual(true)
    })
  })
  describe('mat:rank', () => {
    it('should calculate the rank of a matrix', () => {
      // Basic case
      expect(lits.run('mat:rank([[1, 2], [3, 4]])')).toEqual(2)
      // Case with negative numbers
      expect(lits.run('mat:rank([[-1, -2], [-3, -4]])')).toEqual(2)
      // Case with mixed numbers
      expect(lits.run('mat:rank([[1, -2], [-3, 4]])')).toEqual(2)
      // Case with single element matrix
      expect(lits.run('mat:rank([[42]])')).toEqual(1)

      // Full rank (3×3)
      expect(lits.run('mat:rank([[1, 0, 0], [0, 1, 0], [0, 0, 1]])')).toBe(3)

      // Rank 2 (3×3)
      expect(lits.run('mat:rank([[1, 2, 3], [4, 5, 6], [7, 8, 9]])')).toBe(2)

      // Rank 1 (3×3)
      expect(lits.run('mat:rank([[2, 4, 6], [3, 6, 9], [4, 8, 12]])')).toBe(1)

      // Rank 0 (zero matrix)
      expect(lits.run('mat:rank([[0, 0, 0], [0, 0, 0], [0, 0, 0]])')).toBe(0)

      // Rectangular matrices
      expect(lits.run('mat:rank([[1, 2], [3, 4], [5, 6]])')).toBe(2) // 3×2 matrix
      expect(lits.run('mat:rank([[1, 2, 3], [4, 5, 6]])')).toBe(2) // 2×3 matrix

      // Matrix with floating point elements
      expect(lits.run('mat:rank([[1.5, 2.5, 3.5], [4.5, 5.5, 6.5], [6.0, 8.0, 10.0]])')).toBe(2)

      // Larger matrix with dependency
      expect(lits.run('mat:rank([[1, 2, 3, 4], [2, 3, 4, 5], [3, 4, 5, 6], [4, 5, 6, 7]])')).toBe(2)

      // Edge cases
      expect(() => lits.run('mat:rank([[]])')).toThrowError(LitsError) // Empty matrix

      // Case with empty matrix (should throw an error)
      expect(() => lits.run('mat:rank([])')).toThrowError(LitsError)
    })
  })
  describe('mat:frobenius-norm', () => {
    it('should return the Frobenius norm of a matrix', () => {
      expect(lits.run('mat:frobenius-norm([[1, 2], [3, 4]])')).toEqual(5.477225575051661)
      expect(lits.run('mat:frobenius-norm([[1, 0], [0, 1]])')).toEqual(1.4142135623730951)
    })
  })
  describe('mat:1-norm', () => {
    it('should return the 1-norm of a matrix', () => {
      expect(lits.run('mat:1-norm([[1, 2], [3, 4]])')).toEqual(6)
      expect(lits.run('mat:1-norm([[1, 0], [0, 1]])')).toEqual(1)
    })
  })
  describe('mat:inf-norm', () => {
    it('should return the infinity norm of a matrix', () => {
      expect(lits.run('mat:inf-norm([[1, 2], [3, 4]])')).toEqual(7)
      expect(lits.run('mat:row-norm([[1, 0], [0, 1]])')).toEqual(1)
    })
  })
  describe('mat:max-norm', () => {
    it('should return the max norm of a matrix', () => {
      expect(lits.run('mat:max-norm([[1, 2], [3, 4]])')).toEqual(4)
      expect(lits.run('mat:max-norm([[1, 0], [0, 1]])')).toEqual(1)
    })
  })
})
