import { describe, expect, it } from 'vitest'
import { isTriangular, isTriangularLower, isTriangularUpper } from './isTriangular'

describe('all isTriangular', () => {
  describe('isTriangular.', () => {
    it('should return true for an upper triangular matrix', () => {
      const matrix = [
        [1, 2, 3],
        [0, 4, 5],
        [0, 0, 6],
      ]
      expect(isTriangular(matrix)).toBe(true)
    })

    it('should return true for a lower triangular matrix', () => {
      const matrix = [
        [1, 0, 0],
        [2, 4, 0],
        [3, 5, 6],
      ]
      expect(isTriangular(matrix)).toBe(true)
    })

    it('should return false for a non-triangular matrix', () => {
      const matrix = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9],
      ]
      expect(isTriangular(matrix)).toBe(false)
    })

    it('should return false for a non-square matrix', () => {
      const matrix = [
        [1, 2, 3],
        [4, 5, 6],
      ]
      expect(isTriangular(matrix)).toBe(false)
    })

    it('should return true for a 1x1 matrix', () => {
      const matrix = [[1]]
      expect(isTriangular(matrix)).toBe(true)
    })

    it('should return false for a matrix with unequal row lengths', () => {
      const matrix = [
        [1, 2],
        [3],
      ]
      expect(isTriangular(matrix)).toBe(false)
    })
  })

  describe('isTriangularUpper', () => {
    it('should return true for an upper triangular matrix', () => {
      const matrix = [
        [1, 2, 3],
        [0, 4, 5],
        [0, 0, 6],
      ]
      expect(isTriangularUpper(matrix)).toBe(true)
    })

    it('should return false for a lower triangular matrix', () => {
      const matrix = [
        [1, 0, 0],
        [2, 4, 0],
        [3, 5, 6],
      ]
      expect(isTriangularUpper(matrix)).toBe(false)
    })

    it('should return false for a non-upper triangular matrix', () => {
      const matrix = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9],
      ]
      expect(isTriangularUpper(matrix)).toBe(false)
    })

    it('should return false for a non-square matrix', () => {
      const matrix = [
        [1, 2, 3],
        [4, 5, 6],
      ]
      expect(isTriangularUpper(matrix)).toBe(false)
    })

    it('should return true for a 1x1 matrix', () => {
      const matrix = [[1]]
      expect(isTriangularUpper(matrix)).toBe(true)
    })

    it('should return false for a matrix with unequal row lengths', () => {
      const matrix = [
        [1, 2],
        [3],
      ]
      expect(isTriangularUpper(matrix)).toBe(false)
    })
  })

  describe('isTriangularLower', () => {
    it('should return false for an upper triangular matrix', () => {
      const matrix = [
        [1, 2, 3],
        [0, 4, 5],
        [0, 0, 6],
      ]
      expect(isTriangularLower(matrix)).toBe(false)
    })

    it('should return true for a lower triangular matrix', () => {
      const matrix = [
        [1, 0, 0],
        [2, 4, 0],
        [3, 5, 6],
      ]
      expect(isTriangularLower(matrix)).toBe(true)
    })

    it('should return false for a non-lower triangular matrix', () => {
      const matrix = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9],
      ]
      expect(isTriangularLower(matrix)).toBe(false)
    })

    it('should return false for a non-square matrix', () => {
      const matrix = [
        [1, 2, 3],
        [4, 5, 6],
      ]
      expect(isTriangularLower(matrix)).toBe(false)
    })

    it('should return true for a 1x1 matrix', () => {
      const matrix = [[1]]
      expect(isTriangularLower(matrix)).toBe(true)
    })

    it('should return false for a matrix with unequal row lengths', () => {
      const matrix = [
        [1, 2],
        [3],
      ]
      expect(isTriangularLower(matrix)).toBe(false)
    })
  })
})
