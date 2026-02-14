import { describe, expect, it } from 'vitest'
import { isSymetric } from './isSymetric'

describe('isSymetric', () => {
  it('should return true for a symmetric matrix', () => {
    const matrix = [
      [1, 2, 3],
      [2, 4, 5],
      [3, 5, 6],
    ]
    expect(isSymetric(matrix)).toBe(true)
  })

  it('should return false for a non-symmetric matrix', () => {
    const matrix = [
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9],
    ]
    expect(isSymetric(matrix)).toBe(false)
  })

  it('should return false for a non-square matrix', () => {
    const matrix = [
      [1, 2, 3],
      [4, 5, 6],
    ]
    expect(isSymetric(matrix)).toBe(false)
  })

  it('should return true for a 1x1 matrix', () => {
    const matrix = [[1]]
    expect(isSymetric(matrix)).toBe(true)
  })

  it('should return false for a matrix with unequal row lengths', () => {
    const matrix = [
      [1, 2],
      [3],
    ]
    expect(isSymetric(matrix)).toBe(false)
  })
})
