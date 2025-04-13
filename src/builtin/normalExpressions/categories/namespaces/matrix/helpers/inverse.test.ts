import { describe, expect, it } from 'vitest'
import { inverse } from './inverse'

describe('inverse', () => {
  it('should return the inverse of a 1x1 matrix', () => {
    const matrix = [[2]]
    const result = inverse(matrix)
    expect(result).toEqual([[0.5]])
  })

  it('should return null for a 1x1 matrix with value 0 (not invertible)', () => {
    const matrix = [[0]]
    const result = inverse(matrix)
    expect(result).toBeNull()
  })

  it('should return the inverse of a 2x2 matrix', () => {
    const matrix = [
      [4, 7],
      [2, 6],
    ]
    const result = inverse(matrix)
    expect(result).toEqual([
      [0.6, -0.7],
      [-0.2, 0.4],
    ])
  })

  it('should return null for a 2x2 matrix with determinant 0 (not invertible)', () => {
    const matrix = [
      [1, 2],
      [2, 4],
    ]
    const result = inverse(matrix)
    expect(result).toBeNull()
  })

  it('should return the inverse of a 3x3 matrix', () => {
    const matrix = [
      [3, 0, 2],
      [2, 0, -2],
      [0, 1, 1],
    ]
    const result = inverse(matrix)
    expect(result).toEqual([
      [0.2, 0.2, -0],
      [-0.2, 0.3, 1],
      [0.2, -0.3, 0],
    ])
  })

  it('should return null for a 3x3 matrix with determinant 0 (not invertible)', () => {
    const matrix = [
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9],
    ]
    const result = inverse(matrix)
    expect(result).toBeNull()
  })
})
