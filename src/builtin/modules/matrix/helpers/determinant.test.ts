import { describe, expect, it } from 'vitest'
import { determinant } from './determinant'

describe('determinant', () => {
  it('should return the determinant of a 1x1 matrix', () => {
    const matrix = [[5]]
    expect(determinant(matrix)).toBe(5)
  })

  it('should return the determinant of a 2x2 matrix', () => {
    const matrix = [
      [1, 2],
      [3, 4],
    ]
    expect(determinant(matrix)).toBe(-2)
  })

  it('should return the determinant of a 3x3 matrix', () => {
    const matrix = [
      [6, 1, 1],
      [4, -2, 5],
      [2, 8, 7],
    ]
    expect(determinant(matrix)).toBe(-306)
  })

  it('should return 0 for a singular matrix', () => {
    const matrix = [
      [2, 4, 6],
      [1, 2, 3],
      [7, 8, 9],
    ]
    expect(determinant(matrix)).toBe(0)
  })

  it('should return the determinant of another 3x3 matrix', () => {
    const matrix = [
      [3, 0, 2],
      [2, 0, -2],
      [0, 1, 1],
    ]
    const result = determinant(matrix)
    expect(result).toEqual(10)
  })

  it('should handle larger matrices (4x4)', () => {
    const matrix = [
      [1, 0, 2, -1],
      [3, 0, 0, 5],
      [2, 1, 4, -3],
      [1, 0, 5, 0],
    ]
    expect(determinant(matrix)).toBe(30)
  })

  it('should handle negative values in the matrix', () => {
    const matrix = [
      [-2, 2, -3],
      [-1, 1, 3],
      [2, 0, -1],
    ]
    expect(determinant(matrix)).toBe(18)
  })

  it('should handle identity matrices', () => {
    const matrix = [
      [1, 0, 0],
      [0, 1, 0],
      [0, 0, 1],
    ]
    expect(determinant(matrix)).toBe(1)
  })

  it('should handle zero matrices', () => {
    const matrix = [
      [0, 0, 0],
      [0, 0, 0],
      [0, 0, 0],
    ]
    expect(determinant(matrix)).toBe(0)
  })
})
