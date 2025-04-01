import { describe, expect, it } from 'vitest'
import { isSquare } from './isSquare'

describe('isSquare', () => {
  it('should return true for a square matrix', () => {
    const matrix = [
      [1, 2],
      [3, 4],
    ]
    expect(isSquare(matrix)).toBe(true)
  })

  it('should return false for a non-square matrix', () => {
    const matrix = [
      [1, 2, 3],
      [4, 5, 6],
    ]
    expect(isSquare(matrix)).toBe(false)
  })

  it('should return true for a 1x1 matrix', () => {
    const matrix = [[1]]
    expect(isSquare(matrix)).toBe(true)
  })

  it('should return false for a matrix with unequal row lengths', () => {
    const matrix = [
      [1],
      [3],
    ]
    expect(isSquare(matrix)).toBe(false)
  })
})
