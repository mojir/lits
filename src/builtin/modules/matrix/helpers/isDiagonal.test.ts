import { describe, expect, it } from 'vitest'
import { isDiagonal } from './isDiagonal'

describe('isDiagonal', () => {
  it('should return true for a diagonal matrix', () => {
    const matrix = [
      [1, 0, 0],
      [0, 2, 0],
      [0, 0, 3],
    ]
    expect(isDiagonal(matrix)).toBe(true)
  })

  it('should return false for a non-diagonal matrix', () => {
    const matrix = [
      [1, 0, 0],
      [0, 2, 3],
      [0, 0, 3],
    ]
    expect(isDiagonal(matrix)).toBe(false)
  })

  it('should return false for a non-square matrix', () => {
    const matrix = [
      [1, 2, 3],
      [4, 5, 6],
    ]
    expect(isDiagonal(matrix)).toBe(false)
  })
})
