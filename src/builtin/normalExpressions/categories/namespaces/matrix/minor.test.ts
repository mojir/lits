import { describe, expect, it } from 'vitest'
import { minor } from './minor'

describe('minor', () => {
  it('should return the correct minor matrix when removing a row and column', () => {
    const matrix = [
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9],
    ]
    const result = minor(matrix, 1, 1)
    expect(result).toEqual([
      [1, 3],
      [7, 9],
    ])
  })

  it('should return the correct minor matrix when removing the first row and column', () => {
    const matrix = [
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9],
    ]
    const result = minor(matrix, 0, 0)
    expect(result).toEqual([
      [5, 6],
      [8, 9],
    ])
  })

  it('should return the correct minor matrix when removing the last row and column', () => {
    const matrix = [
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9],
    ]
    const result = minor(matrix, 2, 2)
    expect(result).toEqual([
      [1, 2],
      [4, 5],
    ])
  })

  it('should handle a 2x2 matrix correctly', () => {
    const matrix = [
      [1, 2],
      [3, 4],
    ]
    const result = minor(matrix, 0, 1)
    expect(result).toEqual([[3]])
  })

  it('should return an empty matrix when the input matrix is 1x1', () => {
    const matrix = [[1]]
    const result = minor(matrix, 0, 0)
    expect(result).toEqual([])
  })
})
