import { describe, expect, it } from 'vitest'
import { diagonal } from './diagonal'

describe('diagonal', () => {
  it('should return the diagonal elements of a square matrix', () => {
    const matrix = [
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9],
    ]
    const result = diagonal(matrix)
    expect(result).toEqual([1, 5, 9])
  })

  it('should handle a 1x1 matrix', () => {
    const matrix = [[42]]
    const result = diagonal(matrix)
    expect(result).toEqual([42])
  })
})
