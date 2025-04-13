import { describe, expect, it } from 'vitest'
import { adjugate } from './adjugate'

describe('adjugate', () => {
  it('should return the adjugate of a 2x2 matrix', () => {
    const matrix = [
      [1, 2],
      [3, 4],
    ]
    const result = adjugate(matrix)
    expect(result).toEqual([
      [4, -2],
      [-3, 1],
    ])
  })

  it('should return the adjugate of a 3x3 matrix', () => {
    const matrix = [
      [1, 2, 3],
      [0, 1, 4],
      [5, 6, 0],
    ]
    const result = adjugate(matrix)
    expect(result).toEqual([
      [-24, 18, 5],
      [20, -15, -4],
      [-5, 4, 1],
    ])
  })

  it('should return the adjugate of another 3x3 matrix', () => {
    const matrix = [
      [3, 0, 2],
      [3, 0, -2],
      [0, 1, 1],
    ]
    const result = adjugate(matrix)
    expect(result).toEqual([
      [2, 2, -0],
      [-3, 3, 12],
      [3, -3, 0],
    ])
  })

  it('should return an empty array for an empty matrix', () => {
    const matrix: number[][] = []
    const result = adjugate(matrix)
    expect(result).toEqual([])
  })

  it('should handle a 1x1 matrix', () => {
    const matrix = [[7]]
    const result = adjugate(matrix)
    expect(result).toEqual([[1]])
  })
})
