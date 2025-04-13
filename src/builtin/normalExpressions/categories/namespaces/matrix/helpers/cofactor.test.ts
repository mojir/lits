import { describe, expect, it } from 'vitest'
import { cofactor } from './cofactor'

describe('cofactor', () => {
  it('should calculate the cofactor matrix of a 2x2 matrix', () => {
    const matrix = [
      [1, 2],
      [3, 4],
    ]
    const result = cofactor(matrix)
    expect(result).toEqual([
      [4, -3],
      [-2, 1],
    ])
  })

  it('should calculate the cofactor matrix of a 3x3 matrix', () => {
    const matrix = [
      [1, 2, 3],
      [0, 4, 5],
      [1, 0, 6],
    ]
    const result = cofactor(matrix)
    expect(result).toEqual([
      [24, 5, -4],
      [-12, 3, 2],
      [-2, -5, 4],
    ])
  })

  it('should return an empty array for an empty matrix', () => {
    const matrix: number[][] = []
    const result = cofactor(matrix)
    expect(result).toEqual([])
  })

  it('should handle a 1x1 matrix', () => {
    const matrix = [[7]]
    const result = cofactor(matrix)
    expect(result).toEqual([[1]])
  })
})
