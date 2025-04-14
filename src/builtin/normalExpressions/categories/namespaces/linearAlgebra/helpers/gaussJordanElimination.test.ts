import { describe, expect, it } from 'vitest'
import { gaussJordanElimination } from './gaussJordanElimination'

describe('gaussJordanElimination', () => {
  it('should handle a single row matrix', () => {
    const matrix = [[2, 4, 6]]
    const [result, rank] = gaussJordanElimination(matrix)
    expect(result).toEqual([[1, 2, 3]])
    expect(rank).toBe(1)
  })

  it('should handle a square matrix', () => {
    const matrix = [
      [2, 4, 6],
      [1, 3, 5],
      [3, 7, 9],
    ]
    const [result, rank] = gaussJordanElimination(matrix)
    expect(result).toEqual([
      [1, 0, 0],
      [0, 1, 0],
      [0, 0, 1],
    ])
    expect(rank).toBe(3)
  })

  it('should handle a rectangular matrix with more rows than columns', () => {
    const matrix = [
      [2, 4],
      [1, 3],
      [3, 7],
    ]
    const [result, rank] = gaussJordanElimination(matrix)
    expect(result).toEqual([
      [1, 0],
      [0, 1],
      [0, 0],
    ])
    expect(rank).toBe(2)
  })

  it('should handle a rectangular matrix with more columns than rows', () => {
    const matrix = [
      [2, 4, 6],
      [1, 3, 5],
    ]
    const [result, rank] = gaussJordanElimination(matrix)
    expect(result).toEqual([
      [1, 0, -1],
      [0, 1, 2],
    ])
    expect(rank).toBe(2)
  })

  it('should handle a matrix with all zero rows', () => {
    const matrix = [
      [0, 0, 0],
      [0, 0, 0],
    ]
    const [result, rank] = gaussJordanElimination(matrix)
    expect(result).toEqual([
      [0, 0, 0],
      [0, 0, 0],
    ])
    expect(rank).toBe(0)
  })
})
