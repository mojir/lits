import { describe, expect, it } from 'vitest'
import { trace } from './trace'

describe('trace', () => {
  it('should return the trace of a 1x1 matrix', () => {
    const matrix = [[2]]
    const result = trace(matrix)
    expect(result).toBe(2)
  })

  it('should return the trace of a 2x2 matrix', () => {
    const matrix = [
      [4, 7],
      [2, 6],
    ]
    const result = trace(matrix)
    expect(result).toBe(10)
  })

  it('should return the trace of a 3x3 matrix', () => {
    const matrix = [
      [3, 0, 2],
      [2, 0, -2],
      [0, 1, 1],
    ]
    const result = trace(matrix)
    expect(result).toBe(4)
  })
})
