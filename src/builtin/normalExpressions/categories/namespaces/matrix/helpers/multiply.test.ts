import { describe, expect, it } from 'vitest'
import { multiply } from './multiply'

describe('multiply', () => {
  it('should return null if matrices cannot be multiplied (invalid dimensions)', () => {
    const A = [[1, 2], [3, 4]]
    const B = [[1, 2, 3]]
    expect(() => multiply(A, B)).toThrow()
  })

  it('should return the correct result for valid matrices', () => {
    const A = [
      [1, 2],
      [3, 4],
    ]
    const B = [
      [5, 6],
      [7, 8],
    ]
    const expected = [
      [19, 22],
      [43, 50],
    ]
    expect(multiply(A, B)).toEqual(expected)
  })

  it('should handle single-element matrices', () => {
    const A = [[2]]
    const B = [[3]]
    const expected = [[6]]
    expect(multiply(A, B)).toEqual(expected)
  })

  it('should handle non-square matrices', () => {
    const A = [
      [1, 2, 3],
      [4, 5, 6],
    ]
    const B = [
      [7, 8],
      [9, 10],
      [11, 12],
    ]
    const expected = [
      [58, 64],
      [139, 154],
    ]
    expect(multiply(A, B)).toEqual(expected)
  })

  it('should return a zero matrix if one of the matrices is a zero matrix', () => {
    const A = [
      [1, 2],
      [3, 4],
    ]
    const B = [
      [0, 0],
      [0, 0],
    ]
    const expected = [
      [0, 0],
      [0, 0],
    ]
    expect(multiply(A, B)).toEqual(expected)
  })
})
