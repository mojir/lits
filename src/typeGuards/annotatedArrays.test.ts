import { describe, expect, it } from 'vitest'
import { LitsError } from '../errors'
import { assertGrid, assertMatrix, assertNonEmptyVector, assertSquareMatrix, assertVector, isGrid, isMatrix, isSquareMatrix, isVector } from './annotatedArrays'

describe('annotatedArrays', () => {
  it('should correctly identify vectors', () => {
    expect(isVector([1, 2, 3])).toBe(true)
    expect(isVector([1, 2, '3'])).toBe(false)
    expect(isVector([])).toBe(true)
    expect(isVector('not an array')).toBe(false)
  })

  it('should assert vectors correctly', () => {
    expect(() => assertVector([1, 2, 3], undefined)).not.toThrow()
    expect(() => assertVector([1, 2, '3'], undefined)).toThrow('Expected a vector, but got 1,2,3')
  })

  it('should assert non-empty vectors correctly', () => {
    expect(() => assertNonEmptyVector([1, 2, 3], undefined)).not.toThrow()
    expect(() => assertNonEmptyVector([], undefined)).toThrow('Expected a non empty vector, but got ')
  })

  it('should correctly identify grids', () => {
    expect(isGrid([[1, 2], [3, 4]])).toBe(true)
    expect(isGrid([[1, 2], [3]])).toBe(false)
    expect(isGrid([])).toBe(false)
    expect(isGrid('not an array')).toBe(false)
  })

  it('should assert grids correctly', () => {
    expect(() => assertGrid([[1, 2], [3, 4]], undefined)).not.toThrow()
    expect(() => assertGrid([[1, 2], [3]], undefined)).toThrow('Expected a grid, but got 1,2,3')
  })

  it('should correctly identify matrices', () => {
    expect(isMatrix([[1, 2], [3, 4]])).toBe(true)
    expect(isMatrix([[1, 2], [3, '4']])).toBe(false)
    expect(isMatrix([])).toBe(false)
    expect(isMatrix('not an array')).toBe(false)
  })

  it('should assert matrices correctly', () => {
    expect(() => assertMatrix([[1, 2], [3, 4]], undefined)).not.toThrow()
    expect(() => assertMatrix([[1, 2], [3, '4']], undefined)).toThrow('Expected a matrix, but got 1,2,3,4')
  })

  it('should correctly identify square matrices', () => {
    expect(isSquareMatrix([[1, 2], [3, 4]])).toBe(true)
    expect(isSquareMatrix([[1, 2, 3], [4, 5, 6], [7, 8, 9]])).toBe(true)
    expect(isSquareMatrix([[1, 2], [3, 2], [3, 2]])).toBe(false)
    expect(isSquareMatrix([[1, 2], [3, 4, 5]])).toBe(false)
  })

  it('should assert square matrices correctly', () => {
    expect(() => assertSquareMatrix([[1, 2], [3, 4]], undefined)).not.toThrow(LitsError)
    expect(() => assertSquareMatrix([[1, 2], [3]], undefined)).toThrow(LitsError)
    expect(() => assertSquareMatrix([[1, 2]], undefined)).toThrow(LitsError)
  })
})
