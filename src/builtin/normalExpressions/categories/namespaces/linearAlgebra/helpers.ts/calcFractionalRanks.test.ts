import { describe, expect, it } from 'vitest'
import { calcFractionalRanks } from './calcFractionalRanks'

describe('calcRanks', () => {
  it('should return ranks for a simple vector', () => {
    const vector = [3, 1, 2]
    const expectedRanks = [3, 1, 2]
    const ranks = calcFractionalRanks(vector)
    expect(ranks).toEqual(expectedRanks)
  })

  it('should work 1', () => {
    const vector = [5, 2, 4, 1, 3]
    const expectedRanks = [5, 2, 4, 1, 3]
    const ranks = calcFractionalRanks(vector)
    expect(ranks).toEqual(expectedRanks)
  })

  it('should work 2', () => {
    const vector = [1, 2, 3, 4, 5]
    const expectedRanks = [1, 2, 3, 4, 5]
    const ranks = calcFractionalRanks(vector)
    expect(ranks).toEqual(expectedRanks)
  })

  it('should work 3', () => {
    const vector = [1, 2, 3, 4, 100]
    const expectedRanks = [1, 2, 3, 4, 5]
    const ranks = calcFractionalRanks(vector)
    expect(ranks).toEqual(expectedRanks)
  })

  it('should handle ties correctly', () => {
    const vector = [3, 1, 2, 2]
    const expectedRanks = [4, 1, 2.5, 2.5]
    const ranks = calcFractionalRanks(vector)
    expect(ranks).toEqual(expectedRanks)
  })

  it('should handle empty vectors', () => {
    const vector: number[] = []
    const expectedRanks: number[] = []
    const ranks = calcFractionalRanks(vector)
    expect(ranks).toEqual(expectedRanks)
  })
})
