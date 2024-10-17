import { describe, expect, it } from 'vitest'
import { combinate } from './utils'

describe('utils', () => {
  describe('combine', () => {
    it('should combine two array of arrays', () => {
      expect(combinate([[]])).toEqual([])
      expect(combinate([[1, 2]])).toEqual([[1], [2]])
      expect(combinate([[1, 2], [3]])).toEqual([[1, 3], [2, 3]])
      expect(combinate([[1], [2, 3]])).toEqual([[1, 2], [1, 3]])
      expect(combinate([[1, 2], [3, 4]])).toEqual([[1, 3], [1, 4], [2, 3], [2, 4]])
      expect(combinate([[1, 2], [3], [4, 5]])).toEqual([[1, 3, 4], [1, 3, 5], [2, 3, 4], [2, 3, 5]])
    })
  })
})
