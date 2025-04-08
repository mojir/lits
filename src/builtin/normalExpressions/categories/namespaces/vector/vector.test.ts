import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

const lits = new Lits()
describe('vector functions', () => {
  describe('v:vector?', () => {
    it('should determine if a value is a vector', () => {
      expect(lits.run('v:vector?([])')).toEqual(true)
      expect(lits.run('v:vector?([1, 2, 3])')).toEqual(true)
      expect(lits.run('v:vector?([1, 2, [3]])')).toEqual(false)
      expect(lits.run('v:vector?([1, 2, 3.0])')).toEqual(true)
      expect(lits.run('v:vector?([1, 2, "3"])')).toEqual(false)
      expect(lits.run('v:vector?([1, 2, true])')).toEqual(false)
      expect(lits.run('v:vector?([1, 2, {}])')).toEqual(false)
      expect(lits.run('v:vector?(12)')).toEqual(false)
      expect(lits.run('v:vector?({})')).toEqual(false)
    })
  })
  describe('v:sorted?', () => {
    it('should determine if a vector is sorted', () => {
      expect(lits.run('v:sorted?([1, 2, 3])')).toEqual(true)
      expect(lits.run('v:sorted?([1, 2, 3, 2])')).toEqual(false)
      expect(lits.run('v:sorted?([1, 2, 3, 4])')).toEqual(true)
      expect(lits.run('v:sorted?([4, 3, 2, 1])')).toEqual(false)
      expect(lits.run('v:sorted?([])')).toEqual(true)
    })
  })
  describe('v:monotonic?', () => {
    it('should determine if a vector is monotonic', () => {
      expect(lits.run('v:monotonic?([1, 2, 3])')).toEqual(true)
      expect(lits.run('v:monotonic?([1, 2, 3, 2])')).toEqual(false)
      expect(lits.run('v:monotonic?([1, 2, 3, 4])')).toEqual(true)
      expect(lits.run('v:monotonic?([4, 3, 2, 1])')).toEqual(true)
      expect(lits.run('v:monotonic?([4, 3, 2, 1, 1, 1])')).toEqual(true)
      expect(lits.run('v:monotonic?([])')).toEqual(true)
    })
  })
  describe('v:strictly-monotonic?', () => {
    it('should determine if a vector is monotonic', () => {
      expect(lits.run('v:strictly-monotonic?([1, 2, 3])')).toEqual(true)
      expect(lits.run('v:strictly-monotonic?([1, 2, 3, 2])')).toEqual(false)
      expect(lits.run('v:strictly-monotonic?([1, 2, 3, 4])')).toEqual(true)
      expect(lits.run('v:strictly-monotonic?([4, 3, 2, 1])')).toEqual(true)
      expect(lits.run('v:strictly-monotonic?([4, 3, 2, 1, 1, 1])')).toEqual(false)
      expect(lits.run('v:strictly-monotonic?([])')).toEqual(true)
    })
  })
  describe('v:increasing?', () => {
    it('should determine if a vector is monotonic increasing', () => {
      expect(lits.run('v:increasing?([1, 2, 3])')).toEqual(true)
      expect(lits.run('v:increasing?([1, 2, 2, 3, 2])')).toEqual(false)
      expect(lits.run('v:increasing?([1, 2, 2, 3, 4])')).toEqual(true)
      expect(lits.run('v:increasing?([4, 3, 2, 1])')).toEqual(false)
      expect(lits.run('v:increasing?([])')).toEqual(true)
    })
  })
  describe('v:decreasing?', () => {
    it('should determine if a vector is monotonic decreasing', () => {
      expect(lits.run('v:decreasing?([1, 2, 2, 3])')).toEqual(false)
      expect(lits.run('v:decreasing?([1, 2, 3, 2])')).toEqual(false)
      expect(lits.run('v:decreasing?([1, 2, 3, 4])')).toEqual(false)
      expect(lits.run('v:decreasing?([4, 3, 2, 2, 1])')).toEqual(true)
      expect(lits.run('v:decreasing?([4, 4, 3, 2, 2, 1])')).toEqual(true)
      expect(lits.run('v:decreasing?([])')).toEqual(true)
    })
  })
  describe('v:strictly-increasing?', () => {
    it('should determine if a vector is strictly monotonic increasing', () => {
      expect(lits.run('v:strictly-increasing?([1, 2, 3])')).toEqual(true)
      expect(lits.run('v:strictly-increasing?([1, 2, 3, 2])')).toEqual(false)
      expect(lits.run('v:strictly-increasing?([1, 2, 3, 4])')).toEqual(true)
      expect(lits.run('v:strictly-increasing?([1, 1, 2, 3, 4])')).toEqual(false)
      expect(lits.run('v:strictly-increasing?([4, 3, 2, 1])')).toEqual(false)
      expect(lits.run('v:strictly-increasing?([])')).toEqual(true)
    })
  })
  describe('v:strictly-decreasing?', () => {
    it('should determine if a vector is strictly monotonic decreasing', () => {
      expect(lits.run('v:strictly-decreasing?([1, 2, 3])')).toEqual(false)
      expect(lits.run('v:strictly-decreasing?([1, 2, 3, 2])')).toEqual(false)
      expect(lits.run('v:strictly-decreasing?([1, 2, 3, 4])')).toEqual(false)
      expect(lits.run('v:strictly-decreasing?([4, 3, 2, 1])')).toEqual(true)
      expect(lits.run('v:strictly-decreasing?([4, 3, 3, 2, 1])')).toEqual(false)
      expect(lits.run('v:strictly-decreasing?([])')).toEqual(true)
    })
  })
})
