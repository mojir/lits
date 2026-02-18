import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { vectorNamespace } from '..'
import { LitsError } from '../../../../errors'

const lits = new Lits({ namespaces: [vectorNamespace] })

// Helper to run vec namespace functions with the new import syntax
function runVec(code: string): unknown {
  // Add namespace import prefix to function calls
  const modifiedCode = `let v = import("Vector"); v.${code}`
  return lits.run(modifiedCode)
}

describe('mean functions', () => {
  describe('mean', () => {
    it('should calculate the mean of a vector', () => {
      expect(runVec('mean([1, 2, 3])')).toEqual(2)
      expect(runVec('mean([1, -3, 2])')).toEqual(0)
      expect(runVec('mean([-1, -2, -3])')).toEqual(-2)
      expect(runVec('mean([0])')).toEqual(0)
      expect(() => runVec('mean([])')).toThrowError(LitsError)
    })
    it('should calculate the moving mean of a vector', () => {
      expect(runVec('moving-mean([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
      expect(runVec('moving-mean([1, 2, 3, 4, 5, 6], 3)')).toEqual([2, 3, 4, 5])
      expect(runVec('moving-mean([1, 2, 3, 4, 5, 6], 6)')).toEqual([3.5])
    })
    it('should calculate the centered moving mean of a vector with padding', () => {
      expect(runVec('centered-moving-mean([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
      expect(runVec('centered-moving-mean([1, 2, 3, 4, 5, 6], 2)')).toEqual([null, 1.5, 2.5, 3.5, 4.5, 5.5])
      expect(runVec('centered-moving-mean([1, 2, 3, 4, 5, 6], 2, 10)')).toEqual([5.5, 1.5, 2.5, 3.5, 4.5, 5.5])
      expect(runVec('centered-moving-mean([1, 2, 3, 4, 5, 6], 3)')).toEqual([null, 2, 3, 4, 5, null])
      expect(runVec('centered-moving-mean([1, 2, 3, 4, 5, 6], 4)')).toEqual([null, null, 10 / 4, 14 / 4, 18 / 4, null])
      expect(runVec('centered-moving-mean([1, 2, 3, 4, 5, 6], 5)')).toEqual([null, null, 3, 4, null, null])
      expect(runVec('centered-moving-mean([1, 2, 3, 4, 5, 6], 6)')).toEqual([null, null, null, 21 / 6, null, null])
    })
    it('should calculate the running mean of a vector', () => {
      expect(runVec('running-mean([1, 2, 3, 4, 5, 6])')).toEqual([1, 1.5, 2, 2.5, 3, 3.5])
      expect(runVec('running-mean([1, -3, 2])')).toEqual([1, -1, 0])
      expect(runVec('running-mean([-1, -2, -3])')).toEqual([-1, -1.5, -2])
      expect(runVec('running-mean([0])')).toEqual([0])
      expect(() => runVec('running-mean([])')).toThrowError(LitsError)
    })
  })
  describe('geometric-mean', () => {
    it('should calculate the geometric mean of a vector', () => {
      expect(runVec('geometric-mean([2, 4, 8, 16])')).toBeCloseTo(5.656854)
      expect(runVec('geometric-mean([1, 2, 2, 3])')).toBeCloseTo(1.8612097182041991)
      expect(() => runVec('geometric-mean([])')).toThrowError(LitsError)
    })
    it('should calculate the moving geometric mean of a vector', () => {
      expect(runVec('moving-geometric-mean([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 2.9999999999999996, 4, 5, 6])
      expect(runVec('moving-geometric-mean([1, 2, 3, 4, 5, 6], 3)')).toEqual([1.8171205928321394, 2.8844991406148166, 3.9148676411688634, 4.93242414866094])
      expect(() => runVec('moving-geometric-mean([1, -2, -3], 2)')).toThrow(LitsError)
      expect(() => runVec('moving-geometric-mean([1], 100)')).toThrow(LitsError)
      expect(() => runVec('moving-geometric-mean([], 1)')).toThrowError(LitsError)
    })
    it('should calculate the centered moving geometric mean of a vector with padding', () => {
      expect(runVec('centered-moving-geometric-mean([1, 2, 3, 4, 5], 3)')).toEqual([null, 1.8171205928321394, 2.8844991406148166, 3.9148676411688634, null])
      expect(() => runVec('centered-moving-geometric-mean([1, -2, -3], 2)')).toThrow(LitsError)
      expect(() => runVec('centered-moving-geometric-mean([1], 100)')).toThrow(LitsError)
      expect(() => runVec('centered-moving-geometric-mean([], 1)')).toThrowError(LitsError)
    })
    it('should calculate the running geometric mean of a vector', () => {
      expect(runVec('running-geometric-mean([1, 2, 3, 4, 5, 6])')).toEqual([1, 1.414213562373095, 1.8171205928321394, 2.213363839400643, 2.6051710846973517, 2.993795165523909])
      expect(() => runVec('running-geometric-mean([])')).toThrowError(LitsError)
      expect(() => runVec('running-geometric-mean([1, -2, -3])')).toThrow(LitsError)
    })
  })
  describe('harmonic-mean', () => {
    it('should calculate the harmonic mean of a vector', () => {
      expect(runVec('harmonic-mean([2, 4, 8, 16])')).toBeCloseTo(4.266666666667)
      expect(runVec('harmonic-mean([1, 2, 2, 3])')).toBeCloseTo(1.7142857142857142)
      expect(() => runVec('harmonic-mean([])')).toThrowError(LitsError)
    })
    it('should calculate the moving harmonic mean of a vector', () => {
      expect(runVec('moving-harmonic-mean([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
      expect(runVec('moving-harmonic-mean([1, 2, 3, 4, 5, 6], 3)')).toEqual([1.6363636363636365, 2.7692307692307696, 3.829787234042554, 4.864864864864865])
      expect(() => runVec('moving-harmonic-mean([1], 100)')).toThrow(LitsError)
      expect(() => runVec('moving-harmonic-mean([], 1)')).toThrowError(LitsError)
    })
    it('should calculate the centered moving harmonic mean of a vector with padding', () => {
      expect(runVec('centered-moving-harmonic-mean([1, 2, 3, 4, 5], 3)')).toEqual([null, 1.6363636363636365, 2.7692307692307696, 3.829787234042554, null])
      expect(() => runVec('centered-moving-harmonic-mean([1], 100)')).toThrow(LitsError)
      expect(() => runVec('centered-moving-harmonic-mean([], 1)')).toThrowError(LitsError)
    })
    it('should calculate the running harmonic mean of a vector', () => {
      expect(runVec('running-harmonic-mean([1, 2, 3, 4, 5, 6])')).toEqual([1, 1.3333333333333333, 1.6363636363636365, 1.9200000000000004, 2.18978102189781, 2.4489795918367347])
      expect(() => runVec('running-harmonic-mean([])')).toThrowError(LitsError)
      expect(() => runVec('running-harmonic-mean([1], 100)')).toThrow(LitsError)
    })
  })
})
