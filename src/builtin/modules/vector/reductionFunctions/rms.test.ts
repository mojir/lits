import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { vectorModule } from '..'
import { LitsError } from '../../../../errors'

const lits = new Lits({ modules: [vectorModule] })

// Helper to run vec module functions with the new import syntax
function runVec(code: string): unknown {
  // Add module import prefix to function calls
  const modifiedCode = `let v = import("Vector"); v.${code}`
  return lits.run(modifiedCode)
}

describe('rms', () => {
  it('should calculate the root mean square of a vector', () => {
    expect(runVec('rms([2, 4, 8, 16])')).toBeCloseTo(9.21954446)
    expect(runVec('rms([1, 2, 2, 3])')).toBeCloseTo(2.12132034)
    expect(() => runVec('rms([])')).toThrowError(LitsError)
  })
  it('should calculate the moving rms of a vector', () => {
    expect(runVec('moving-rms([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(runVec('moving-rms([1, 2, 3, 4, 5, 6], 3)')).toEqual([2.160246899469287, 3.1091263510296048, 4.08248290463863, 5.066228051190222])
    expect(runVec('moving-rms([1, 2, 3, 4, 5, 6], 6)')).toEqual([3.8944404818493075])
  })
  it('should calculate the centered moving rms of a vector with padding', () => {
    expect(runVec('centered-moving-rms([1, 2, 3, 4, 5, 6], 3)')).toEqual([null, 2.160246899469287, 3.1091263510296048, 4.08248290463863, 5.066228051190222, null])
    expect(runVec('centered-moving-rms([1, 2, 3, 4, 5, 6], 3, 0)')).toEqual([1.2909944487358056, 2.160246899469287, 3.1091263510296048, 4.08248290463863, 5.066228051190222, null])
    expect(runVec('centered-moving-rms([1, -3, -2], 1)')).toEqual([1, 3, 2])
    expect(runVec('centered-moving-rms([-1, -2], 1)')).toEqual([1, 2])
    expect(runVec('centered-moving-rms([-1], 1)')).toEqual([1])
    expect(() => runVec('centered-moving-rms([], 0)')).toThrowError(LitsError)
  })
  it('should calculate the running rms of a vector', () => {
    expect(runVec('running-rms([1, 2, 3, 4, 5, 6])')).toEqual([1, 1.5811388300841898, 2.160246899469287, 2.7386127875258306, 3.3166247903554, 3.8944404818493075])
    expect(runVec('running-rms([1, -3, 2])')).toEqual([1, 2.23606797749979, 2.160246899469287])
    expect(runVec('running-rms([-1, -2, -3])')).toEqual([1, 1.5811388300841898, 2.160246899469287])
    expect(runVec('running-rms([0])')).toEqual([0])
    expect(() => runVec('running-rms([])')).toThrowError(LitsError)
  })
})
