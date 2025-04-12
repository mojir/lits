import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()

describe('vec:rms', () => {
  it('should calculate the root mean square of a vector', () => {
    expect(lits.run('vec:rms([2, 4, 8, 16])')).toBeCloseTo(9.21954446)
    expect(lits.run('vec:rms([1, 2, 2, 3])')).toBeCloseTo(2.12132034)
    expect(() => lits.run('vec:rms([])')).toThrowError(LitsError)
  })
  it('should calculate the moving rms of a vector', () => {
    expect(lits.run('vec:moving-rms([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(lits.run('vec:moving-rms([1, 2, 3, 4, 5, 6], 3)')).toEqual([2.160246899469287, 3.1091263510296048, 4.08248290463863, 5.066228051190222])
    expect(lits.run('vec:moving-rms([1, 2, 3, 4, 5, 6], 6)')).toEqual([3.8944404818493075])
  })
  it('should calculate the centered moving rms of a vector with padding', () => {
    expect(lits.run('vec:centered-moving-rms([1, 2, 3, 4, 5, 6], 3)')).toEqual([null, 2.160246899469287, 3.1091263510296048, 4.08248290463863, 5.066228051190222, null])
    expect(lits.run('vec:centered-moving-rms([1, 2, 3, 4, 5, 6], 3, 0)')).toEqual([1.2909944487358056, 2.160246899469287, 3.1091263510296048, 4.08248290463863, 5.066228051190222, null])
    expect(lits.run('vec:centered-moving-rms([1, -3, -2], 1)')).toEqual([1, 3, 2])
    expect(lits.run('vec:centered-moving-rms([-1, -2], 1)')).toEqual([1, 2])
    expect(lits.run('vec:centered-moving-rms([-1], 1)')).toEqual([1])
    expect(() => lits.run('vec:centered-moving-rms([], 0)')).toThrowError(LitsError)
  })
  it('should calculate the running rms of a vector', () => {
    expect(lits.run('vec:running-rms([1, 2, 3, 4, 5, 6])')).toEqual([1, 1.5811388300841898, 2.160246899469287, 2.7386127875258306, 3.3166247903554, 3.8944404818493075])
    expect(lits.run('vec:running-rms([1, -3, 2])')).toEqual([1, 2.23606797749979, 2.160246899469287])
    expect(lits.run('vec:running-rms([-1, -2, -3])')).toEqual([1, 1.5811388300841898, 2.160246899469287])
    expect(lits.run('vec:running-rms([0])')).toEqual([0])
    expect(() => lits.run('vec:running-rms([])')).toThrowError(LitsError)
  })
})
