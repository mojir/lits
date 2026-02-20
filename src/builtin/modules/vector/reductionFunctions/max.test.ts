import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { vectorModule } from '..'
import { LitsError } from '../../../../errors'

const lits = new Lits({ modules: [vectorModule] })

describe('max', () => {
  it('should calculate max of a vector', () => {
    expect(lits.run('max([1, 2, 3])')).toEqual(3)
    expect(lits.run('max([1, -2, 3])')).toEqual(3)
    expect(lits.run('max([-1, -2, -3])')).toEqual(-1)
    expect(lits.run('max([0])')).toEqual(0)
    expect(() => lits.run('max([])')).toThrowError(LitsError)
  })
})
