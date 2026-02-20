import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { vectorModule } from '..'
import { LitsError } from '../../../../errors'

const lits = new Lits({ modules: [vectorModule] })

describe('min', () => {
  it('should calculate min of a vector', () => {
    expect(lits.run('min([1, 2, 3])')).toEqual(1)
    expect(lits.run('min([1, -2, 3])')).toEqual(-2)
    expect(lits.run('min([-1, -2, -3])')).toEqual(-3)
    expect(lits.run('min([0])')).toEqual(0)
    expect(() => lits.run('min([])')).toThrow(LitsError)
  })
})
