import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()
describe('collatz', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('nth:collatz-seq(11)')).toEqual([
      11,
      34,
      17,
      52,
      26,
      13,
      40,
      20,
      10,
      5,
      16,
      8,
      4,
      2,
      1,
    ])
    expect(() => lits.run('nth:collatz-seq(0)')).toThrow(LitsError)
  })
})
