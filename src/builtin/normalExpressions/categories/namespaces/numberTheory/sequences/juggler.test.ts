import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()
describe('juggler', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('nth:juggler-seq(10)')).toEqual([
      10,
      3,
      5,
      11,
      36,
      6,
      2,
      1,
    ])
    expect(() => lits.run('nth:juggler-seq(0)')).toThrow(LitsError)
    expect(() => lits.run('nth:juggler-seq(58025)')).toThrow(LitsError)
  })
})
