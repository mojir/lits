import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

const lits = new Lits()
describe('juggler', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('c:juggler-seq(11)')).toEqual([
      11,
      36,
      6,
      2,
      1,
    ])
    expect(() => lits.run('c:juggler-seq(0)')).toThrow()
  })
})
