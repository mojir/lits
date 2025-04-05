import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'

const lits = new Lits()
describe('c:factorial', () => {
  it('should return the factorial of a number', () => {
    expect(lits.run('c:factorial(0)')).toEqual(1)
    expect(lits.run('c:factorial(1)')).toEqual(1)
    expect(lits.run('c:factorial(2)')).toEqual(2)
    expect(lits.run('c:factorial(3)')).toEqual(6)
    expect(lits.run('c:factorial(4)')).toEqual(24)
    expect(lits.run('c:factorial(5)')).toEqual(120)
    expect(lits.run('c:factorial(6)')).toEqual(720)
    expect(lits.run('c:factorial(7)')).toEqual(5040)
    expect(lits.run('c:factorial(8)')).toEqual(40320)
    expect(lits.run('c:factorial(9)')).toEqual(362880)
    expect(lits.run('c:factorial(10)')).toEqual(3628800)
  })
})
