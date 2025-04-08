import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'

const lits = new Lits()
describe('bernoulli', () => {
  it('should return the correct sequence', () => {
    expect((lits.run('nth:bernoulli-seq(7)') as number[])[0]).toBeCloseTo(1, 10)
    expect((lits.run('nth:bernoulli-seq(7)') as number[])[1]).toBe(-0.5)
    expect((lits.run('nth:bernoulli-seq(7)') as number[])[2]).toBeCloseTo(1 / 6, 10)
    expect((lits.run('nth:bernoulli-seq(7)') as number[])[3]).toBe(0)
    expect((lits.run('nth:bernoulli-seq(7)') as number[])[4]).toBeCloseTo(-1 / 30, 10)
    expect((lits.run('nth:bernoulli-seq(7)') as number[])[5]).toBe(0)
    expect((lits.run('nth:bernoulli-seq(7)') as number[])[6]).toBeCloseTo(1 / 42, 10)
    expect(() => lits.run('nth:bernoulli-seq(0)')).toThrow()
  })

  it('should return the correct nth term', () => {
    expect(lits.run('nth:bernoulli-nth(1)')).toBe(1)
    expect(lits.run('nth:bernoulli-nth(2)')).toBe(-0.5)
    expect(lits.run('nth:bernoulli-nth(3)')).toBe(1 / 6)
    expect(lits.run('nth:bernoulli-nth(4)')).toBe(0)
    expect(lits.run('nth:bernoulli-nth(29)')).toBeCloseTo(-27298230.14735771, 10)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('nth:bernoulli-take-while(-> $ != 0)')).toEqual([1, -0.5, 1 / 6])
  })
})
