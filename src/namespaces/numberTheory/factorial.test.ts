import { describe, expect, it } from 'vitest'
import { Lits } from '../../Lits/Lits'
import { LitsError } from '../../errors'
import { factorialOf } from './factorial'

const lits = new Lits()

function runNth(code: string) {
  return lits.run(`let nt = import("TEMP-nth"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('factorial', () => {
  it('should return the factorial of a number', () => {
    expect(() => factorialOf(-1)).toThrow()
    expect(factorialOf(0)).toEqual(1)
    expect(factorialOf(20)).toBe(2432902008176640000)
  })
})
describe('nth:factorial', () => {
  it('should return the factorial of a number', () => {
    expect(runNth('nth:factorial(0)')).toEqual(1)
    expect(runNth('nth:factorial(1)')).toEqual(1)
    expect(runNth('nth:factorial(2)')).toEqual(2)
    expect(runNth('nth:factorial(3)')).toEqual(6)
    expect(runNth('nth:factorial(4)')).toEqual(24)
    expect(runNth('nth:factorial(5)')).toEqual(120)
    expect(runNth('nth:factorial(6)')).toEqual(720)
    expect(runNth('nth:factorial(7)')).toEqual(5040)
    expect(runNth('nth:factorial(8)')).toEqual(40320)
    expect(runNth('nth:factorial(9)')).toEqual(362880)
    expect(runNth('nth:factorial(10)')).toEqual(3628800)
    expect(runNth('nth:factorial(20)')).toEqual(2432902008176640000)
    expect(() => runNth('nth:factorial(171)')).toThrow(LitsError)
  })
})
