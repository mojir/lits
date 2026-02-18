import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { numberTheoryNamespace } from '..'
import { LitsError } from '../../../../errors'

const lits = new Lits({ namespaces: [numberTheoryNamespace] })

function runNth(code: string) {
  return lits.run(`let nt = import("Number-Theory"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('collatz', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:collatz-seq(11)')).toEqual([
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
    expect(() => runNth('nth:collatz-seq(0)')).toThrow(LitsError)
  })
})
