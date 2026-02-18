import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { numberTheoryNamespace } from '..'
import { LitsError } from '../../../../errors'

const lits = new Lits({ namespaces: [numberTheoryNamespace] })

function runNth(code: string) {
  return lits.run(`let nt = import("Number-Theory"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('juggler', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:juggler-seq(10)')).toEqual([
      10,
      3,
      5,
      11,
      36,
      6,
      2,
      1,
    ])
    expect(() => runNth('nth:juggler-seq(0)')).toThrow(LitsError)
    expect(() => runNth('nth:juggler-seq(58025)')).toThrow(LitsError)
  })
})
