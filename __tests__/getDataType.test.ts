import { Lits, LitsParams } from '../src'
import { Type } from '../src/types/Type'

const lits = new Lits()

describe(`Evaluate data types`, () => {
  test(`simple expression`, () => {
    const litsParams: LitsParams = {
      globals: {
        x: Type.positiveInteger,
      },
    }
    const result = lits.run(`(+ 10.1 x)`, litsParams)
    Type.assertType(result, undefined)
  })
})
