import { Lits } from '../src'

let lits: Lits

beforeEach(() => {
  lits = new Lits()
})

describe(`array litterals`, () => {
  test(`samples`, () => {
    expect(lits.run(`[1 2 3]`)).toEqual([1, 2, 3])
    expect(lits.run(`[:1 nil]`)).toEqual([`1`, null])
    expect(lits.run(`[]`)).toEqual([])
  })
})
