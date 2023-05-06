import { Lits } from '../src'

describe(`array literals`, () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    test(`samples`, () => {
      expect(lits.run(`[1 2 3]`)).toEqual([1, 2, 3])
      expect(lits.run(`[:1 nil]`)).toEqual([`1`, null])
      expect(lits.run(`[]`)).toEqual([])
    })
  }
})
