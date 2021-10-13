import { Lispish } from '../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`fnShorthand`, () => {
  test(`samples`, () => {
    expect(lispish.run(`(#(identity "Kalle"))`)).toBe(`Kalle`)
    expect(lispish.run(`(#(+ %1 %2) 1 2)`)).toBe(3)
    expect(lispish.run(`(def x 10) (#(+ %1 %2 x) 1 2)`)).toBe(13)
    expect(lispish.run(`(#(+ %1 %20) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)`)).toBe(21)
    expect(() => lispish.run(`#(+ %1 %21)`)).toThrow()
    expect(() => lispish.run(`(#(+ %1 %2) 1)`)).toThrow()
    expect(() => lispish.run(`(#(+ %1 %2) 1)`)).toThrow()
    expect(() => lispish.run(`(#(+ % %2) 1 2)`)).toThrow()
    expect(() => lispish.run(`(#(+ (#(+ %1 1) 1) %2) 1 2)`)).toThrow()
  })
})
