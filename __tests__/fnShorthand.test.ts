import { Lispish } from '../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`fnShorthand`, () => {
  test(`samples`, () => {
    expect(lispish.run(`(#(+ %1 %2) 1 2)`)).toBe(3)
    expect(lispish.run(`(def x 10) (#(+ %1 %2 x) 1 2)`)).toBe(13)
    expect(() => lispish.run(`(#(+ % %2) 1 2)`)).toThrow()
    expect(() => lispish.run(`(#(+ (#(+ %1 1) 1) %2) 1 2)`)).toThrow()
  })
})
