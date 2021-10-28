import { Lits } from '../src'

let lits: Lits

beforeEach(() => {
  lits = new Lits()
})

describe(`regexpShorthand`, () => {
  test(`samples`, () => {
    expect(lits.run(`#" "g`)).toEqual(/ /g)
    expect(lits.run(`#"a"gi`)).toEqual(/a/gi)
    expect(lits.run(`#"a"ig`)).toEqual(/a/gi)
    expect(lits.run(`#"a"i`)).toEqual(/a/i)
    expect(() => lits.run(`#"a"is`)).toThrow()
    expect(() => lits.run(`#"a"s`)).toThrow()
    expect(() => lits.run(`#"a"ii`)).toThrow()
    expect(() => lits.run(`#"a"gg`)).toThrow()
    expect(lits.run(`#"^abc"`)).toEqual(/^abc/)
  })
})
