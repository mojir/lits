import { Lispish } from '../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`regexpShorthand`, () => {
  test(`samples`, () => {
    expect(lispish.run(`#" "g`)).toEqual(/ /g)
    expect(lispish.run(`#"a"gi`)).toEqual(/a/gi)
    expect(lispish.run(`#"a"ig`)).toEqual(/a/gi)
    expect(lispish.run(`#"a"i`)).toEqual(/a/i)
    expect(() => lispish.run(`#"a"is`)).toThrow()
    expect(() => lispish.run(`#"a"s`)).toThrow()
    expect(() => lispish.run(`#"a"ii`)).toThrow()
    expect(() => lispish.run(`#"a"gg`)).toThrow()
    expect(lispish.run(`#"^abc"`)).toEqual(/^abc/)
  })
})
