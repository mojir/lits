import { Lispish } from '../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`regexpShorthand`, () => {
  test(`samples`, () => {
    expect(lispish.run(`#"^abc"`)).toEqual(/^abc/)
  })
})
