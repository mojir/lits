import { Lispish } from '../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`array litterals`, () => {
  test(`samples`, () => {
    expect(lispish.run(`[1 2 3]`)).toEqual([1, 2, 3])
    expect(lispish.run(`["1" null undefined]`)).toEqual([`1`, null, undefined])
    expect(lispish.run(`[]`)).toEqual([])
  })
})
