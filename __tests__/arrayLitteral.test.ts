/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lispish } from '../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`array litterals`, () => {
  test(`samples`, () => {
    expect(lispish.run(`[1 2 3]`)).toEqual([1, 2, 3])
  })
})
