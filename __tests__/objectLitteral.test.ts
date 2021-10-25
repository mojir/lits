/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lispish } from '../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`object litterals`, () => {
  test(`samples`, () => {
    expect(lispish.run(`{:1 1, :2 2}`)).toEqual({ 1: 1, 2: 2 })
    expect(lispish.run(`{}`)).toEqual({})
  })
})
