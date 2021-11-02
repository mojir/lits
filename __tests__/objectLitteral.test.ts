/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lits } from '../src'

let lits: Lits

beforeEach(() => {
  lits = new Lits({ debug: true })
})

describe(`object litterals`, () => {
  test(`samples`, () => {
    expect(lits.run(`{:1 1, :2 2}`)).toEqual({ 1: 1, 2: 2 })
    expect(lits.run(`{}`)).toEqual({})
  })
})
