/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lits } from '../src'

describe(`object literals`, () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    test(`samples`, () => {
      expect(lits.run(`{:1 1, :2 2}`)).toEqual({ 1: 1, 2: 2 })
      expect(lits.run(`{}`)).toEqual({})
    })
  }
})
