import { describe, expect, it } from 'vitest'
import { Lits } from '../src'

describe('object literals', () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    it('samples', () => {
      expect(lits.run('{:1 1, :2 2}')).toEqual({ 1: 1, 2: 2 })
      expect(lits.run('{}')).toEqual({})
    })
  }
})
