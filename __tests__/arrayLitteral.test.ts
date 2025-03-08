import { describe, expect, it } from 'vitest'
import { Lits } from '../src'

describe('array literals', () => {
  for (const lits of [new Lits({ polish: true }), new Lits({ debug: true, polish: true })]) {
    it('samples', () => {
      expect(lits.run('[1 2 3]')).toEqual([1, 2, 3])
      expect(lits.run('[:1 null]')).toEqual(['1', null])
      expect(lits.run('[]')).toEqual([])
    })
  }
})
