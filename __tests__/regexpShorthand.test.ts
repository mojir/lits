import { describe, expect, it } from 'vitest'
import { Lits } from '../src/Lits/Lits'
import { LitsError } from '../src/errors'
import { regexpEquals } from './testUtils'

describe('regexpShorthand', () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    it('samples', () => {
      expect(regexpEquals(lits.run('#" "g'), / /g)).toBe(true)
      expect(regexpEquals(lits.run('#"a"gi'), /a/gi)).toBe(true)
      expect(regexpEquals(lits.run('#"a"ig'), /a/gi)).toBe(true)
      expect(regexpEquals(lits.run('#"a"i'), /a/i)).toBe(true)
      expect(regexpEquals(lits.run('#"^abc"'), /^abc/)).toBe(true)
      expect(() => lits.run('#"a"is')).toThrow(LitsError)
      expect(() => lits.run('#"a"s')).toThrow(LitsError)
      expect(() => lits.run('#"a"ii')).toThrow(LitsError)
      expect(() => lits.run('#"a"gg')).toThrow(LitsError)
    })
  }
})
