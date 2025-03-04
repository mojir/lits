import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src'
import { regexpEquals } from '../../testUtils'

describe('regexp functions', () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    describe('regexp', () => {
      it('samples', () => {
        expect(regexpEquals(lits.run('(regexp "^abc$")'), /^abc$/)).toBe(true)
        expect(regexpEquals(lits.run('#"^abc$"'), /^abc$/)).toBe(true)
        expect(regexpEquals(lits.run('(regexp "^abc$" "gi")'), /^abc$/gi)).toBe(true)
        expect(regexpEquals(lits.run('(regexp "^abc$" "ig")'), /^abc$/gi)).toBe(true)
        // eslint-disable-next-line prefer-regex-literals
        expect(regexpEquals(lits.run('(regexp "")'), new RegExp(''))).toBe(true)
        expect(() => lits.run('(regexp "(")')).toThrow()
        expect(() => lits.run('(regexp)')).toThrow()
        expect(() => lits.run('(regexp 1)')).toThrow()
        expect(() => lits.run('(regexp nil)')).toThrow()
        expect(() => lits.run('(regexp undefined)')).toThrow()
        expect(() => lits.run('(regexp true)')).toThrow()
        expect(() => lits.run('(regexp false)')).toThrow()
        expect(() => lits.run('(regexp [])')).toThrow()
        expect(() => lits.run('(regexp (object))')).toThrow()
        expect(() => lits.run('(regexp "" "ab")')).toThrow()
        expect(() => lits.run('(regexp "abc" :g "extra")')).toThrow()
      })

      it('email regexp', () => {
        expect(
          lits.run(
            `
          (defn email? [string] (boolean (match string #"^(?:[a-z0-9!#$%&'*+/=?^_\`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_\`{|}~-]+)*|'(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*')@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])$")))
          (email? "albert.mojir@gmail.com"),
          `,
          ),
        ).toBe(true)
      })
      it('regexp mathcing .', () => {
        expect(
          lits.run(
            `
          (defn dot? [string] (boolean (match string #"^\\.$")))
          (dot? "."),
          `,
          ),
        ).toBe(true)
        expect(
          lits.run(
            `
          (defn dot? [string] (boolean (match string (regexp "^\\\\.$"))))
          (dot? "."),
          `,
          ),
        ).toBe(true)
      })
    })

    describe('match', () => {
      it('samples', () => {
        expect(lits.run('(match "abc" (regexp "^abc$"))')).toEqual(['abc'])
        expect(lits.run('(match "abx" (regexp "^abc$"))')).toBeNull()
        expect(lits.run('(match "abc" (regexp "^(a)bc$"))')).toEqual(['abc', 'a'])
        expect(lits.run('(match "abc" (regexp "^(A)BC$" :i))')).toEqual(['abc', 'a'])
        expect(lits.run('(match nil (regexp "^abc$"))')).toBeNull()
        expect(lits.run('(match 1 (regexp "^abc$"))')).toBeNull()
        expect(lits.run('(match true (regexp "^abc$"))')).toBeNull()
        expect(lits.run('(match false (regexp "^abc$"))')).toBeNull()
        expect(lits.run('(match [] (regexp "^abc$"))')).toBeNull()
        expect(lits.run('(match (object) (regexp "^abc$"))')).toBeNull()

        expect(() => lits.run('(match undefined (regexp "^abc$"))')).toThrow()
        expect(() => lits.run('(match (regexp "^abc$"))')).toThrow()
        expect(() => lits.run('(match "asd")')).toThrow()
        expect(() => lits.run('(match :x (regexp "^abc$") :x)')).toThrow()
      })
    })

    describe('replace', () => {
      it('samples', () => {
        expect(lits.run('(replace "abcabcABCABC" (regexp "^abc") "ABC")')).toEqual('ABCabcABCABC')
        expect(lits.run('(replace "abcabcABCABC" (regexp :a) :A)')).toEqual('AbcabcABCABC')
        expect(lits.run('(replace "abcabcABCABC" (regexp :a :g) :A)')).toEqual('AbcAbcABCABC')
        expect(lits.run('(replace "abcabcABCABC" (regexp :a "gi") "-")')).toEqual('-bc-bc-BC-BC')
        expect(() => lits.run('(replace "abcabcABCABC" (regexp "^abc$") 1)')).toThrow()
        expect(() => lits.run('(replace "abcabcABCABC" (regexp "^abc$") nil)')).toThrow()
        expect(() => lits.run('(replace "abcabcABCABC" (regexp "^abc$") undefined)')).toThrow()
        expect(() => lits.run('(replace "abcabcABCABC" (regexp "^abc$") true)')).toThrow()
        expect(() => lits.run('(replace "abcabcABCABC" (regexp "^abc$") false)')).toThrow()
        expect(() => lits.run('(replace "abcabcABCABC" (regexp "^abc$") [])')).toThrow()
        expect(() => lits.run('(replace "abcabcABCABC" (regexp "^abc$") (object))')).toThrow()
      })
    })
  }
})
