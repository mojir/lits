import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src/Lits/Lits'
import { regexpEquals } from '../../testUtils'
import { LitsError } from '../../../src/errors'

describe('regexp functions', () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    describe('regexp', () => {
      it('samples', () => {
        expect(regexpEquals(lits.run('regexp("^abc$")'), /^abc$/)).toBe(true)
        expect(regexpEquals(lits.run('#"^abc$"'), /^abc$/)).toBe(true)
        expect(regexpEquals(lits.run('regexp("^abc$", "gi")'), /^abc$/gi)).toBe(true)
        expect(regexpEquals(lits.run('regexp("^abc$", "ig")'), /^abc$/gi)).toBe(true)
        // eslint-disable-next-line prefer-regex-literals
        expect(regexpEquals(lits.run('regexp("")'), new RegExp(''))).toBe(true)
        expect(() => lits.run('regexp("(")')).toThrow(LitsError)
        expect(() => lits.run('regexp()')).toThrow(LitsError)
        expect(() => lits.run('regexp(1)')).toThrow(LitsError)
        expect(() => lits.run('regexp(null)')).toThrow(LitsError)
        expect(() => lits.run('regexp(undefined)')).toThrow(LitsError)
        expect(() => lits.run('regexp(true)')).toThrow(LitsError)
        expect(() => lits.run('regexp(false)')).toThrow(LitsError)
        expect(() => lits.run('regexp([])')).toThrow(LitsError)
        expect(() => lits.run('regexp(object())')).toThrow(LitsError)
        expect(() => lits.run('regexp("" "ab")')).toThrow(LitsError)
        expect(() => lits.run('regexp("abc" "g" "extra")')).toThrow(LitsError)
      })

      it('email regexp', () => {
        expect(
          lits.run(
            `
          let email? = (string) -> do
            boolean(
              re-match(
                string,
                #"^(?:[a-z0-9!#$%&'*+/=?^_\`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_\`{|}~-]+)*|'(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*')@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])$"))
          end;
          email?("albert.mojir@gmail.com");
          `,
          ),
        ).toBe(true)
      })
      it('regexp mathcing .', () => {
        expect(
          lits.run(
            `
          let dot? = (string) -> do
            boolean(re-match(string, #"^\\.$"))
          end;
          [dot?("."), dot?(",")];
          `,
          ),
        ).toEqual([true, false])
      })
    })

    describe('re-match', () => {
      it('samples', () => {
        expect(lits.run('re-match("abc", regexp("^abc$"))')).toEqual(['abc'])
        expect(lits.run('re-match("abx", regexp("^abc$"))')).toBeNull()
        expect(lits.run('re-match("abc", regexp("^(a)bc$"))')).toEqual(['abc', 'a'])
        expect(lits.run('re-match("abc", regexp("^(A)BC$", "i"))')).toEqual(['abc', 'a'])
        expect(lits.run('re-match(null, regexp("^abc$"))')).toBeNull()
        expect(lits.run('re-match(1, regexp("^abc$"))')).toBeNull()
        expect(lits.run('re-match(true, regexp("^abc$"))')).toBeNull()
        expect(lits.run('re-match(false, regexp("^abc$"))')).toBeNull()
        expect(lits.run('re-match([], regexp("^abc$"))')).toBeNull()
        expect(lits.run('re-match(object(), regexp("^abc$"))')).toBeNull()

        expect(() => lits.run('re-match(regexp("^abc$"))')).toThrow(LitsError)
        expect(() => lits.run('re-match("asd")')).toThrow(LitsError)
        expect(() => lits.run('re-match("x" regexp("^abc$") "x")')).toThrow(LitsError)
      })
    })

    describe('replace-all', () => {
      it('samples', () => {
        expect(lits.run('replace-all("abcabcABCABC", "abc", "ABC")')).toEqual('ABCABCABCABC')
        expect(lits.run('replace-all("abcabcABCABC", regexp("^abc"), "ABC")')).toEqual('ABCabcABCABC')
        expect(lits.run('replace-all("abcabcABCABC", regexp("a"), "A")')).toEqual('AbcAbcABCABC')
        expect(lits.run('replace-all("abcabcABCABC", regexp("a", "g"), "A")')).toEqual('AbcAbcABCABC')
        expect(lits.run('replace-all("abcabcABCABC", regexp("a", "gi"), "-")')).toEqual('-bc-bc-BC-BC')
        expect(() => lits.run('replace-all("abcabcABCABC", regexp("^abc$"), 1)')).toThrow(LitsError)
        expect(() => lits.run('replace-all("abcabcABCABC", regexp("^abc$"), null)')).toThrow(LitsError)
        expect(() => lits.run('replace-all("abcabcABCABC", regexp("^abc$"), true)')).toThrow(LitsError)
        expect(() => lits.run('replace-all("abcabcABCABC", regexp("^abc$"), false)')).toThrow(LitsError)
        expect(() => lits.run('replace-all("abcabcABCABC", regexp("^abc$"), [])')).toThrow(LitsError)
        expect(() => lits.run('replace-all("abcabcABCABC", regexp("^abc$"), {})')).toThrow(LitsError)
      })
    })
    describe('replace', () => {
      it('samples', () => {
        expect(lits.run('replace("abcabcABCABC", "abc", "ABC")')).toEqual('ABCabcABCABC')
        expect(lits.run('replace("abcabcABCABC", regexp("^abc"), "ABC")')).toEqual('ABCabcABCABC')
        expect(lits.run('replace("abcabcABCABC", regexp("a"), "A")')).toEqual('AbcabcABCABC')
        expect(lits.run('replace("abcabcABCABC", regexp("a", "g"), "A")')).toEqual('AbcAbcABCABC')
        expect(lits.run('replace("abcabcABCABC", regexp("a", "gi"), "-")')).toEqual('-bc-bc-BC-BC')
        expect(() => lits.run('replace("abcabcABCABC", regexp("^abc$") 1)')).toThrow(LitsError)
        expect(() => lits.run('replace("abcabcABCABC", regexp("^abc$") null)')).toThrow(LitsError)
        expect(() => lits.run('replace("abcabcABCABC", regexp("^abc$") undefined)')).toThrow(LitsError)
        expect(() => lits.run('replace("abcabcABCABC", regexp("^abc$") true)')).toThrow(LitsError)
        expect(() => lits.run('replace("abcabcABCABC", regexp("^abc$") false)')).toThrow(LitsError)
        expect(() => lits.run('replace("abcabcABCABC", regexp("^abc$") [])')).toThrow(LitsError)
        expect(() => lits.run('replace("abcabcABCABC", regexp("^abc$") object())')).toThrow(LitsError)
      })
    })
  }
})
