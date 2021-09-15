import { lispish } from '../../../src'

describe('regexp functions', () => {
  describe('regexp', () => {
    test('samples', () => {
      expect(lispish(`(regexp "^abc$")`)).toEqual(/^abc$/)
      expect(lispish(`(regexp "^abc$" "gi")`)).toEqual(/^abc$/gi)
      expect(lispish(`(regexp "^abc$" "ig")`)).toEqual(/^abc$/gi)
      expect(lispish(`(regexp "")`)).toEqual(/(?:)/)
      expect(() => lispish(`(regexp "(")`)).toThrow()
      expect(() => lispish(`(regexp)`)).toThrow()
      expect(() => lispish(`(regexp 1)`)).toThrow()
      expect(() => lispish(`(regexp null)`)).toThrow()
      expect(() => lispish(`(regexp undefined)`)).toThrow()
      expect(() => lispish(`(regexp true)`)).toThrow()
      expect(() => lispish(`(regexp false)`)).toThrow()
      expect(() => lispish(`(regexp (list))`)).toThrow()
      expect(() => lispish(`(regexp (object))`)).toThrow()
      expect(() => lispish(`(regexp "" "ab")`)).toThrow()
      expect(() => lispish(`(regexp "abc" "g" "extra")`)).toThrow()
    })
  })

  describe('match', () => {
    test('samples', () => {
      expect(lispish(`(match (regexp "^abc$") "abc")`)).toEqual(['abc'])
      expect(lispish(`(match (regexp "^abc$") "abx")`)).toBeUndefined()
      expect(lispish(`(match (regexp "^(a)bc$") "abc")`)).toEqual(['abc', 'a'])
      expect(lispish(`(match (regexp "^(A)BC$" "i") "abc")`)).toEqual(['abc', 'a'])
      expect(() => lispish(`(match (regexp "^abc$") 1)`)).toThrow()
      expect(() => lispish(`(match (regexp "^abc$") null)`)).toThrow()
      expect(() => lispish(`(match (regexp "^abc$") undefined)`)).toThrow()
      expect(() => lispish(`(match (regexp "^abc$") true)`)).toThrow()
      expect(() => lispish(`(match (regexp "^abc$") false)`)).toThrow()
      expect(() => lispish(`(match (regexp "^abc$") (list))`)).toThrow()
      expect(() => lispish(`(match (regexp "^abc$") (object))`)).toThrow()
    })
  })

  describe('replace', () => {
    test('samples', () => {
      expect(lispish(`(replace "abcabcABCABC" (regexp "^abc") "ABC")`)).toEqual('ABCabcABCABC')
      expect(lispish(`(replace "abcabcABCABC" (regexp "a") "A")`)).toEqual('AbcabcABCABC')
      expect(lispish(`(replace "abcabcABCABC" (regexp "a" "g") "A")`)).toEqual('AbcAbcABCABC')
      expect(lispish(`(replace "abcabcABCABC" (regexp "a" "gi") "-")`)).toEqual('-bc-bc-BC-BC')
      expect(() => lispish(`(replace "abcabcABCABC" (regexp "^abc$") 1)`)).toThrow()
      expect(() => lispish(`(replace "abcabcABCABC" (regexp "^abc$") null)`)).toThrow()
      expect(() => lispish(`(replace "abcabcABCABC" (regexp "^abc$") undefined)`)).toThrow()
      expect(() => lispish(`(replace "abcabcABCABC" (regexp "^abc$") true)`)).toThrow()
      expect(() => lispish(`(replace "abcabcABCABC" (regexp "^abc$") false)`)).toThrow()
      expect(() => lispish(`(replace "abcabcABCABC" (regexp "^abc$") (list))`)).toThrow()
      expect(() => lispish(`(replace "abcabcABCABC" (regexp "^abc$") (object))`)).toThrow()
    })
  })
})
