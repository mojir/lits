import { Lispish } from '../../../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`regexp functions`, () => {
  describe(`regexp`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(regexp "^abc$")`)).toEqual(/^abc$/)
      expect(lispish.run(`(regexp "^abc$" "gi")`)).toEqual(/^abc$/gi)
      expect(lispish.run(`(regexp "^abc$" "ig")`)).toEqual(/^abc$/gi)
      expect(lispish.run(`(regexp "")`)).toEqual(/(?:)/)
      expect(() => lispish.run(`(regexp "(")`)).toThrow()
      expect(() => lispish.run(`(regexp)`)).toThrow()
      expect(() => lispish.run(`(regexp 1)`)).toThrow()
      expect(() => lispish.run(`(regexp null)`)).toThrow()
      expect(() => lispish.run(`(regexp undefined)`)).toThrow()
      expect(() => lispish.run(`(regexp true)`)).toThrow()
      expect(() => lispish.run(`(regexp false)`)).toThrow()
      expect(() => lispish.run(`(regexp [])`)).toThrow()
      expect(() => lispish.run(`(regexp (object))`)).toThrow()
      expect(() => lispish.run(`(regexp "" "ab")`)).toThrow()
      expect(() => lispish.run(`(regexp "abc" "g" "extra")`)).toThrow()
    })
  })

  describe(`match`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(match (regexp "^abc$") "abc")`)).toEqual([`abc`])
      expect(lispish.run(`(match (regexp "^abc$") "abx")`)).toBeUndefined()
      expect(lispish.run(`(match (regexp "^(a)bc$") "abc")`)).toEqual([`abc`, `a`])
      expect(lispish.run(`(match (regexp "^(A)BC$" "i") "abc")`)).toEqual([`abc`, `a`])
      expect(() => lispish.run(`(match (regexp "^abc$") 1)`)).toThrow()
      expect(() => lispish.run(`(match (regexp "^abc$") null)`)).toThrow()
      expect(() => lispish.run(`(match (regexp "^abc$") undefined)`)).toThrow()
      expect(() => lispish.run(`(match (regexp "^abc$") true)`)).toThrow()
      expect(() => lispish.run(`(match (regexp "^abc$") false)`)).toThrow()
      expect(() => lispish.run(`(match (regexp "^abc$") [])`)).toThrow()
      expect(() => lispish.run(`(match (regexp "^abc$") (object))`)).toThrow()
    })
  })

  describe(`test`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(test (regexp "^abc$") "abc")`)).toBe(true)
      expect(lispish.run(`(test (regexp "^abc$") "abx")`)).toBe(false)
      expect(lispish.run(`(test (regexp "^(a)bc$") "abc")`)).toBe(true)
      expect(lispish.run(`(test (regexp "^(A)BC$" "i") "abc")`)).toBe(true)
      expect(() => lispish.run(`(test (regexp "^abc$") 1)`)).toThrow()
      expect(() => lispish.run(`(test (regexp "^abc$") null)`)).toThrow()
      expect(() => lispish.run(`(test (regexp "^abc$") undefined)`)).toThrow()
      expect(() => lispish.run(`(test (regexp "^abc$") true)`)).toThrow()
      expect(() => lispish.run(`(test (regexp "^abc$") false)`)).toThrow()
      expect(() => lispish.run(`(test (regexp "^abc$") [])`)).toThrow()
      expect(() => lispish.run(`(test (regexp "^abc$") (object))`)).toThrow()
    })
  })

  describe(`replace`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(replace "abcabcABCABC" (regexp "^abc") "ABC")`)).toEqual(`ABCabcABCABC`)
      expect(lispish.run(`(replace "abcabcABCABC" (regexp "a") "A")`)).toEqual(`AbcabcABCABC`)
      expect(lispish.run(`(replace "abcabcABCABC" (regexp "a" "g") "A")`)).toEqual(`AbcAbcABCABC`)
      expect(lispish.run(`(replace "abcabcABCABC" (regexp "a" "gi") "-")`)).toEqual(`-bc-bc-BC-BC`)
      expect(() => lispish.run(`(replace "abcabcABCABC" (regexp "^abc$") 1)`)).toThrow()
      expect(() => lispish.run(`(replace "abcabcABCABC" (regexp "^abc$") null)`)).toThrow()
      expect(() => lispish.run(`(replace "abcabcABCABC" (regexp "^abc$") undefined)`)).toThrow()
      expect(() => lispish.run(`(replace "abcabcABCABC" (regexp "^abc$") true)`)).toThrow()
      expect(() => lispish.run(`(replace "abcabcABCABC" (regexp "^abc$") false)`)).toThrow()
      expect(() => lispish.run(`(replace "abcabcABCABC" (regexp "^abc$") [])`)).toThrow()
      expect(() => lispish.run(`(replace "abcabcABCABC" (regexp "^abc$") (object))`)).toThrow()
    })
  })
})
