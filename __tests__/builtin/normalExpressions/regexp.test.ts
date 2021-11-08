import { Lits } from '../../../src'

let lits: Lits

beforeEach(() => {
  lits = new Lits({ debug: true })
})

describe(`regexp functions`, () => {
  describe(`regexp`, () => {
    test(`samples`, () => {
      expect(lits.run(`(regexp '^abc$')`)).toEqual(/^abc$/)
      expect(lits.run(`#'^abc$'`)).toEqual(/^abc$/)
      expect(lits.run(`(regexp '^abc$' 'gi')`)).toEqual(/^abc$/gi)
      expect(lits.run(`(regexp '^abc$' 'ig')`)).toEqual(/^abc$/gi)
      expect(lits.run(`(regexp '')`)).toEqual(/(?:)/)
      expect(() => lits.run(`(regexp '(')`)).toThrow()
      expect(() => lits.run(`(regexp)`)).toThrow()
      expect(() => lits.run(`(regexp 1)`)).toThrow()
      expect(() => lits.run(`(regexp nil)`)).toThrow()
      expect(() => lits.run(`(regexp undefined)`)).toThrow()
      expect(() => lits.run(`(regexp true)`)).toThrow()
      expect(() => lits.run(`(regexp false)`)).toThrow()
      expect(() => lits.run(`(regexp [])`)).toThrow()
      expect(() => lits.run(`(regexp (object))`)).toThrow()
      expect(() => lits.run(`(regexp '' 'ab')`)).toThrow()
      expect(() => lits.run(`(regexp 'abc' :g 'extra')`)).toThrow()
    })

    test(`email regexp`, () => {
      expect(
        lits.run(
          `
          (defn email? [string] (boolean (match (regexp '^(?:[a-z0-9!#$%&\\'*+/=?^_\`{|}~-]+(?:\\.[a-z0-9!#$%&\\'*+/=?^_\`{|}~-]+)*|\\'(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\\')@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])$') string)))
          (email? 'albert.mojir@youcruit.com'),
          `,
        ),
      ).toBe(true)
    })
  })

  describe(`match`, () => {
    test(`samples`, () => {
      expect(lits.run(`(match (regexp '^abc$') 'abc')`)).toEqual([`abc`])
      expect(lits.run(`(match (regexp '^abc$') 'abx')`)).toBeNull()
      expect(lits.run(`(match (regexp '^(a)bc$') 'abc')`)).toEqual([`abc`, `a`])
      expect(lits.run(`(match (regexp '^(A)BC$' :i) 'abc')`)).toEqual([`abc`, `a`])
      expect(() => lits.run(`(match (regexp '^abc$') 1)`)).toThrow()
      expect(() => lits.run(`(match (regexp '^abc$') nil)`)).toThrow()
      expect(() => lits.run(`(match (regexp '^abc$') undefined)`)).toThrow()
      expect(() => lits.run(`(match (regexp '^abc$') true)`)).toThrow()
      expect(() => lits.run(`(match (regexp '^abc$') false)`)).toThrow()
      expect(() => lits.run(`(match (regexp '^abc$') [])`)).toThrow()
      expect(() => lits.run(`(match (regexp '^abc$') (object))`)).toThrow()
    })
  })

  describe(`replace`, () => {
    test(`samples`, () => {
      expect(lits.run(`(replace 'abcabcABCABC' (regexp '^abc') 'ABC')`)).toEqual(`ABCabcABCABC`)
      expect(lits.run(`(replace 'abcabcABCABC' (regexp :a) :A)`)).toEqual(`AbcabcABCABC`)
      expect(lits.run(`(replace 'abcabcABCABC' (regexp :a :g) :A)`)).toEqual(`AbcAbcABCABC`)
      expect(lits.run(`(replace 'abcabcABCABC' (regexp :a 'gi') '-')`)).toEqual(`-bc-bc-BC-BC`)
      expect(() => lits.run(`(replace 'abcabcABCABC' (regexp '^abc$') 1)`)).toThrow()
      expect(() => lits.run(`(replace 'abcabcABCABC' (regexp '^abc$') nil)`)).toThrow()
      expect(() => lits.run(`(replace 'abcabcABCABC' (regexp '^abc$') undefined)`)).toThrow()
      expect(() => lits.run(`(replace 'abcabcABCABC' (regexp '^abc$') true)`)).toThrow()
      expect(() => lits.run(`(replace 'abcabcABCABC' (regexp '^abc$') false)`)).toThrow()
      expect(() => lits.run(`(replace 'abcabcABCABC' (regexp '^abc$') [])`)).toThrow()
      expect(() => lits.run(`(replace 'abcabcABCABC' (regexp '^abc$') (object))`)).toThrow()
    })
  })
})
