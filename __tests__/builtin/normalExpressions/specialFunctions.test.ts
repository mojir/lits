import { Lispish } from '../../../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`specialFunctions`, () => {
  describe(`string as function`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(def person {"firstName" "Albert", "lastName", "Mojir"}) ("firstName" person)`)).toBe(
        `Albert`,
      )
      expect(lispish.run(`("firstName" {"firstName" "Albert", "lastName", "Mojir"})`)).toBe(`Albert`)
      expect(lispish.run(`("lastName" {"firstName" "Albert", "lastName", "Mojir"})`)).toBe(`Mojir`)
      expect(lispish.run(`("x" {"firstName" "Albert", "lastName", "Mojir"})`)).toBeUndefined()
      expect(lispish.run(`("Albert" 2)`)).toBe(`b`)
      expect(lispish.run(`("Albert" 12)`)).toBeUndefined()
      expect(() => lispish.run(`("firstName" {"firstName" "Albert", "lastName", "Mojir"} 1)`)).toThrow()
      expect(() => lispish.run(`({"firstName" "Albert", "lastName", "Mojir"})`)).toThrow()
      expect(() => lispish.run(`(1 {"firstName" "Albert", "lastName", "Mojir"})`)).toThrow()
      expect(() => lispish.run(`(null {"firstName" "Albert", "lastName", "Mojir"})`)).toThrow()
      expect(() => lispish.run(`(undefined {"firstName" "Albert", "lastName", "Mojir"})`)).toThrow()
      expect(() => lispish.run(`(true {"firstName" "Albert", "lastName", "Mojir"})`)).toThrow()
      expect(() => lispish.run(`(false {"firstName" "Albert", "lastName", "Mojir"})`)).toThrow()
      expect(() => lispish.run(`({} {"firstName" "Albert", "lastName", "Mojir"})`)).toThrow()
      expect(() => lispish.run(`([] {"firstName" "Albert", "lastName", "Mojir"})`)).toThrow()
    })
  })

  describe(`object as function`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(def person {"firstName" "Albert", "lastName", "Mojir"}) (person "firstName")`)).toBe(
        `Albert`,
      )
      expect(lispish.run(`({"firstName" "Albert", "lastName", "Mojir"} "firstName")`)).toBe(`Albert`)
      expect(lispish.run(`({"firstName" "Albert", "lastName", "Mojir"} "lastName")`)).toBe(`Mojir`)
      expect(lispish.run(`({"firstName" "Albert", "lastName", "Mojir"} "x")`)).toBeUndefined()
      expect(() => lispish.run(`({"firstName" "Albert", "lastName", "Mojir"})`)).toThrow()
      expect(() => lispish.run(`({"firstName" "Albert", "lastName", "Mojir"} 1)`)).toThrow()
      expect(() => lispish.run(`({"firstName" "Albert", "lastName", "Mojir"} null)`)).toThrow()
      expect(() => lispish.run(`({"firstName" "Albert", "lastName", "Mojir"} undefined)`)).toThrow()
      expect(() => lispish.run(`({"firstName" "Albert", "lastName", "Mojir"} true)`)).toThrow()
      expect(() => lispish.run(`({"firstName" "Albert", "lastName", "Mojir"} false)`)).toThrow()
      expect(() => lispish.run(`({"firstName" "Albert", "lastName", "Mojir"} {})`)).toThrow()
      expect(() => lispish.run(`({"firstName" "Albert", "lastName", "Mojir"} [])`)).toThrow()
    })
  })

  describe(`array as function`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(def nameArray ["Albert" "Mojir"]) (nameArray 0)`)).toBe(`Albert`)
      expect(lispish.run(`(["Albert" "Mojir"] 0)`)).toBe(`Albert`)
      expect(lispish.run(`((cons 1 [2 3]) 1)`)).toBe(2)
      expect(lispish.run(`("Albert" 0)`)).toBe(`A`)
      expect(lispish.run(`("Albert" 10)`)).toBeUndefined()
      expect(() => lispish.run(`(["Albert" "Mojir"])`)).toThrow()
      expect(() => lispish.run(`(["Albert" "Mojir"] "0")`)).toThrow()
      expect(() => lispish.run(`(["Albert" "Mojir"] 0 1)`)).toThrow()
      expect(() => lispish.run(`((+ 1 2) 0)`)).toThrow()
    })
  })

  describe(`number as function`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(def nameArray ["Albert" "Mojir"]) (0 nameArray)`)).toBe(`Albert`)
      expect(lispish.run(`(0 ["Albert" "Mojir"])`)).toBe(`Albert`)
      expect(lispish.run(`(3 ["Albert" "Mojir"])`)).toBeUndefined()
      expect(lispish.run(`(1 (cons 1 [2 3]))`)).toBe(2)
      expect(lispish.run(`(1 "Albert")`)).toBe(`l`)
      expect(lispish.run(`(10 "Albert")`)).toBeUndefined()
      expect(() => lispish.run(`(["Albert" "Mojir"])`)).toThrow()
      expect(() => lispish.run(`("0" ["Albert" "Mojir"])`)).toThrow()
      expect(() => lispish.run(`(0 1 ["Albert" "Mojir"])`)).toThrow()
      expect(() => lispish.run(`((regexp "kj") "Albert")`)).toThrow()
      expect(() => lispish.run(`(0 (+ 1 2))`)).toThrow()
    })
  })
})
