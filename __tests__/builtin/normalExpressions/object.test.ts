/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lispish } from '../../../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`object functions`, () => {
  describe(`object`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(object)`)).toEqual({})
      expect(lispish.run(`(object "x" 1)`)).toEqual({ x: 1 })
      expect(lispish.run(`(object "x" undefined)`)).toEqual({ x: undefined })
      expect(lispish.run(`(object "x" 1 "x" 2)`)).toEqual({ x: 2 })
      expect(lispish.run(`(object "a" null "b" true "c" false "d" undefined "e" (object "x" []))`)).toEqual({
        a: null,
        b: true,
        c: false,
        d: undefined,
        e: { x: [] },
      })
      expect(lispish.run(`(let [a "a"] (object a 1))`)).toEqual({ a: 1 })
      expect(() => lispish.run(`(object "x")`)).toThrow()
      expect(() => lispish.run(`(object "x")`)).toThrow()
      expect(() => lispish.run(`(object "x" 1 "y")`)).toThrow()
      expect(() => lispish.run(`(object 0 1)`)).toThrow()
      expect(() => lispish.run(`(object true 1)`)).toThrow()
      expect(() => lispish.run(`(object false 1)`)).toThrow()
      expect(() => lispish.run(`(object null 1)`)).toThrow()
      expect(() => lispish.run(`(object undefined 1)`)).toThrow()
      expect(() => lispish.run(`(object [] 1)`)).toThrow()
      expect(() => lispish.run(`(object (object) 1)`)).toThrow()
    })
  })

  describe(`keys`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(keys (object))`)).toEqual([])
      expect(lispish.run(`(keys (object "x" 1))`)).toEqual([`x`])
      expect(lispish.run(`(keys (object "x" undefined "y" 2))`)).toEqual([`x`, `y`])
      expect(() => lispish.run(`(keys)`)).toThrow()
      expect(() => lispish.run(`(keys (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(keys 0)`)).toThrow()
      expect(() => lispish.run(`(keys true)`)).toThrow()
      expect(() => lispish.run(`(keys false)`)).toThrow()
      expect(() => lispish.run(`(keys null)`)).toThrow()
      expect(() => lispish.run(`(keys undefined)`)).toThrow()
      expect(() => lispish.run(`(keys [1])`)).toThrow()
    })
  })

  describe(`vals`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(vals (object))`)).toEqual([])
      expect(lispish.run(`(vals (object "x" 1))`)).toEqual([1])
      expect(lispish.run(`(vals (object "x" undefined "y" 2))`)).toEqual([undefined, 2])
      expect(() => lispish.run(`(vals)`)).toThrow()
      expect(() => lispish.run(`(vals (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(vals 0)`)).toThrow()
      expect(() => lispish.run(`(vals true)`)).toThrow()
      expect(() => lispish.run(`(vals false)`)).toThrow()
      expect(() => lispish.run(`(vals null)`)).toThrow()
      expect(() => lispish.run(`(vals undefined)`)).toThrow()
      expect(() => lispish.run(`(vals [1])`)).toThrow()
    })
  })

  describe(`entries`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(entries (object))`)).toEqual([])
      expect(lispish.run(`(entries (object "x" 1))`)).toEqual([[`x`, 1]])
      expect(lispish.run(`(entries (object "x" undefined "y" 2))`)).toEqual([
        [`x`, undefined],
        [`y`, 2],
      ])
      expect(() => lispish.run(`(entries)`)).toThrow()
      expect(() => lispish.run(`(entries (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(entries 0)`)).toThrow()
      expect(() => lispish.run(`(entries true)`)).toThrow()
      expect(() => lispish.run(`(entries false)`)).toThrow()
      expect(() => lispish.run(`(entries null)`)).toThrow()
      expect(() => lispish.run(`(entries undefined)`)).toThrow()
      expect(() => lispish.run(`(entries [1])`)).toThrow()
    })
  })

  describe(`find`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(find (object "x" 1) "a")`)).toBeUndefined()
      expect(lispish.run(`(find (object "x" 1) "x")`)).toEqual([`x`, 1])
      expect(lispish.run(`(find (object "x" 1 "y" 2) "x")`)).toEqual([`x`, 1])
      expect(() => lispish.run(`(find)`)).toThrow()
      expect(() => lispish.run(`(find (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(find (object "x") null)`)).toThrow()
      expect(() => lispish.run(`(find (object "x") true)`)).toThrow()
      expect(() => lispish.run(`(find (object "x") false)`)).toThrow()
      expect(() => lispish.run(`(find (object "x") undefined)`)).toThrow()
      expect(() => lispish.run(`(find (object "x") "x" "y")`)).toThrow()
      expect(() => lispish.run(`(find (object "x"))`)).toThrow()
      expect(() => lispish.run(`(find [] "x")`)).toThrow()
      expect(() => lispish.run(`(find null "x")`)).toThrow()
      expect(() => lispish.run(`(find false "x")`)).toThrow()
      expect(() => lispish.run(`(find 4 "x")`)).toThrow()
    })
  })

  describe(`dissoc`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(dissoc (object) "x")`)).toBeUndefined()
      expect(lispish.run(`(dissoc (object "x" 1) "x")`)).toBe(1)
      expect(lispish.run(`(dissoc (object "x" 1) "")`)).toBeUndefined()
      expect(lispish.run(`(dissoc (object "x" (object)) "x")`)).toEqual({})
      expect(() => lispish.run(`(dissoc (object "x" 1) 1)`)).toThrow()
      expect(() => lispish.run(`(dissoc)`)).toThrow()
      expect(() => lispish.run(`(dissoc (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(dissoc 0 "x")`)).toThrow()
      expect(() => lispish.run(`(dissoc true "x")`)).toThrow()
      expect(() => lispish.run(`(dissoc false "x")`)).toThrow()
      expect(() => lispish.run(`(dissoc null "x")`)).toThrow()
      expect(() => lispish.run(`(dissoc undefined "x")`)).toThrow()
      expect(() => lispish.run(`(dissoc [1] "x")`)).toThrow()
    })
    test(`delete atribute`, () => {
      const program = `
        (def obj (object "x" 10))
        (dissoc obj "x")
        obj
      `
      expect(lispish.run(program)).toEqual({})
    })

    test(`delete unexisting attribute`, () => {
      const program = `
        (def obj (object "x" 10))
        (dissoc obj "y")
        obj
      `
      expect(lispish.run(program)).toEqual({ x: 10 })
    })
  })

  describe(`merge`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(merge (object "x" 10))`)).toEqual({ x: 10 })
      expect(lispish.run(`(merge (object "x" 10) (object "y" 20))`)).toEqual({ x: 10, y: 20 })
      expect(lispish.run(`(merge (object "x" 10) (object "x" 5))`)).toEqual({ x: 5 })
      expect(lispish.run(`(merge (object) (object "x" 10) (object "y" 10) (object "z" 10))`)).toEqual({
        x: 10,
        y: 10,
        z: 10,
      })
      expect(() => lispish.run(`(merge)`)).toThrow()
      expect(() => lispish.run(`(merge 1)`)).toThrow()
      expect(() => lispish.run(`(merge "1")`)).toThrow()
      expect(() => lispish.run(`(merge true)`)).toThrow()
      expect(() => lispish.run(`(merge false)`)).toThrow()
      expect(() => lispish.run(`(merge null)`)).toThrow()
      expect(() => lispish.run(`(merge undefined)`)).toThrow()
      expect(() => lispish.run(`(merge (array))`)).toThrow()
    })

    test(`merge returns new object`, () => {
      const program = `
        (def obj1 (object "x" 10))
        (def obj2 (merge obj1))
        (not= obj1 obj2)
      `
      expect(lispish.run(program)).toBe(true)
    })
  })
})
