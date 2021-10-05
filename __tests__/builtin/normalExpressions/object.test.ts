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
      expect(lispish.run(`(let ((a "a")) (object a 1))`)).toEqual({ a: 1 })
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

  describe(`values`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(values (object))`)).toEqual([])
      expect(lispish.run(`(values (object "x" 1))`)).toEqual([1])
      expect(lispish.run(`(values (object "x" undefined "y" 2))`)).toEqual([undefined, 2])
      expect(() => lispish.run(`(values)`)).toThrow()
      expect(() => lispish.run(`(values (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(values 0)`)).toThrow()
      expect(() => lispish.run(`(values true)`)).toThrow()
      expect(() => lispish.run(`(values false)`)).toThrow()
      expect(() => lispish.run(`(values null)`)).toThrow()
      expect(() => lispish.run(`(values undefined)`)).toThrow()
      expect(() => lispish.run(`(values [1])`)).toThrow()
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

  describe(`ohas`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(ohas (object) "x")`)).toBe(false)
      expect(lispish.run(`(ohas (object "x" 1) "x")`)).toBe(true)
      expect(lispish.run(`(ohas (object "x" 1) "")`)).toBe(false)
      expect(lispish.run(`(ohas (object "x" 1) "y")`)).toBe(false)
      expect(lispish.run(`(ohas (object "" 1) "")`)).toBe(true)
      expect(lispish.run(`(ohas (object "x" undefined "y" 2) "x")`)).toBe(true)
      expect(() => lispish.run(`(ohas (object "x" 1) 1)`)).toThrow()
      expect(() => lispish.run(`(ohas)`)).toThrow()
      expect(() => lispish.run(`(ohas (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(ohas 0 "x")`)).toThrow()
      expect(() => lispish.run(`(ohas true "x")`)).toThrow()
      expect(() => lispish.run(`(ohas false "x")`)).toThrow()
      expect(() => lispish.run(`(ohas null "x")`)).toThrow()
      expect(() => lispish.run(`(ohas undefined "x")`)).toThrow()
      expect(() => lispish.run(`(ohas [1] "x")`)).toThrow()
    })
  })

  describe(`oget`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(oget (object) "x")`)).toBeUndefined()
      expect(lispish.run(`(oget (object "x" 1) "x")`)).toBe(1)
      expect(lispish.run(`(oget (object "x" 1) "")`)).toBeUndefined()
      expect(lispish.run(`(oget (object "x" 1) "y")`)).toBeUndefined()
      expect(lispish.run(`(oget (object "" 1) "")`)).toBe(1)
      expect(lispish.run(`(oget (object "x" undefined "y" 2) "x")`)).toBeUndefined()
      expect(() => lispish.run(`(oget (object "x" 1) 1)`)).toThrow()
      expect(() => lispish.run(`(oget)`)).toThrow()
      expect(() => lispish.run(`(oget (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(oget 0 "x")`)).toThrow()
      expect(() => lispish.run(`(oget true "x")`)).toThrow()
      expect(() => lispish.run(`(oget false "x")`)).toThrow()
      expect(() => lispish.run(`(oget null "x")`)).toThrow()
      expect(() => lispish.run(`(oget undefined "x")`)).toThrow()
      expect(() => lispish.run(`(oget [1] "x")`)).toThrow()
    })
  })

  describe(`oset`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(oset (object) "x" 1)`)).toBe(1)
      expect(lispish.run(`(oset (object "x" 1) "x" 2)`)).toBe(2)
      expect(lispish.run(`(oset (object "x" 1) "" 3)`)).toBe(3)
      expect(lispish.run(`(oset (object "x" 1) "y" (object))`)).toEqual({})
      expect(lispish.run(`(oset O "x" 1) O`, { vars: { O: {} } })).toEqual({ x: 1 })
      expect(() => lispish.run(`(oset (object "x" 1) 1)`)).toThrow()
      expect(() => lispish.run(`(oset)`)).toThrow()
      expect(() => lispish.run(`(oset (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(oset 0 "x")`)).toThrow()
      expect(() => lispish.run(`(oset true "x")`)).toThrow()
      expect(() => lispish.run(`(oset false "x")`)).toThrow()
      expect(() => lispish.run(`(oset null "x")`)).toThrow()
      expect(() => lispish.run(`(oset undefined "x")`)).toThrow()
      expect(() => lispish.run(`(oset [1] "x")`)).toThrow()
    })
    test(`set new value`, () => {
      const program = `
        (def obj (object "x" 10))
        (oset obj "y" 20)
        obj
      `
      expect(lispish.run(program)).toEqual({ x: 10, y: 20 })
    })
    test(`update value`, () => {
      const program = `
        (def obj (object "x" 10))
        (oset obj "x" 20)
        obj
      `
      expect(lispish.run(program)).toEqual({ x: 20 })
    })
  })

  describe(`odel`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(odel (object) "x")`)).toBeUndefined()
      expect(lispish.run(`(odel (object "x" 1) "x")`)).toBe(1)
      expect(lispish.run(`(odel (object "x" 1) "")`)).toBeUndefined()
      expect(lispish.run(`(odel (object "x" (object)) "x")`)).toEqual({})
      expect(() => lispish.run(`(odel (object "x" 1) 1)`)).toThrow()
      expect(() => lispish.run(`(odel)`)).toThrow()
      expect(() => lispish.run(`(odel (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(odel 0 "x")`)).toThrow()
      expect(() => lispish.run(`(odel true "x")`)).toThrow()
      expect(() => lispish.run(`(odel false "x")`)).toThrow()
      expect(() => lispish.run(`(odel null "x")`)).toThrow()
      expect(() => lispish.run(`(odel undefined "x")`)).toThrow()
      expect(() => lispish.run(`(odel [1] "x")`)).toThrow()
    })
    test(`delete atribute`, () => {
      const program = `
        (def obj (object "x" 10))
        (odel obj "x")
        obj
      `
      expect(lispish.run(program)).toEqual({})
    })

    test(`delete unexisting attribute`, () => {
      const program = `
        (def obj (object "x" 10))
        (odel obj "y")
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
        (!= obj1 obj2)
      `
      expect(lispish.run(program)).toBe(true)
    })
  })
})
