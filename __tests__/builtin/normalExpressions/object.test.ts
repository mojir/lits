/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lispish } from '../../../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe('object functions', () => {
  describe('object', () => {
    test('samples', () => {
      expect(lispish.run(`(object)`)).toEqual({})
      expect(lispish.run(`(object "x" 1)`)).toEqual({ x: 1 })
      expect(lispish.run(`(object "x" undefined)`)).toEqual({ x: undefined })
      expect(lispish.run(`(object "x" 1 "x" 2)`)).toEqual({ x: 2 })
      expect(lispish.run(`(object "a" null "b" true "c" false "d" undefined "e" (object "x" '()))`)).toEqual({
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
      expect(() => lispish.run(`(object '() 1)`)).toThrow()
      expect(() => lispish.run(`(object (object) 1)`)).toThrow()
    })
  })

  describe('keys', () => {
    test('samples', () => {
      expect(lispish.run(`(keys (object))`)).toEqual([])
      expect(lispish.run(`(keys (object "x" 1))`)).toEqual(['x'])
      expect(lispish.run(`(keys (object "x" undefined "y" 2))`)).toEqual(['x', 'y'])
      expect(() => lispish.run(`(keys)`)).toThrow()
      expect(() => lispish.run(`(keys (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(keys 0)`)).toThrow()
      expect(() => lispish.run(`(keys true)`)).toThrow()
      expect(() => lispish.run(`(keys false)`)).toThrow()
      expect(() => lispish.run(`(keys null)`)).toThrow()
      expect(() => lispish.run(`(keys undefined)`)).toThrow()
      expect(() => lispish.run(`(keys '(1))`)).toThrow()
    })
  })

  describe('values', () => {
    test('samples', () => {
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
      expect(() => lispish.run(`(values '(1))`)).toThrow()
    })
  })

  describe('entries', () => {
    test('samples', () => {
      expect(lispish.run(`(entries (object))`)).toEqual([])
      expect(lispish.run(`(entries (object "x" 1))`)).toEqual([['x', 1]])
      expect(lispish.run(`(entries (object "x" undefined "y" 2))`)).toEqual([
        ['x', undefined],
        ['y', 2],
      ])
      expect(() => lispish.run(`(entries)`)).toThrow()
      expect(() => lispish.run(`(entries (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(entries 0)`)).toThrow()
      expect(() => lispish.run(`(entries true)`)).toThrow()
      expect(() => lispish.run(`(entries false)`)).toThrow()
      expect(() => lispish.run(`(entries null)`)).toThrow()
      expect(() => lispish.run(`(entries undefined)`)).toThrow()
      expect(() => lispish.run(`(entries '(1))`)).toThrow()
    })
  })

  describe('has-attr', () => {
    test('samples', () => {
      expect(lispish.run(`(has-attr (object) "x")`)).toBe(false)
      expect(lispish.run(`(has-attr (object "x" 1) "x")`)).toBe(true)
      expect(lispish.run(`(has-attr (object "x" 1) "")`)).toBe(false)
      expect(lispish.run(`(has-attr (object "x" 1) "y")`)).toBe(false)
      expect(lispish.run(`(has-attr (object "" 1) "")`)).toBe(true)
      expect(lispish.run(`(has-attr (object "x" undefined "y" 2) "x")`)).toBe(true)
      expect(() => lispish.run(`(has-attr (object "x" 1) 1)`)).toThrow()
      expect(() => lispish.run(`(has-attr)`)).toThrow()
      expect(() => lispish.run(`(has-attr (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(has-attr 0 "x")`)).toThrow()
      expect(() => lispish.run(`(has-attr true "x")`)).toThrow()
      expect(() => lispish.run(`(has-attr false "x")`)).toThrow()
      expect(() => lispish.run(`(has-attr null "x")`)).toThrow()
      expect(() => lispish.run(`(has-attr undefined "x")`)).toThrow()
      expect(() => lispish.run(`(has-attr '(1) "x")`)).toThrow()
    })
  })

  describe('get-attr', () => {
    test('samples', () => {
      expect(lispish.run(`(get-attr (object) "x")`)).toBeUndefined()
      expect(lispish.run(`(get-attr (object "x" 1) "x")`)).toBe(1)
      expect(lispish.run(`(get-attr (object "x" 1) "")`)).toBeUndefined()
      expect(lispish.run(`(get-attr (object "x" 1) "y")`)).toBeUndefined()
      expect(lispish.run(`(get-attr (object "" 1) "")`)).toBe(1)
      expect(lispish.run(`(get-attr (object "x" undefined "y" 2) "x")`)).toBeUndefined()
      expect(() => lispish.run(`(get-attr (object "x" 1) 1)`)).toThrow()
      expect(() => lispish.run(`(get-attr)`)).toThrow()
      expect(() => lispish.run(`(get-attr (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(get-attr 0 "x")`)).toThrow()
      expect(() => lispish.run(`(get-attr true "x")`)).toThrow()
      expect(() => lispish.run(`(get-attr false "x")`)).toThrow()
      expect(() => lispish.run(`(get-attr null "x")`)).toThrow()
      expect(() => lispish.run(`(get-attr undefined "x")`)).toThrow()
      expect(() => lispish.run(`(get-attr '(1) "x")`)).toThrow()
    })
  })

  describe('set-attr', () => {
    test('samples', () => {
      expect(lispish.run(`(set-attr (object) "x" 1)`)).toBe(1)
      expect(lispish.run(`(set-attr (object "x" 1) "x" 2)`)).toBe(2)
      expect(lispish.run(`(set-attr (object "x" 1) "" 3)`)).toBe(3)
      expect(lispish.run(`(set-attr (object "x" 1) "y" (object))`)).toEqual({})
      expect(lispish.run(`(set-attr O "x" 1) O`, { O: {} })).toEqual({ x: 1 })
      expect(() => lispish.run(`(set-attr (object "x" 1) 1)`)).toThrow()
      expect(() => lispish.run(`(set-attr)`)).toThrow()
      expect(() => lispish.run(`(set-attr (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(set-attr 0 "x")`)).toThrow()
      expect(() => lispish.run(`(set-attr true "x")`)).toThrow()
      expect(() => lispish.run(`(set-attr false "x")`)).toThrow()
      expect(() => lispish.run(`(set-attr null "x")`)).toThrow()
      expect(() => lispish.run(`(set-attr undefined "x")`)).toThrow()
      expect(() => lispish.run(`(set-attr '(1) "x")`)).toThrow()
    })
    test('set new value', () => {
      const program = `
        (setq obj (object "x" 10))
        (set-attr obj "y" 20)
        obj
      `
      expect(lispish.run(program)).toEqual({ x: 10, y: 20 })
    })
    test('update value', () => {
      const program = `
        (setq obj (object "x" 10))
        (set-attr obj "x" 20)
        obj
      `
      expect(lispish.run(program)).toEqual({ x: 20 })
    })
  })

  describe('del-attr', () => {
    test('samples', () => {
      expect(lispish.run(`(del-attr (object) "x")`)).toBeUndefined()
      expect(lispish.run(`(del-attr (object "x" 1) "x")`)).toBe(1)
      expect(lispish.run(`(del-attr (object "x" 1) "")`)).toBeUndefined()
      expect(lispish.run(`(del-attr (object "x" (object)) "x")`)).toEqual({})
      expect(() => lispish.run(`(del-attr (object "x" 1) 1)`)).toThrow()
      expect(() => lispish.run(`(del-attr)`)).toThrow()
      expect(() => lispish.run(`(del-attr (object "x") (object "x"))`)).toThrow()
      expect(() => lispish.run(`(del-attr 0 "x")`)).toThrow()
      expect(() => lispish.run(`(del-attr true "x")`)).toThrow()
      expect(() => lispish.run(`(del-attr false "x")`)).toThrow()
      expect(() => lispish.run(`(del-attr null "x")`)).toThrow()
      expect(() => lispish.run(`(del-attr undefined "x")`)).toThrow()
      expect(() => lispish.run(`(del-attr '(1) "x")`)).toThrow()
    })
    test('delete atribute', () => {
      const program = `
        (setq obj (object "x" 10))
        (del-attr obj "x")
        obj
      `
      expect(lispish.run(program)).toEqual({})
    })

    test('delete unexisting attribute', () => {
      const program = `
        (setq obj (object "x" 10))
        (del-attr obj "y")
        obj
      `
      expect(lispish.run(program)).toEqual({ x: 10 })
    })
  })

  describe('merge', () => {
    test('samples', () => {
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

    test('merge returns new object', () => {
      const program = `
        (setq obj1 (object "x" 10))
        (setq obj2 (merge obj1))
        (!= obj1 obj2)
      `
      expect(lispish.run(program)).toBe(true)
    })
  })
})
