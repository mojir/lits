/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { lispish } from '../../../src'

describe('object functions', () => {
  describe('object', () => {
    test('samples', () => {
      expect(lispish(`(object)`)).toEqual({})
      expect(lispish(`(object "x" 1)`)).toEqual({ x: 1 })
      expect(lispish(`(object "x" undefined)`)).toEqual({ x: undefined })
      expect(lispish(`(object "x" 1 "x" 2)`)).toEqual({ x: 2 })
      expect(lispish(`(object "a" null "b" true "c" false "d" undefined "e" (object "x" (list)))`)).toEqual({
        a: null,
        b: true,
        c: false,
        d: undefined,
        e: { x: [] },
      })
      expect(lispish(`(let ((a "a")) (object a 1))`)).toEqual({ a: 1 })
      expect(() => lispish(`(object "x")`)).toThrow()
      expect(() => lispish(`(object "x")`)).toThrow()
      expect(() => lispish(`(object "x" 1 "y")`)).toThrow()
      expect(() => lispish(`(object 0 1)`)).toThrow()
      expect(() => lispish(`(object true 1)`)).toThrow()
      expect(() => lispish(`(object false 1)`)).toThrow()
      expect(() => lispish(`(object null 1)`)).toThrow()
      expect(() => lispish(`(object undefined 1)`)).toThrow()
      expect(() => lispish(`(object (list) 1)`)).toThrow()
      expect(() => lispish(`(object (object) 1)`)).toThrow()
    })
  })

  describe('keys', () => {
    test('samples', () => {
      expect(lispish(`(keys (object))`)).toEqual([])
      expect(lispish(`(keys (object "x" 1))`)).toEqual(['x'])
      expect(lispish(`(keys (object "x" undefined "y" 2))`)).toEqual(['x', 'y'])
      expect(() => lispish(`(keys)`)).toThrow()
      expect(() => lispish(`(keys (object "x") (object "x"))`)).toThrow()
      expect(() => lispish(`(keys 0)`)).toThrow()
      expect(() => lispish(`(keys true)`)).toThrow()
      expect(() => lispish(`(keys false)`)).toThrow()
      expect(() => lispish(`(keys null)`)).toThrow()
      expect(() => lispish(`(keys undefined)`)).toThrow()
      expect(() => lispish(`(keys (list 1))`)).toThrow()
    })
  })

  describe('values', () => {
    test('samples', () => {
      expect(lispish(`(values (object))`)).toEqual([])
      expect(lispish(`(values (object "x" 1))`)).toEqual([1])
      expect(lispish(`(values (object "x" undefined "y" 2))`)).toEqual([undefined, 2])
      expect(() => lispish(`(values)`)).toThrow()
      expect(() => lispish(`(values (object "x") (object "x"))`)).toThrow()
      expect(() => lispish(`(values 0)`)).toThrow()
      expect(() => lispish(`(values true)`)).toThrow()
      expect(() => lispish(`(values false)`)).toThrow()
      expect(() => lispish(`(values null)`)).toThrow()
      expect(() => lispish(`(values undefined)`)).toThrow()
      expect(() => lispish(`(values (list 1))`)).toThrow()
    })
  })

  describe('entries', () => {
    test('samples', () => {
      expect(lispish(`(entries (object))`)).toEqual([])
      expect(lispish(`(entries (object "x" 1))`)).toEqual([['x', 1]])
      expect(lispish(`(entries (object "x" undefined "y" 2))`)).toEqual([
        ['x', undefined],
        ['y', 2],
      ])
      expect(() => lispish(`(entries)`)).toThrow()
      expect(() => lispish(`(entries (object "x") (object "x"))`)).toThrow()
      expect(() => lispish(`(entries 0)`)).toThrow()
      expect(() => lispish(`(entries true)`)).toThrow()
      expect(() => lispish(`(entries false)`)).toThrow()
      expect(() => lispish(`(entries null)`)).toThrow()
      expect(() => lispish(`(entries undefined)`)).toThrow()
      expect(() => lispish(`(entries (list 1))`)).toThrow()
    })
  })

  describe('has-attr', () => {
    test('samples', () => {
      expect(lispish(`(has-attr (object) "x")`)).toBe(false)
      expect(lispish(`(has-attr (object "x" 1) "x")`)).toBe(true)
      expect(lispish(`(has-attr (object "x" 1) "")`)).toBe(false)
      expect(lispish(`(has-attr (object "x" 1) "y")`)).toBe(false)
      expect(lispish(`(has-attr (object "" 1) "")`)).toBe(true)
      expect(lispish(`(has-attr (object "x" undefined "y" 2) "x")`)).toBe(true)
      expect(() => lispish(`(has-attr (object "x" 1) 1)`)).toThrow()
      expect(() => lispish(`(has-attr)`)).toThrow()
      expect(() => lispish(`(has-attr (object "x") (object "x"))`)).toThrow()
      expect(() => lispish(`(has-attr 0 "x")`)).toThrow()
      expect(() => lispish(`(has-attr true "x")`)).toThrow()
      expect(() => lispish(`(has-attr false "x")`)).toThrow()
      expect(() => lispish(`(has-attr null "x")`)).toThrow()
      expect(() => lispish(`(has-attr undefined "x")`)).toThrow()
      expect(() => lispish(`(has-attr (list 1) "x")`)).toThrow()
    })
  })

  describe('get-attr', () => {
    test('samples', () => {
      expect(lispish(`(get-attr (object) "x")`)).toBeUndefined()
      expect(lispish(`(get-attr (object "x" 1) "x")`)).toBe(1)
      expect(lispish(`(get-attr (object "x" 1) "")`)).toBeUndefined()
      expect(lispish(`(get-attr (object "x" 1) "y")`)).toBeUndefined()
      expect(lispish(`(get-attr (object "" 1) "")`)).toBe(1)
      expect(lispish(`(get-attr (object "x" undefined "y" 2) "x")`)).toBeUndefined()
      expect(() => lispish(`(get-attr (object "x" 1) 1)`)).toThrow()
      expect(() => lispish(`(get-attr)`)).toThrow()
      expect(() => lispish(`(get-attr (object "x") (object "x"))`)).toThrow()
      expect(() => lispish(`(get-attr 0 "x")`)).toThrow()
      expect(() => lispish(`(get-attr true "x")`)).toThrow()
      expect(() => lispish(`(get-attr false "x")`)).toThrow()
      expect(() => lispish(`(get-attr null "x")`)).toThrow()
      expect(() => lispish(`(get-attr undefined "x")`)).toThrow()
      expect(() => lispish(`(get-attr (list 1) "x")`)).toThrow()
    })
  })

  describe('set-attr', () => {
    test('samples', () => {
      expect(lispish(`(set-attr (object) "x" 1)`)).toBe(1)
      expect(lispish(`(set-attr (object "x" 1) "x" 2)`)).toBe(2)
      expect(lispish(`(set-attr (object "x" 1) "" 3)`)).toBe(3)
      expect(lispish(`(set-attr (object "x" 1) "y" (object))`)).toEqual({})
      expect(lispish(`(set-attr O "x" 1) O`, { O: {} })).toEqual({ x: 1 })
      expect(() => lispish(`(set-attr (object "x" 1) 1)`)).toThrow()
      expect(() => lispish(`(set-attr)`)).toThrow()
      expect(() => lispish(`(set-attr (object "x") (object "x"))`)).toThrow()
      expect(() => lispish(`(set-attr 0 "x")`)).toThrow()
      expect(() => lispish(`(set-attr true "x")`)).toThrow()
      expect(() => lispish(`(set-attr false "x")`)).toThrow()
      expect(() => lispish(`(set-attr null "x")`)).toThrow()
      expect(() => lispish(`(set-attr undefined "x")`)).toThrow()
      expect(() => lispish(`(set-attr (list 1) "x")`)).toThrow()
    })
    test('set new value', () => {
      const program = `
        (setq obj (object "x" 10))
        (set-attr obj "y" 20)
        obj
      `
      expect(lispish(program)).toEqual({ x: 10, y: 20 })
    })
    test('update value', () => {
      const program = `
        (setq obj (object "x" 10))
        (set-attr obj "x" 20)
        obj
      `
      expect(lispish(program)).toEqual({ x: 20 })
    })
  })

  describe('del-attr', () => {
    test('samples', () => {
      expect(lispish(`(del-attr (object) "x")`)).toBeUndefined()
      expect(lispish(`(del-attr (object "x" 1) "x")`)).toBe(1)
      expect(lispish(`(del-attr (object "x" 1) "")`)).toBeUndefined()
      expect(lispish(`(del-attr (object "x" (object)) "x")`)).toEqual({})
      expect(() => lispish(`(del-attr (object "x" 1) 1)`)).toThrow()
      expect(() => lispish(`(del-attr)`)).toThrow()
      expect(() => lispish(`(del-attr (object "x") (object "x"))`)).toThrow()
      expect(() => lispish(`(del-attr 0 "x")`)).toThrow()
      expect(() => lispish(`(del-attr true "x")`)).toThrow()
      expect(() => lispish(`(del-attr false "x")`)).toThrow()
      expect(() => lispish(`(del-attr null "x")`)).toThrow()
      expect(() => lispish(`(del-attr undefined "x")`)).toThrow()
      expect(() => lispish(`(del-attr (list 1) "x")`)).toThrow()
    })
    test('delete atribute', () => {
      const program = `
        (setq obj (object "x" 10))
        (del-attr obj "x")
        obj
      `
      expect(lispish(program)).toEqual({})
    })

    test('delete unexisting attribute', () => {
      const program = `
        (setq obj (object "x" 10))
        (del-attr obj "y")
        obj
      `
      expect(lispish(program)).toEqual({ x: 10 })
    })
  })

  describe('merge', () => {
    test('samples', () => {
      expect(lispish(`(merge (object "x" 10))`)).toEqual({ x: 10 })
      expect(lispish(`(merge (object "x" 10) (object "y" 20))`)).toEqual({ x: 10, y: 20 })
      expect(lispish(`(merge (object "x" 10) (object "x" 5))`)).toEqual({ x: 5 })
      expect(lispish(`(merge (object) (object "x" 10) (object "y" 10) (object "z" 10))`)).toEqual({
        x: 10,
        y: 10,
        z: 10,
      })
      expect(() => lispish(`(merge)`)).toThrow()
      expect(() => lispish(`(merge 1)`)).toThrow()
      expect(() => lispish(`(merge "1")`)).toThrow()
      expect(() => lispish(`(merge true)`)).toThrow()
      expect(() => lispish(`(merge false)`)).toThrow()
      expect(() => lispish(`(merge null)`)).toThrow()
      expect(() => lispish(`(merge undefined)`)).toThrow()
      expect(() => lispish(`(merge (array))`)).toThrow()
    })

    test('merge returns new object', () => {
      const program = `
        (setq obj1 (object "x" 10))
        (setq obj2 (merge obj1))
        (!= obj1 obj2)
      `
      expect(lispish(program)).toBe(true)
    })
  })
})
