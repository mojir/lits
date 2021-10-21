import { Lispish } from '../../../src'
import { Arr } from '../../../src/interface'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`array functions`, () => {
  describe(`array`, () => {
    test(`samples`, () => {
      expect(lispish.run(`[]`)).toEqual([])
      expect(lispish.run(`(array 1)`)).toEqual([1])
      expect(lispish.run(`(array 0 "1" null true false (array []) (object))`)).toEqual([
        0,
        `1`,
        null,
        true,
        false,
        [[]],
        {},
      ])
    })
    test(`shorthand samples`, () => {
      expect(lispish.run(`[]`)).toEqual([])
      expect(lispish.run(`[1]`)).toEqual([1])
      expect((lispish.run(`[null]`) as Arr)[0]).toEqual(null)
      expect(lispish.run(`[0 "1" null true false [[]] (object)]`)).toEqual([0, `1`, null, true, false, [[]], {}])
    })
  })

  describe(`range`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(range 0)`)).toEqual([])
      expect(lispish.run(`(range 5)`)).toEqual([0, 1, 2, 3, 4])
      expect(lispish.run(`(range -5)`)).toEqual([0, -1, -2, -3, -4])
      expect(lispish.run(`(range 5 1)`)).toEqual([5, 4, 3, 2])
      expect(lispish.run(`(range 1 5)`)).toEqual([1, 2, 3, 4])
      expect(lispish.run(`(range 5 1 -2)`)).toEqual([5, 3])
      expect(lispish.run(`(range 0 0.5 0.125)`)).toEqual([0, 0.125, 0.25, 0.375])
      expect(() => lispish.run(`(range)`)).toThrow()
      expect(() => lispish.run(`(range 0 2 1 1)`)).toThrow()
      expect(() => lispish.run(`(range 0 2 0)`)).toThrow()
      expect(() => lispish.run(`(range 0 0 0)`)).toThrow()
      expect(() => lispish.run(`(range 1 'x')`)).toThrow()
      expect(() => lispish.run(`(range false 1 2)`)).toThrow()
      expect(() => lispish.run(`(range 0 2 'y')`)).toThrow()
      expect(() => lispish.run(`(range (object) 'x' 'y')`)).toThrow()
    })
  })

  describe(`repeat`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(repeat 3 5)`)).toEqual([5, 5, 5])
      expect(lispish.run(`(repeat 3 "5")`)).toEqual([`5`, `5`, `5`])
      expect(lispish.run(`(repeat 1 "5")`)).toEqual([`5`])
      expect(lispish.run(`(repeat 0 "5")`)).toEqual([])
      expect(() => lispish.run(`(repeat 1.3 "5")`)).toThrow()
      expect(() => lispish.run(`(repeat -10 "5")`)).toThrow()
      expect(() => lispish.run(`(repeat 10)`)).toThrow()
      expect(() => lispish.run(`(repeat "5")`)).toThrow()
      expect(() => lispish.run(`(repeat)`)).toThrow()
    })
  })

  describe(`flatten`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(flatten [1 2 [3 4] 5])`)).toEqual([1, 2, 3, 4, 5])
      expect(lispish.run(`(flatten [1 2 [3 [4 [5]]] 6])`)).toEqual([1, 2, 3, 4, 5, 6])
      expect(lispish.run(`(flatten {})`)).toEqual([])
      expect(lispish.run(`(flatten 12)`)).toEqual([])
      expect(lispish.run(`(flatten true)`)).toEqual([])
      expect(lispish.run(`(flatten false)`)).toEqual([])
      expect(lispish.run(`(flatten null)`)).toEqual([])
      expect(lispish.run(`(flatten #"abc")`)).toEqual([])
      expect(() => lispish.run(`(flatten [] [])`)).toThrow()
      expect(() => lispish.run(`(flatten)`)).toThrow()
    })
  })

  describe(`mapcat`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])`)).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
      expect(lispish.run(`(mapcat reverse [[3 2 1 0] [6 [5] 4] [9 8 7]])`)).toEqual([0, 1, 2, 3, 4, [5], 6, 7, 8, 9])
      expect(lispish.run(`(defn foo [n] [(- n 1) n (+ n 1)]) (mapcat foo [1 2 3])`)).toEqual([
        0, 1, 2, 1, 2, 3, 2, 3, 4,
      ])
      expect(lispish.run(`(mapcat #(remove even? %1) [[1 2] [2 2] [2 3]])`)).toEqual([1, 3])
    })
  })
})
