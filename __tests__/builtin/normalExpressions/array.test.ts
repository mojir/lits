import { Lits } from '../../../src'
import { Arr } from '../../../src/interface'

let lits: Lits

beforeEach(() => {
  lits = new Lits({ debug: true })
})

describe(`array functions`, () => {
  describe(`array`, () => {
    test(`samples`, () => {
      expect(lits.run(`[]`)).toEqual([])
      expect(lits.run(`(array 1)`)).toEqual([1])
      expect(lits.run(`(array 0 :1 nil true false (array []) (object))`)).toEqual([0, `1`, null, true, false, [[]], {}])
    })
    test(`shorthand samples`, () => {
      expect(lits.run(`[]`)).toEqual([])
      expect(lits.run(`[1]`)).toEqual([1])
      expect((lits.run(`[nil]`) as Arr)[0]).toEqual(null)
      expect(lits.run(`[0 :1 nil true false [[]] (object)]`)).toEqual([0, `1`, null, true, false, [[]], {}])
    })
  })

  describe(`range`, () => {
    test(`samples`, () => {
      // expect(lits.run(`(range 0)`)).toEqual([])
      // expect(lits.run(`(range 5)`)).toEqual([0, 1, 2, 3, 4])
      // expect(lits.run(`(range -5)`)).toEqual([0, -1, -2, -3, -4])
      // expect(lits.run(`(range 5 1)`)).toEqual([5, 4, 3, 2])
      // expect(lits.run(`(range 1 5)`)).toEqual([1, 2, 3, 4])
      // expect(lits.run(`(range 5 1 -2)`)).toEqual([5, 3])
      // expect(lits.run(`(range 0 0.5 0.125)`)).toEqual([0, 0.125, 0.25, 0.375])
      // expect(() => lits.run(`(range)`)).toThrow()
      // expect(() => lits.run(`(range 0 2 1 1)`)).toThrow()
      // expect(() => lits.run(`(range 0 2 0)`)).toThrow()
      expect(() => lits.run(`(range 0 0 0)`)).toThrow()
      expect(() => lits.run(`(range 1 'x')`)).toThrow()
      expect(() => lits.run(`(range false 1 2)`)).toThrow()
      expect(() => lits.run(`(range 0 2 'y')`)).toThrow()
      expect(() => lits.run(`(range (object) 'x' 'y')`)).toThrow()
    })
  })

  describe(`repeat`, () => {
    test(`samples`, () => {
      expect(lits.run(`(repeat 3 5)`)).toEqual([5, 5, 5])
      expect(lits.run(`(repeat 3 :5)`)).toEqual([`5`, `5`, `5`])
      expect(lits.run(`(repeat 1 :5)`)).toEqual([`5`])
      expect(lits.run(`(repeat 0 :5)`)).toEqual([])
      expect(() => lits.run(`(repeat 1.3 :5)`)).toThrow()
      expect(() => lits.run(`(repeat -10 :5)`)).toThrow()
      expect(() => lits.run(`(repeat 10)`)).toThrow()
      expect(() => lits.run(`(repeat :5)`)).toThrow()
      expect(() => lits.run(`(repeat)`)).toThrow()
    })
  })

  describe(`flatten`, () => {
    test(`samples`, () => {
      expect(lits.run(`(flatten [1 2 [3 4] 5])`)).toEqual([1, 2, 3, 4, 5])
      expect(lits.run(`(flatten [1 2 [3 [4 [5]]] 6])`)).toEqual([1, 2, 3, 4, 5, 6])
      expect(lits.run(`(flatten {})`)).toEqual([])
      expect(lits.run(`(flatten 12)`)).toEqual([])
      expect(lits.run(`(flatten true)`)).toEqual([])
      expect(lits.run(`(flatten false)`)).toEqual([])
      expect(lits.run(`(flatten nil)`)).toEqual([])
      expect(lits.run(`(flatten #'abc')`)).toEqual([])
      expect(() => lits.run(`(flatten [] [])`)).toThrow()
      expect(() => lits.run(`(flatten)`)).toThrow()
    })
  })

  describe(`mapcat`, () => {
    test(`samples`, () => {
      expect(lits.run(`(mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])`)).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
      expect(lits.run(`(mapcat reverse [[3 2 1 0] [6 [5] 4] [9 8 7]])`)).toEqual([0, 1, 2, 3, 4, [5], 6, 7, 8, 9])
      expect(lits.run(`(defn foo [n] [(- n 1) n (+ n 1)]) (mapcat foo [1 2 3])`)).toEqual([0, 1, 2, 1, 2, 3, 2, 3, 4])
      expect(lits.run(`(mapcat #(remove even? %1) [[1 2] [2 2] [2 3]])`)).toEqual([1, 3])
    })
  })
})
