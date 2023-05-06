/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lits } from '../../../src'

describe(`object functions`, () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    describe(`object`, () => {
      test(`samples`, () => {
        expect(lits.run(`(object)`)).toEqual({})
        expect(lits.run(`(object :x 1)`)).toEqual({ x: 1 })
        expect(lits.run(`(object :x nil)`)).toEqual({ x: null })
        expect(lits.run(`(object :x 1 :x 2)`)).toEqual({ x: 2 })
        expect(lits.run(`(object :a nil :b true :c false :d 0 :e (object :x []))`)).toEqual({
          a: null,
          b: true,
          c: false,
          d: 0,
          e: { x: [] },
        })
        expect(lits.run(`(let [a :a] (object a 1))`)).toEqual({ a: 1 })
        expect(() => lits.run(`(object :x)`)).toThrow()
        expect(() => lits.run(`(object :x)`)).toThrow()
        expect(() => lits.run(`(object :x 1 :y)`)).toThrow()
        expect(() => lits.run(`(object 0 1)`)).toThrow()
        expect(() => lits.run(`(object true 1)`)).toThrow()
        expect(() => lits.run(`(object false 1)`)).toThrow()
        expect(() => lits.run(`(object nil 1)`)).toThrow()
        expect(() => lits.run(`(object [] 1)`)).toThrow()
        expect(() => lits.run(`(object (object) 1)`)).toThrow()
      })
    })

    describe(`keys`, () => {
      test(`samples`, () => {
        expect(lits.run(`(keys (object))`)).toEqual([])
        expect(lits.run(`(keys (object :x 1))`)).toEqual([`x`])
        expect(lits.run(`(keys (object :x nil :y 2))`)).toEqual([`x`, `y`])
        expect(() => lits.run(`(keys)`)).toThrow()
        expect(() => lits.run(`(keys (object :x) (object :x))`)).toThrow()
        expect(() => lits.run(`(keys 0)`)).toThrow()
        expect(() => lits.run(`(keys true)`)).toThrow()
        expect(() => lits.run(`(keys false)`)).toThrow()
        expect(() => lits.run(`(keys nil)`)).toThrow()
        expect(() => lits.run(`(keys [1])`)).toThrow()
      })
    })

    describe(`vals`, () => {
      test(`samples`, () => {
        expect(lits.run(`(vals (object))`)).toEqual([])
        expect(lits.run(`(vals (object :x 1))`)).toEqual([1])
        expect(lits.run(`(vals (object :x nil :y 2))`)).toEqual([null, 2])
        expect(() => lits.run(`(vals)`)).toThrow()
        expect(() => lits.run(`(vals (object :x) (object :x))`)).toThrow()
        expect(() => lits.run(`(vals 0)`)).toThrow()
        expect(() => lits.run(`(vals true)`)).toThrow()
        expect(() => lits.run(`(vals false)`)).toThrow()
        expect(() => lits.run(`(vals nil)`)).toThrow()
        expect(() => lits.run(`(vals [1])`)).toThrow()
      })
    })

    describe(`entries`, () => {
      test(`samples`, () => {
        expect(lits.run(`(entries (object))`)).toEqual([])
        expect(lits.run(`(entries (object :x 1))`)).toEqual([[`x`, 1]])
        expect(lits.run(`(entries (object :x nil :y 2))`)).toEqual([
          [`x`, null],
          [`y`, 2],
        ])
        expect(() => lits.run(`(entries)`)).toThrow()
        expect(() => lits.run(`(entries (object :x) (object :x))`)).toThrow()
        expect(() => lits.run(`(entries 0)`)).toThrow()
        expect(() => lits.run(`(entries true)`)).toThrow()
        expect(() => lits.run(`(entries false)`)).toThrow()
        expect(() => lits.run(`(entries nil)`)).toThrow()
        expect(() => lits.run(`(entries [1])`)).toThrow()
      })
    })

    describe(`find`, () => {
      test(`samples`, () => {
        expect(lits.run(`(find (object :x 1) :a)`)).toBeNull()
        expect(lits.run(`(find (object :x 1) :x)`)).toEqual([`x`, 1])
        expect(lits.run(`(find (object :x 1 :y 2) :x)`)).toEqual([`x`, 1])
        expect(() => lits.run(`(find)`)).toThrow()
        expect(() => lits.run(`(find (object :x) (object :x))`)).toThrow()
        expect(() => lits.run(`(find (object :x) nil)`)).toThrow()
        expect(() => lits.run(`(find (object :x) true)`)).toThrow()
        expect(() => lits.run(`(find (object :x) false)`)).toThrow()
        expect(() => lits.run(`(find (object :x) :x :y)`)).toThrow()
        expect(() => lits.run(`(find (object :x))`)).toThrow()
        expect(() => lits.run(`(find [] :x)`)).toThrow()
        expect(() => lits.run(`(find nil :x)`)).toThrow()
        expect(() => lits.run(`(find false :x)`)).toThrow()
        expect(() => lits.run(`(find 4 :x)`)).toThrow()
      })
    })

    describe(`dissoc`, () => {
      test(`samples`, () => {
        expect(lits.run(`(dissoc (object) :x)`)).toEqual({})
        expect(lits.run(`(dissoc (object :x 1 :y 2) :x)`)).toEqual({ y: 2 })
        expect(lits.run(`(dissoc (object :x 1) "")`)).toEqual({ x: 1 })
        expect(lits.run(`(dissoc (object :x (object)) :x)`)).toEqual({})
        expect(() => lits.run(`(dissoc (object :x 1) 1)`)).toThrow()
        expect(() => lits.run(`(dissoc)`)).toThrow()
        expect(() => lits.run(`(dissoc (object :x) (object :x))`)).toThrow()
        expect(() => lits.run(`(dissoc 0 :x)`)).toThrow()
        expect(() => lits.run(`(dissoc true :x)`)).toThrow()
        expect(() => lits.run(`(dissoc false :x)`)).toThrow()
        expect(() => lits.run(`(dissoc nil :x)`)).toThrow()
        expect(() => lits.run(`(dissoc undefined :x)`)).toThrow()
        expect(() => lits.run(`(dissoc [1] :x)`)).toThrow()
      })
      test(`delete atribute`, () => {
        const program = `
        (def obj (object :x 10))
        (dissoc obj :x)
        obj
      `
        expect(lits.run(program)).toEqual({ x: 10 })
      })

      test(`delete unexisting attribute`, () => {
        const program = `
        (def obj (object :x 10))
        (dissoc obj :y)
        obj
      `
        expect(lits.run(program)).toEqual({ x: 10 })
      })
    })

    describe(`merge`, () => {
      test(`samples`, () => {
        expect(lits.run(`(merge (object :x 10))`)).toEqual({ x: 10 })
        expect(lits.run(`(merge (object :x 10) (object :y 20))`)).toEqual({ x: 10, y: 20 })
        expect(lits.run(`(merge (object :x 10) (object :x 5))`)).toEqual({ x: 5 })
        expect(lits.run(`(merge (object) (object :x 10) (object :y 10) (object :z 10))`)).toEqual({
          x: 10,
          y: 10,
          z: 10,
        })
        expect(lits.run(`(merge)`)).toBeNull()
        expect(() => lits.run(`(merge 1)`)).toThrow()
        expect(() => lits.run(`(merge :1)`)).toThrow()
        expect(() => lits.run(`(merge true)`)).toThrow()
        expect(() => lits.run(`(merge false)`)).toThrow()
        expect(() => lits.run(`(merge nil)`)).toThrow()
        expect(() => lits.run(`(merge undefined)`)).toThrow()
        expect(() => lits.run(`(merge (array))`)).toThrow()
      })

      describe(`merge-with`, () => {
        test(`samples`, () => {
          expect(lits.run(`(merge-with + (object :x 10) (object :y 20))`)).toEqual({
            x: 10,
            y: 20,
          })
          expect(lits.run(`(merge-with + (object :x 10) (object :x 15 :y 20))`)).toEqual({
            x: 25,
            y: 20,
          })
          expect(lits.run(`(merge-with - (object :x 10) (object :x 20) (object :x 30) (object :x 40))`)).toEqual({
            x: -80,
          })
          expect(lits.run(`(merge-with +)`)).toBeNull()
          expect(() => lits.run(`(merge-with)`)).toThrow()
          expect(() => lits.run(`(merge-with + "kjh")`)).toThrow()
          expect(() => lits.run(`(merge-with + [1 2 3])`)).toThrow()
        })
      })

      test(`merge returns new object`, () => {
        const program = `
        (def obj1 (object :x 10))
        (def obj2 (merge obj1))
        (not= obj1 obj2)
      `
        expect(lits.run(program)).toBe(true)
      })
    })

    describe(`zipmap`, () => {
      test(`samples`, () => {
        expect(lits.run(`(zipmap [:a :b :c] [10 nil [1 2 3]])`)).toEqual({ a: 10, b: null, c: [1, 2, 3] })
        expect(lits.run(`(zipmap [:a :b] [10 nil [1 2 3]])`)).toEqual({ a: 10, b: null })
        expect(lits.run(`(zipmap [:a :b :c] [10 nil])`)).toEqual({ a: 10, b: null })
        expect(lits.run(`(zipmap [:a :b :c] [])`)).toEqual({})
        expect(lits.run(`(zipmap [] [10 nil [1 2 3]])`)).toEqual({})
        expect(lits.run(`(zipmap [] [])`)).toEqual({})
        expect(() => lits.run(`(zipmap [])`)).toThrow()
        expect(() => lits.run(`(zipmap "abc" [])`)).toThrow()
        expect(() => lits.run(`(zipmap [] "abc)`)).toThrow()
        expect(() => lits.run(`(zipmap [] [] [])`)).toThrow()
      })
    })

    describe(`select-keys`, () => {
      test(`samples`, () => {
        expect(lits.run(`(select-keys {:a 1 :b 2 :c 3} [:a :b])`)).toEqual({ a: 1, b: 2 })
        expect(lits.run(`(select-keys {:a 1} [:a :b])`)).toEqual({ a: 1 })
        expect(() => lits.run(`(select-keys {:a 1})`)).toThrow()
        expect(() => lits.run(`(select-keys {:a 1} :a)`)).toThrow()
        expect(() => lits.run(`(select-keys {:a 1} [:a] [:a])`)).toThrow()
      })
    })
  }
})
