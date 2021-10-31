/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lits } from '../src'

let lits: Lits

beforeEach(() => {
  lits = new Lits()
})

describe(`parseArguments`, () => {
  describe(`defn`, () => {
    test(`& params`, () => {
      expect(() => lits.run(`(defn foo [& a] a)`)).not.toThrow()
      expect(() => lits.run(`(defn foo [& 1] a)`)).toThrow()
      expect(() => lits.run(`(defn foo [x & a] a)`)).not.toThrow()
      expect(() => lits.run(`(defn foo [x &] a)`)).toThrow()
      expect(() => lits.run(`(defn foo [x & a b] a)`)).toThrow()
      expect(() => lits.run(`(defn foo [x & a] a) (foo 1)`)).not.toThrow()
      expect(() => lits.run(`(defn foo [x & a] a) (foo)`)).toThrow()
      expect(() => lits.run(`(defn foo [& a] a) (foo +)`)).not.toThrow()
      const program = `
        (defn foo [first & rest] rest)
        (foo 1 2 3)
      `
      expect(lits.run(program)).toEqual([2, 3])
    })
    test(`modifier combinations`, () => {
      expect(lits.run(`((fn [& b] [b]))`)).toEqual([[]])
      expect(lits.run(`((fn [& b] [b]) 1)`)).toEqual([[1]])
      expect(lits.run(`((fn [& b] [b]) 1 2)`)).toEqual([[1, 2]])
      expect(lits.run(`((fn [& b] [b]) 1 2 3)`)).toEqual([[1, 2, 3]])
      expect(lits.run(`((fn [a & d] [a d]) 1)`)).toEqual([1, []])
      expect(lits.run(`((fn [a & d] [a d]) 1 2)`)).toEqual([1, [2]])
      expect(lits.run(`((fn [a & d] [a d]) 1 2 3)`)).toEqual([1, [2, 3]])
      expect(lits.run(`((fn [a & d] [a d]) 1 2 3 4)`)).toEqual([1, [2, 3, 4]])
      expect(lits.run(`((fn [& b &let [x (+ 5 5)]] [b x]))`)).toEqual([[], 10])
      expect(lits.run(`((fn [& b &let [x (+ 5 5)]] [b x]) 1)`)).toEqual([[1], 10])
      expect(lits.run(`((fn [& b &let [x (+ 5 5)]] [b x]) 1 2)`)).toEqual([[1, 2], 10])
      expect(lits.run(`((fn [a & d &let [x (+ 5 5)]] [a d x]) 1)`)).toEqual([1, [], 10])
      expect(lits.run(`((fn [a & d &let [x (+ 5 5)]] [a d x]) 1 2)`)).toEqual([1, [2], 10])
      expect(lits.run(`((fn [a & d &let [x (+ 5 5)]] [a d x]) 1 2 3)`)).toEqual([1, [2, 3], 10])
      expect(() => lits.run(`(fn [& b &let []] a)`)).not.toThrow()
      expect(() => lits.run(`(fn [&let [a 1)] a)`)).toThrow()
      expect(() => lits.run(`(fn [& b &let [] &let []] a)`)).toThrow()
      expect(() => lits.run(`(fn [&let [] & b] a)`)).toThrow()
      expect(() => lits.run(`(fn [&let []] nil)`)).not.toThrow()
      expect(() => lits.run(`(fn [& &let []] nil)`)).toThrow()
      expect(() => lits.run(`(fn [&let [] & b] a)`)).toThrow()
      expect(() => lits.run(`(fn [& & a] a)`)).toThrow()
      expect(() => lits.run(`(fn [a a] a)`)).toThrow()
      expect(() => lits.run(`(fn [&rst a] a)`)).toThrow()
      expect(() => lits.run(`(fn [&when a] a)`)).toThrow()
      expect(() => lits.run(`(fn [&while a] a)`)).toThrow()
    })
  })
})
