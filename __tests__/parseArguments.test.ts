/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lits } from '../src'

let lits: Lits

beforeEach(() => {
  lits = new Lits()
})

describe(`parseArguments`, () => {
  describe(`defn`, () => {
    test(`&rest params`, () => {
      expect(() => lits.run(`(defn foo [&rest a] a)`)).not.toThrow()
      expect(() => lits.run(`(defn foo [&rest 1] a)`)).toThrow()
      expect(() => lits.run(`(defn foo [x &rest a] a)`)).not.toThrow()
      expect(() => lits.run(`(defn foo [x &rest] a)`)).toThrow()
      expect(() => lits.run(`(defn foo [x &rest a b] a)`)).toThrow()
      expect(() => lits.run(`(defn foo [x &rest a] a) (foo 1)`)).not.toThrow()
      expect(() => lits.run(`(defn foo [x &rest a] a) (foo)`)).toThrow()
      expect(() => lits.run(`(defn foo [&rest a] a) (foo +)`)).not.toThrow()
      const program = `
        (defn foo [first &rest rest] rest)
        (foo 1 2 3)
      `
      expect(lits.run(program)).toEqual([2, 3])
    })
    test(`&opt params`, () => {
      expect(() => lits.run(`(defn foo [&opt a] a)`)).not.toThrow()
      expect(lits.run(`(defn foo [&opt (a 10)] a) (foo)`)).toBe(10)
      expect(lits.run(`(defn foo [&opt (a 10)] a) (foo 5)`)).toBe(5)
      expect(() => lits.run(`(defn foo [&opt (a 10 20)] a)`)).toThrow()
      expect(() => lits.run(`(defn foo [&opt (:a 10)] a)`)).toThrow()
      expect(() => lits.run(`(defn foo [x &opt a] a)`)).not.toThrow()
      expect(() => lits.run(`(defn foo [x &opt] a)`)).toThrow()
      expect(() => lits.run(`(defn foo [x &opt a b] a)`)).not.toThrow()
      expect(() => lits.run(`(defn foo [x &opt a] a) (foo 1)`)).not.toThrow()
      expect(() => lits.run(`(defn foo [x &opt a] a) (foo 1 2)`)).not.toThrow()
      expect(() => lits.run(`(defn foo [x &opt a] a) (foo 1 2 3)`)).toThrow()
      expect(() => lits.run(`(defn foo [x &opt a] a) (foo)`)).toThrow()
      expect(() => lits.run(`(defn foo [&opt a] a) (foo +)`)).not.toThrow()
      expect(lits.run(`(defn foo [first &opt o] o) (foo 1 2)`)).toEqual(2)
      expect(lits.run(`(defn foo [first &opt o] o) (foo 1)`)).toBeNull()
      expect(lits.run(`(defn foo [first &opt (o 10)] o) (foo 1)`)).toEqual(10)
    })

    test(`modifier combinations`, () => {
      expect(lits.run(`((fn [&opt a &rest b] [a b]))`)).toEqual([null, []])
      expect(lits.run(`((fn [&opt a &rest b] [a b]) 1)`)).toEqual([1, []])
      expect(lits.run(`((fn [&opt a &rest b] [a b]) 1 2)`)).toEqual([1, [2]])
      expect(lits.run(`((fn [&opt a &rest b] [a b]) 1 2 3)`)).toEqual([1, [2, 3]])
      expect(lits.run(`((fn [a &opt b c &rest d] [a b c d]) 1)`)).toEqual([1, null, null, []])
      expect(lits.run(`((fn [a &opt b c &rest d] [a b c d]) 1 2)`)).toEqual([1, 2, null, []])
      expect(lits.run(`((fn [a &opt b c &rest d] [a b c d]) 1 2 3)`)).toEqual([1, 2, 3, []])
      expect(lits.run(`((fn [a &opt b c &rest d] [a b c d]) 1 2 3 4)`)).toEqual([1, 2, 3, [4]])
      expect(lits.run(`((fn [a &opt b c &rest d] [a b c d]) 1 2 3 4 5)`)).toEqual([1, 2, 3, [4, 5]])
      expect(lits.run(`((fn [&opt a &rest b &let [x (+ 5 5)]] [a b x]))`)).toEqual([null, [], 10])
      expect(lits.run(`((fn [&opt a &rest b &let [x (+ 5 5)]] [a b x]) 1)`)).toEqual([1, [], 10])
      expect(lits.run(`((fn [&opt a &rest b &let [x (+ 5 5)]] [a b x]) 1 2)`)).toEqual([1, [2], 10])
      expect(lits.run(`((fn [&opt a &rest b &let [x (+ 5 5)]] [a b x]) 1 2 3)`)).toEqual([1, [2, 3], 10])
      expect(lits.run(`((fn [a &opt b c &rest d &let [x (+ 5 5)]] [a b c d x]) 1)`)).toEqual([1, null, null, [], 10])
      expect(lits.run(`((fn [a &opt b c &rest d &let [x (+ 5 5)]] [a b c d x]) 1 2)`)).toEqual([1, 2, null, [], 10])
      expect(lits.run(`((fn [a &opt b c &rest d &let [x (+ 5 5)]] [a b c d x]) 1 2 3)`)).toEqual([1, 2, 3, [], 10])
      expect(lits.run(`((fn [a &opt b c &rest d &let [x (+ 5 5)]] [a b c d x]) 1 2 3 4)`)).toEqual([1, 2, 3, [4], 10])
      expect(lits.run(`((fn [a &opt b c &rest d &let [x (+ 5 5)]] [a b c d x]) 1 2 3 4 5)`)).toEqual([
        1,
        2,
        3,
        [4, 5],
        10,
      ])
      expect(() => lits.run(`(fn [&opt a &rest b &let []] a)`)).not.toThrow()
      expect(() => lits.run(`(fn [&let [a 1)] a)`)).toThrow()
      expect(() => lits.run(`(fn [&opt a &rest b &let [] &let []] a)`)).toThrow()
      expect(() => lits.run(`(fn [&opt a &let [] &rest b] a)`)).toThrow()
      expect(() => lits.run(`(fn [&opt &let []] nil)`)).toThrow()
      expect(() => lits.run(`(fn [&rest &let []] nil)`)).toThrow()
      expect(() => lits.run(`(fn [&let [] &opt a &rest b] a)`)).toThrow()
      expect(() => lits.run(`(fn [&opt &rest a] a)`)).toThrow()
      expect(() => lits.run(`(fn [&opt &opt a] a)`)).toThrow()
      expect(() => lits.run(`(fn [&rest &rest a] a)`)).toThrow()
      expect(() => lits.run(`(fn [(a 10) &opt b] a)`)).toThrow()
      expect(() => lits.run(`(fn [a &opt a] a)`)).toThrow()
      expect(() => lits.run(`(fn [&rest &opt a] a)`)).toThrow()
      expect(() => lits.run(`(fn [&rest a &opt b] a)`)).toThrow()
      expect(() => lits.run(`(fn [&rst a] a)`)).toThrow()
      expect(() => lits.run(`(fn [&when a] a)`)).toThrow()
      expect(() => lits.run(`(fn [&while a] a)`)).toThrow()
    })
  })
})
