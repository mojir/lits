/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lispish } from '../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`parseArguments`, () => {
  describe(`defn`, () => {
    test(`&rest params`, () => {
      expect(() => lispish.run(`(defn foo [&rest a] a)`)).not.toThrow()
      expect(() => lispish.run(`(defn foo [&rest 1] a)`)).toThrow()
      expect(() => lispish.run(`(defn foo [x &rest a] a)`)).not.toThrow()
      expect(() => lispish.run(`(defn foo [x &rest] a)`)).toThrow()
      expect(() => lispish.run(`(defn foo [x &rest a b] a)`)).toThrow()
      expect(() => lispish.run(`(defn foo [x &rest a] a) (foo 1)`)).not.toThrow()
      expect(() => lispish.run(`(defn foo [x &rest a] a) (foo)`)).toThrow()
      expect(() => lispish.run(`(defn foo [&rest a] a) (foo +)`)).toThrow()
      const program = `
        (defn foo [first &rest rest] rest)
        (foo 1 2 3)
      `
      expect(lispish.run(program)).toEqual([2, 3])
    })
    test(`&opt params`, () => {
      expect(() => lispish.run(`(defn foo [&opt a] a)`)).not.toThrow()
      expect(lispish.run(`(defn foo [&opt (a 10)] a) (foo)`)).toBe(10)
      expect(lispish.run(`(defn foo [&opt (a 10)] a) (foo 5)`)).toBe(5)
      expect(() => lispish.run(`(defn foo [&opt (a 10 20)] a)`)).toThrow()
      expect(() => lispish.run(`(defn foo [&opt ("a" 10)] a)`)).toThrow()
      expect(() => lispish.run(`(defn foo [x &opt a] a)`)).not.toThrow()
      expect(() => lispish.run(`(defn foo [x &opt] a)`)).toThrow()
      expect(() => lispish.run(`(defn foo [x &opt a b] a)`)).not.toThrow()
      expect(() => lispish.run(`(defn foo [x &opt a] a) (foo 1)`)).not.toThrow()
      expect(() => lispish.run(`(defn foo [x &opt a] a) (foo 1 2)`)).not.toThrow()
      expect(() => lispish.run(`(defn foo [x &opt a] a) (foo 1 2 3)`)).toThrow()
      expect(() => lispish.run(`(defn foo [x &opt a] a) (foo)`)).toThrow()
      expect(() => lispish.run(`(defn foo [&opt a] a) (foo +)`)).not.toThrow()
      expect(lispish.run(`(defn foo [first &opt o] o) (foo 1 2)`)).toEqual(2)
      expect(lispish.run(`(defn foo [first &opt o] o) (foo 1)`)).toEqual(undefined)
      expect(lispish.run(`(defn foo [first &opt (o 10)] o) (foo 1)`)).toEqual(10)
    })

    test(`modifier combinations`, () => {
      expect(lispish.run(`((fn [&opt a &rest b] [a b]))`)).toEqual([undefined, []])
      expect(lispish.run(`((fn [&opt a &rest b] [a b]) 1)`)).toEqual([1, []])
      expect(lispish.run(`((fn [&opt a &rest b] [a b]) 1 2)`)).toEqual([1, [2]])
      expect(lispish.run(`((fn [&opt a &rest b] [a b]) 1 2 3)`)).toEqual([1, [2, 3]])
      expect(lispish.run(`((fn [a &opt b c &rest d] [a b c d]) 1)`)).toEqual([1, undefined, undefined, []])
      expect(lispish.run(`((fn [a &opt b c &rest d] [a b c d]) 1 2)`)).toEqual([1, 2, undefined, []])
      expect(lispish.run(`((fn [a &opt b c &rest d] [a b c d]) 1 2 3)`)).toEqual([1, 2, 3, []])
      expect(lispish.run(`((fn [a &opt b c &rest d] [a b c d]) 1 2 3 4)`)).toEqual([1, 2, 3, [4]])
      expect(lispish.run(`((fn [a &opt b c &rest d] [a b c d]) 1 2 3 4 5)`)).toEqual([1, 2, 3, [4, 5]])
      expect(lispish.run(`((fn [&opt a &rest b &bind [x (+ 5 5)]] [a b x]))`)).toEqual([undefined, [], 10])
      expect(lispish.run(`((fn [&opt a &rest b &bind [x (+ 5 5)]] [a b x]) 1)`)).toEqual([1, [], 10])
      expect(lispish.run(`((fn [&opt a &rest b &bind [x (+ 5 5)]] [a b x]) 1 2)`)).toEqual([1, [2], 10])
      expect(lispish.run(`((fn [&opt a &rest b &bind [x (+ 5 5)]] [a b x]) 1 2 3)`)).toEqual([1, [2, 3], 10])
      expect(lispish.run(`((fn [a &opt b c &rest d &bind [x (+ 5 5)]] [a b c d x]) 1)`)).toEqual([
        1,
        undefined,
        undefined,
        [],
        10,
      ])
      expect(lispish.run(`((fn [a &opt b c &rest d &bind [x (+ 5 5)]] [a b c d x]) 1 2)`)).toEqual([
        1,
        2,
        undefined,
        [],
        10,
      ])
      expect(lispish.run(`((fn [a &opt b c &rest d &bind [x (+ 5 5)]] [a b c d x]) 1 2 3)`)).toEqual([1, 2, 3, [], 10])
      expect(lispish.run(`((fn [a &opt b c &rest d &bind [x (+ 5 5)]] [a b c d x]) 1 2 3 4)`)).toEqual([
        1,
        2,
        3,
        [4],
        10,
      ])
      expect(lispish.run(`((fn [a &opt b c &rest d &bind [x (+ 5 5)]] [a b c d x]) 1 2 3 4 5)`)).toEqual([
        1,
        2,
        3,
        [4, 5],
        10,
      ])
      expect(() => lispish.run(`(fn [&opt a &rest b &bind []] a)`)).not.toThrow()
      expect(() => lispish.run(`(fn [&bind [a 1)] a)`)).toThrow()
      expect(() => lispish.run(`(fn [&opt a &rest b &bind [] &bind []] a)`)).toThrow()
      expect(() => lispish.run(`(fn [&opt a &bind [] &rest b] a)`)).toThrow()
      expect(() => lispish.run(`(fn [&opt &bind []] null)`)).toThrow()
      expect(() => lispish.run(`(fn [&rest &bind []] null)`)).toThrow()
      expect(() => lispish.run(`(fn [&bind [] &opt a &rest b] a)`)).toThrow()
      expect(() => lispish.run(`(fn [&opt &rest a] a)`)).toThrow()
      expect(() => lispish.run(`(fn [&opt &opt a] a)`)).toThrow()
      expect(() => lispish.run(`(fn [&rest &rest a] a)`)).toThrow()
      expect(() => lispish.run(`(fn [(a 10) &opt b] a)`)).toThrow()
      expect(() => lispish.run(`(fn [a &opt a] a)`)).toThrow()
      expect(() => lispish.run(`(fn [&rest &opt a] a)`)).toThrow()
      expect(() => lispish.run(`(fn [&rest a &opt b] a)`)).toThrow()
      expect(() => lispish.run(`(fn [&rst a] a)`)).toThrow()
    })
  })
})
