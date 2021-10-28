/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lits } from '../../src'
import { UserDefinedError } from '../../src/errors'

let lits: Lits

beforeEach(() => {
  lits = new Lits()
})

describe(`specialExpressions`, () => {
  let oldLog: () => void
  let logSpy: jest.Mock<any, any>
  let lastLog: unknown
  beforeEach(() => {
    oldLog = console.log
    logSpy = jest.fn()
    console.log = (...args: unknown[]) => {
      logSpy(...args)
      lastLog = args[0]
    }
  })
  afterEach(() => {
    console.log = oldLog
  })

  describe(`defs`, () => {
    test(`samples`, () => {
      expect(lits.run(`(defs :a 10) a`)).toBe(10)
      expect(lits.run(`(defs :a :b) (defs a :c) b`)).toBe(`c`)
      expect(lits.run(`(defs (str :a :1) 20) a1`)).toBe(20)
      expect(() => lits.run(`(defs true false)`)).toThrow()
      expect(() => lits.run(`(defs a)`)).toThrow()
      expect(() => lits.run(`(defs a 10 10)`)).toThrow()
      expect(() => lits.run(`(defs 1 10)`)).toThrow()
      expect(() => lits.run(`(defs nil 10)`)).toThrow()
      expect(() => lits.run(`(defs false 10)`)).toThrow()
      expect(() => lits.run(`(defs true 10)`)).toThrow()
      expect(() => lits.run(`(defs [] 10)`)).toThrow()
      expect(() => lits.run(`(defs (object) 10)`)).toThrow()
      expect(() => lits.run(`(defs a 10)`)).toThrow()
    })
  })

  describe(`def`, () => {
    test(`samples`, () => {
      expect(lits.run(`(def a 10) a`)).toBe(10)
      expect(lits.run(`(do (def a 10)) a`)).toBe(10)
      expect(() => lits.run(`(def a 10) (def a 20) a`)).toThrow()
      expect(() => lits.run(`(def true false)`)).toThrow()
      expect(() => lits.run(`(def a)`)).toThrow()
      expect(() => lits.run(`(def a 10 10)`)).toThrow()
      expect(() => lits.run(`(def 1 10)`)).toThrow()
      expect(() => lits.run(`(def nil 10)`)).toThrow()
      expect(() => lits.run(`(def false 10)`)).toThrow()
      expect(() => lits.run(`(def true 10)`)).toThrow()
      expect(() => lits.run(`(def [] 10)`)).toThrow()
      expect(() => lits.run(`(def (object) 10)`)).toThrow()
      expect(() => lits.run(`(def :a 10)`)).toThrow()
    })

    test(`local variable`, () => {
      const program = `
        (def x :A)     ;Global variable x
        (write! x)       ;:A
        (let [x :B]    ;Local variable x
          (write! x)     ;:B
        )
        (write! x)       ;:A - global variable x
      `
      lits.run(program)
      expect(logSpy).toHaveBeenNthCalledWith(1, `A`)
      expect(logSpy).toHaveBeenNthCalledWith(2, `B`)
      expect(logSpy).toHaveBeenNthCalledWith(3, `A`)
    })
  })

  describe(`if`, () => {
    test(`samples`, () => {
      expect(lits.run(`(if true :A :B)`)).toBe(`A`)
      expect(lits.run(`(if false :A :B)`)).toBe(`B`)
      expect(lits.run(`(if nil :A :B)`)).toBe(`B`)
      expect(lits.run(`(if true :A)`)).toBe(`A`)
      expect(lits.run(`(if false :A)`)).toBeNull()
      expect(lits.run(`(if nil :A)`)).toBeNull()
      expect(lits.run(`(if "" :A :B)`)).toBe(`B`)
      expect(lits.run(`(if :x :A :B)`)).toBe(`A`)
      expect(lits.run(`(if 0 :A :B)`)).toBe(`B`)
      expect(lits.run(`(if 1 :A :B)`)).toBe(`A`)
      expect(lits.run(`(if -1 :A :B)`)).toBe(`A`)
      expect(lits.run(`(if [] :A :B)`)).toBe(`A`)
      expect(lits.run(`(if (object) :A :B)`)).toBe(`A`)
      expect(() => lits.run(`(if)`)).toThrow()
      expect(() => lits.run(`(if true)`)).toThrow()
      expect(() => lits.run(`(if true :A :B :Q)`)).toThrow()
    })
    test(`That special form "if" only evaluate the correct path (true)`, () => {
      lits.run(`(if true (write! :A) (write! :B))`)
      expect(logSpy).toHaveBeenCalledWith(`A`)
      expect(logSpy).not.toHaveBeenCalledWith(`B`)
    })
    test(`That special form "if" only evaluate the correct path (false)`, () => {
      lits.run(`(if false (write! :A) (write! :B))`)
      expect(logSpy).not.toHaveBeenCalledWith(`A`)
      expect(logSpy).toHaveBeenCalledWith(`B`)
    })
  })

  describe(`if-not`, () => {
    test(`samples`, () => {
      expect(lits.run(`(if-not true :A :B)`)).toBe(`B`)
      expect(lits.run(`(if-not false :A :B)`)).toBe(`A`)
      expect(lits.run(`(if-not nil :A :B)`)).toBe(`A`)
      expect(lits.run(`(if-not true :A)`)).toBeNull()
      expect(lits.run(`(if-not false :A)`)).toBe(`A`)
      expect(lits.run(`(if-not nil :A)`)).toBe(`A`)
      expect(lits.run(`(if-not "" :A :B)`)).toBe(`A`)
      expect(lits.run(`(if-not :x :A :B)`)).toBe(`B`)
      expect(lits.run(`(if-not 0 :A :B)`)).toBe(`A`)
      expect(lits.run(`(if-not 1 :A :B)`)).toBe(`B`)
      expect(lits.run(`(if-not -1 :A :B)`)).toBe(`B`)
      expect(lits.run(`(if-not [] :A :B)`)).toBe(`B`)
      expect(lits.run(`(if-not (object) :A :B)`)).toBe(`B`)
      expect(() => lits.run(`(if-not)`)).toThrow()
      expect(() => lits.run(`(if-not true)`)).toThrow()
      expect(() => lits.run(`(if-not true :A :B :Q)`)).toThrow()
    })
    test(`That special form "if-not" only evaluate the correct path (true)`, () => {
      lits.run(`(if-not true (write! :A) (write! :B))`)
      expect(logSpy).toHaveBeenCalledWith(`B`)
      expect(logSpy).not.toHaveBeenCalledWith(`A`)
    })
    test(`That special form "if-not" only evaluate the correct path (false)`, () => {
      lits.run(`(if-not false (write! :A) (write! :B))`)
      expect(logSpy).not.toHaveBeenCalledWith(`B`)
      expect(logSpy).toHaveBeenCalledWith(`A`)
    })
  })

  describe(`if-let`, () => {
    test(`samples`, () => {
      expect(lits.run(`(if-let [a (> (count "Albert") 4)] a)`)).toBe(true)
      expect(lits.run(`(if-let [a (> (count "Albert") 10)] a)`)).toBeNull()
      expect(lits.run(`(if-let [a (> (count "Albert") 4)] "YES" "NO")`)).toBe(`YES`)
      expect(lits.run(`(if-let [a (> (count "Albert") 10)] "YES" "NO")`)).toBe(`NO`)
      expect(() => lits.run(`(if-let [a (> (count "Albert") 10)] "YES" a)`)).toThrow()
      expect(() => lits.run(`(if-let [a (> (count "Albert") 10)])`)).toThrow()
      expect(() => lits.run(`(if-let [a (> (count "Albert") 10) b 20] 1 2)`)).toThrow()
    })
  })

  describe(`when-let`, () => {
    test(`samples`, () => {
      expect(lits.run(`(when-let [a (> (count "Albert") 4)] a)`)).toBe(true)
      expect(lits.run(`(when-let [a (> (count "Albert") 10)] a)`)).toBeNull()
      expect(lits.run(`(when-let [a (> (count "Albert") 10)])`)).toBeNull()
      expect(lits.run(`(when-let [a (> (count "Albert") 10)] 10 20)`)).toBeNull()
      expect(lits.run(`(when-let [a (> (count "Albert") 4)] 10 20)`)).toBe(20)
      expect(() => lits.run(`(when-let [a (> (count "Albert") 10) b 20] 1)`)).toThrow()
    })
  })

  describe(`let`, () => {
    test(`samples`, () => {
      expect(lits.run(`(let [a :A] a)`)).toBe(`A`)
      expect(lits.run(`(let [a :A b :B] a b)`)).toBe(`B`)
      expect(lits.run(`(let [a :A b :B] a b)`)).toBe(`B`)
      expect(lits.run(`(let [a (+ 10 20) b :B] b a)`)).toBe(30)
      expect(lits.run(`(let [a (fn [] 1)] (a))`)).toBe(1)
      expect(() => lits.run(`(let)`)).toThrow()
      expect(() => lits.run(`(let ())`)).toThrow()
      expect(() => lits.run(`(let [)))`)).toThrow()
      expect(() => lits.run(`(let [let [b :B] b])`)).toThrow()
      expect(() => lits.run(`(let [a :A) b) a`)).toThrow()
      expect(() => lits.run(`(let (a :A]`)).toThrow()
      expect(() => lits.run(`(let (a :A) a)`)).toThrow()
    })
    test(`variables depend on each other`, () => {
      const program = `
      (let
        [
          year 2000
          month 1
          day 1
          leapYear
            (and
              (zero? (mod year 4))
              (or
                (not (zero? (mod year 100)))
                (zero? (mod year 400))
              )
            )
        ]
        leapYear
      )
      `
      expect(lits.run(program)).toBe(true)
    })
    test(`local and global variables`, () => {
      expect(() =>
        lits.run(`
          (let (
            (a :A)
            (b a)     ;Cannot access local variable a here. This is what let* whould be for
          )
            b
          )
        `),
      ).toThrow()
      expect(
        lits.run(`
          (def a :X)
          (let [
            a :A
            b a     ;a is the global variable
          ]
            b
          )
        `),
      ).toBe(`A`)
    })
  })

  describe(`and`, () => {
    test(`samples`, () => {
      expect(lits.run(`(and)`)).toBe(true)
      expect(lits.run(`(and 0)`)).toBe(0)
      expect(lits.run(`(and 0 1)`)).toBe(0)
      expect(lits.run(`(and 2 0)`)).toBe(0)
      expect(lits.run(`(and 2 0 1)`)).toBe(0)
      expect(lits.run(`(and 2 3 0)`)).toBe(0)
      expect(lits.run(`(and 2 3 "")`)).toBe(``)
      expect(lits.run(`(and 2 3 :x)`)).toBe(`x`)
      expect(lits.run(`(and false 1)`)).toBe(false)
      expect(lits.run(`(and 1 false)`)).toBe(false)
      expect(lits.run(`(and 1 nil)`)).toBe(null)
      expect(lits.run(`(and 2 2 false)`)).toBe(false)
      expect(lits.run(`(and 3 true 3)`)).toBe(3)
    })
    describe(`short circuit`, () => {
      test(`true, false`, () => {
        expect(lits.run(`(and (write! true) (write! false))`)).toBe(false)
        expect(logSpy).toHaveBeenNthCalledWith(1, true)
        expect(logSpy).toHaveBeenNthCalledWith(2, false)
      })
      test(`true, 1`, () => {
        expect(lits.run(`(and (write! true) (write! 1))`)).toBe(1)
        expect(logSpy).toHaveBeenNthCalledWith(1, true)
        expect(logSpy).toHaveBeenNthCalledWith(2, 1)
      })
      test(`false, true`, () => {
        expect(lits.run(`(and (write! false) (write! true))`)).toBe(false)
        expect(logSpy).toHaveBeenCalledWith(false)
        expect(logSpy).not.toHaveBeenCalledWith(true)
      })
      test(`false, true`, () => {
        expect(lits.run(`(and (write! false) (write! 0))`)).toBe(false)
        expect(logSpy).toHaveBeenCalledWith(false)
        expect(logSpy).not.toHaveBeenCalledWith(0)
      })
    })
  })

  describe(`or`, () => {
    test(`samples`, () => {
      expect(lits.run(`(or)`)).toBe(false)
      expect(lits.run(`(or 0)`)).toBe(0)
      expect(lits.run(`(or 0 1)`)).toBe(1)
      expect(lits.run(`(or 2 0)`)).toBe(2)
      expect(lits.run(`(or nil 0 false)`)).toBe(false)
      expect(lits.run(`(or nil 0 1)`)).toBe(1)
    })
    describe(`short circuit`, () => {
      test(`true, false`, () => {
        expect(lits.run(`(or (write! true) (write! false))`)).toBe(true)
        expect(logSpy).toHaveBeenCalledWith(true)
        expect(logSpy).not.toHaveBeenCalledWith(false)
      })
      test(`true, 1`, () => {
        expect(lits.run(`(or (write! true) (write! 1))`)).toBe(true)
        expect(logSpy).toHaveBeenCalledWith(true)
        expect(logSpy).not.toHaveBeenCalledWith(1)
      })
      test(`false, true`, () => {
        expect(lits.run(`(or (write! false) (write! true))`)).toBe(true)
        expect(logSpy).toHaveBeenNthCalledWith(1, false)
        expect(logSpy).toHaveBeenNthCalledWith(2, true)
      })
      test(`false, true`, () => {
        expect(lits.run(`(or (write! false) (write! 0))`)).toBe(0)
        expect(logSpy).toHaveBeenNthCalledWith(1, false)
        expect(logSpy).toHaveBeenNthCalledWith(2, 0)
      })
    })
  })

  describe(`cond`, () => {
    test(`samples`, () => {
      expect(lits.run(`(cond)`)).toBeNull()
      expect(lits.run(`(cond true 10 true 20)`)).toBe(10)
      expect(lits.run(`(cond true 10)`)).toBe(10)
      expect(lits.run(`(cond false 20 true (+ 5 5))`)).toBe(10)
      expect(
        lits.run(`(cond (> 5 10) 20 (> 10 10) (do (write! "Hej") (+ 5 5)) (>= 10 10) (do "This will work" (+ 5 5 5)))`),
      ).toBe(15)
      expect(() => lits.run(`(cond true 123)`)).not.toThrow()
      expect(() => lits.run(`(cond (true 123))`)).toThrow()
    })
    test(`middle condition true`, () => {
      expect(
        lits.run(`(cond (> 5 10) (write! 20) (>= 10 10) (+ 5 5) (write! (>= 10 10)) (do "This will work" (+ 5 5 5)))`),
      ).toBe(10)
      expect(logSpy).not.toHaveBeenCalled()
    })
  })

  describe(`defn`, () => {
    test(`samples`, () => {
      expect(lits.run(`(defn add [a b] (+ a b)) (add 1 2)`)).toBe(3)
      expect(lits.run(`(defn add [a b &let [x 10]] (+ a b x)) (add 1 2)`)).toBe(13)
      expect(() => lits.run(`(defn add [] 10)`)).not.toThrow()
      expect(() => lits.run(`(defn x [a a] 10)`)).toThrow()
      expect(() => lits.run(`(defn true [] 10)`)).toThrow()
      expect(() => lits.run(`(defn false [] 10)`)).toThrow()
      expect(() => lits.run(`(defn nil [] 10)`)).toThrow()
      expect(() => lits.run(`(defn add [:s] 10)`)).toThrow()
      expect(() => lits.run(`(defn "add" [a b] (+ a b))`)).toThrow()
      expect(() => lits.run(`(defn add 1 (+ a b))`)).toThrow()
      expect(() => lits.run(`(defn add [a b])`)).toThrow()
    })
    test(`call defn function`, () => {
      expect(lits.run(`(defn sumOneToN [n] (if (<= n 1) n (+ n (sumOneToN (- n 1))))) (sumOneToN 10)`)).toBe(55)
      expect(lits.run(`(defn applyWithVal [fun val] (fun val)) (applyWithVal inc 10)`)).toBe(11)
      expect(lits.run(`(defn applyWithVal [fun val] (fun val)) (applyWithVal inc 10)`)).toBe(11)
    })
  })

  describe(`defns`, () => {
    test(`samples`, () => {
      expect(lits.run(`(defns (str :a :d :d) [a b] (+ a b)) (add 1 2)`)).toBe(3)
      expect(() => lits.run(`(defns "add" [] 10)`)).not.toThrow()
      expect(() => lits.run(`(defns :x [a a] 10)`)).toThrow()
      expect(() => lits.run(`(defns true [] 10)`)).toThrow()
      expect(() => lits.run(`(defns false [] 10)`)).toThrow()
      expect(() => lits.run(`(defns nil [] 10)`)).toThrow()
      expect(() => lits.run(`(defns add [:s] 10)`)).toThrow()
      expect(() => lits.run(`(defns add 1 (+ a b))`)).toThrow()
      expect(() => lits.run(`(defns add (a b))`)).toThrow()
    })
    test(`call defns function`, () => {
      expect(lits.run(`(defns "sumOneToN" [n] (if (<= n 1) n (+ n (sumOneToN (- n 1))))) (sumOneToN 10)`)).toBe(55)
      expect(lits.run(`(defns "applyWithVal" [fun val] (fun val)) (applyWithVal inc 10)`)).toBe(11)
    })
  })

  describe(`fn`, () => {
    test(`samples`, () => {
      lits.run(`(fn [x] (+ x 1))`)
      lits.run(`(fn [] 1)`)
      expect(() => lits.run(`((fn [x] (+ y 1)) 10)`)).toThrow()
      expect(() => lits.run(`(fn (false) 1)`)).toThrow()
      expect(() => lits.run(`(fn (true) 1)`)).toThrow()
      expect(() => lits.run(`(fn (nil) 1)`)).toThrow()
      expect(() => lits.run(`(fn)`)).toThrow()
      expect(() => lits.run(`(fn [x])`)).toThrow()
      expect(() => lits.run(`(fn :k)`)).toThrow()
      expect(() => lits.run(`(fn k s)`)).toThrow()
      expect(() => lits.run(`(fn add)`)).toThrow()
    })
  })

  describe(`try`, () => {
    test(`samples`, () => {
      expect(lits.run(`(try (/ 2 4) ((error) 1))`)).toBe(0.5)
      expect(lits.run(`(try (throw "oops") ((error) 1))`)).toBe(1)
      expect(lits.run(`(try (throw "oops") ((error) error))`)).toBeInstanceOf(Error)
      expect(() => lits.run(`(try (/ 2 4) 1)`)).toThrow()
      expect(() => lits.run(`(try (/ 2 4) (1))`)).toThrow()
      expect(() => lits.run(`(try (/ 2 4) (("error") 1))`)).toThrow()
      expect(() => lits.run(`(try (/ 2 4) ((error1 error2) 1))`)).toThrow()
      expect(() => lits.run(`(try (/ 2 4) ((error) 1 2))`)).toThrow()
      expect(() => lits.run(`(try (/ 2 4) ((error) 1 )2)`)).toThrow()
    })
  })

  describe(`throw`, () => {
    test(`samples`, () => {
      expect(() => lits.run(`(throw "An error")`)).toThrowError(UserDefinedError)
      expect(() => lits.run(`(throw (subs "An error" 3))`)).toThrowError(UserDefinedError)
      expect(() => lits.run(`(throw "An error" 10)`)).not.toThrowError(UserDefinedError)
      expect(() => lits.run(`(throw "An error" 10)`)).toThrow()
      try {
        lits.run(`(throw (subs "An error" 3))`)
        throw Error()
      } catch (error) {
        expect((error as UserDefinedError).message).toBe(`error`)
      }
    })
  })

  describe(`when`, () => {
    test(`samples`, () => {
      expect(lits.run(`(when true (write! 10) (write! 20))`)).toBe(20)
      expect(lits.run(`(when "Charles" (write! 10) (write! 20))`)).toBe(20)
      expect(lits.run(`(when false)`)).toBeNull()
      expect(lits.run(`(when true)`)).toBeNull()
      expect(() => lits.run(`(when)`)).toThrow()
    })
  })

  describe(`when-not`, () => {
    test(`samples`, () => {
      expect(lits.run(`(when-not true (write! 10) (write! 20))`)).toBeNull()
      expect(lits.run(`(when-not (> 10 20) (write! 10) (write! 20))`)).toBe(20)
      expect(lits.run(`(when-not false)`)).toBeNull()
      expect(lits.run(`(when-not true)`)).toBeNull()
      expect(() => lits.run(`(when-not)`)).toThrow()
    })
  })

  describe(`when-first`, () => {
    test(`samples`, () => {
      expect(lits.run(`(when-first [x [1 2 3]] (write! 10) (write! 20) x)`)).toBe(1)
      expect(lits.run(`(when-first [x []] (write! 10) (write! 20) x)`)).toBeNull()
      expect(lits.run(`(when-first [x "Albert"] (write! 10) (write! 20) x)`)).toBe(`A`)
      expect(lits.run(`(when-first [x ""] (write! 10) (write! 20) x)`)).toBeNull()
      expect(lits.run(`(when-first [x [0]])`)).toBeNull()
      expect(() => lits.run(`(when-first [x nil] x)`)).toThrow()
      expect(() => lits.run(`(when-first [x nil a 2] x)`)).toThrow()
      expect(() => lits.run(`(when-first [] x)`)).toThrow()
      expect(() => lits.run(`(when-first x 10)`)).toThrow()
    })
  })

  describe(`do`, () => {
    test(`samples`, () => {
      expect(lits.run(`(do [1 2 3] "[1]" (+ 1 2))`)).toBe(3)
      expect(lits.run(`(do (object :a 1) :a)`)).toBe(`a`)
      expect(lits.run(`(do)`)).toBeNull()
    })
  })

  describe(`recur`, () => {
    test(`should work with defn`, () => {
      lits.run(`(defn foo [n] (write! n) (when (not (zero? n)) (recur (dec n)))) (foo 3)`)
      expect(logSpy).toHaveBeenNthCalledWith(1, 3)
      expect(logSpy).toHaveBeenNthCalledWith(2, 2)
      expect(logSpy).toHaveBeenNthCalledWith(3, 1)
      expect(logSpy).toHaveBeenNthCalledWith(4, 0)
    })
    test(`recur must be called with the right number of parameters`, () => {
      expect(() => lits.run(`(defn foo [n &opt m] (write! n m) (when (not (zero? n)) (recur))) (foo 3)`)).toThrow()
      expect(() =>
        lits.run(`(defn foo [n &opt m] (write! n m) (when (not (zero? n)) (recur (dec n)))) (foo 3)`),
      ).not.toThrow()
      expect(() =>
        lits.run(`(defn foo [n &opt m] (write! n m) (when (not (zero? n)) (recur (dec n) 1))) (foo 3)`),
      ).not.toThrow()
      expect(() =>
        lits.run(`(defn foo [n &opt m] (write! n m) (when (not (zero? n)) (recur (dec n) 1 2))) (foo 3)`),
      ).toThrow()
      expect(() => lits.run(`((fn [n &opt m] (write! n m) (when (not (zero? n)) (recur))) 3)`)).toThrow()
      expect(() => lits.run(`((fn [n &opt m] (write! n m) (when (not (zero? n)) (recur (dec n)))) 3)`)).not.toThrow()
      expect(() => lits.run(`((fn [n &opt m] (write! n m) (when (not (zero? n)) (recur (dec n) 1))) 3)`)).not.toThrow()
      expect(() => lits.run(`((fn [n &opt m] (write! n m) (when (not (zero? n)) (recur (dec n) 1 2))) 3)`)).toThrow()
    })
  })

  describe(`loop`, () => {
    test(`should work with recur`, () => {
      lits.run(`(loop [n 3] (write! n) (when (not (zero? n)) (recur (dec n))))`)
      expect(logSpy).toHaveBeenNthCalledWith(1, 3)
      expect(logSpy).toHaveBeenNthCalledWith(2, 2)
      expect(logSpy).toHaveBeenNthCalledWith(3, 1)
      expect(logSpy).toHaveBeenNthCalledWith(4, 0)
    })
    test(`recur must be called with right number of parameters`, () => {
      expect(() => lits.run(`(loop [n 3] (write! n) (when (not (zero? n)) (recur (dec n) 2)))`)).toThrow()
      expect(() => lits.run(`(loop [n 3] (write! n) (when (not (zero? n)) (recur)))`)).toThrow()
    })
    test(`throw should work`, () => {
      expect(() => lits.run(`(loop [n 3] (write! n) (when (not (zero? n)) (throw (dec n))))`)).toThrow()
    })
  })
  describe(`time!`, () => {
    test(`samples`, () => {
      expect(lits.run(`(time! (+ 1 2)`)).toBe(3)
      expect(lastLog).toMatch(/Elapsed time: \d+ ms/)
    })
  })

  describe(`for`, () => {
    test(`samples`, () => {
      expect(lits.run(`(for [x []] x)`)).toEqual([])
      expect(lits.run(`(for [x [1 2 3] y []] x)`)).toEqual([])
      expect(lits.run(`(for [x [] y [1 2 3]] x)`)).toEqual([])

      expect(lits.run(`(for [x "Al" y [1 2]] (repeat y x))`)).toEqual([[`A`], [`A`, `A`], [`l`], [`l`, `l`]])
      expect(lits.run(`(for [x {:a 10 :b 20} y [1 2]] (repeat y x))`)).toEqual([
        [[`a`, 10]],
        [
          [`a`, 10],
          [`a`, 10],
        ],
        [[`b`, 20]],
        [
          [`b`, 20],
          [`b`, 20],
        ],
      ])

      expect(lits.run(`(for [x [1 2] y [1 10]] (* x y))`)).toEqual([1, 10, 2, 20])
      expect(lits.run(`(for [x [1 2] &let [z (* x x x)]] z)`)).toEqual([1, 8])
      expect(lits.run(`(for [x [1 2] y [x (* 2 x)]] (* x y))`)).toEqual([1, 2, 4, 8])

      expect(lits.run(`(for [x [0 1 2 3 4 5] &let [y (* x 3)] &when (even? y)] y)`)).toEqual([0, 6, 12])
      expect(lits.run(`(for [x [0 1 2 3 4 5] &let [y (* x 3)] &while (even? y)] y)`)).toEqual([0])

      expect(lits.run(`(for [x [1 2 3] y [1 2 3] &while (<= x y) z [1 2 3]] [x y z])`)).toEqual([
        [1, 1, 1],
        [1, 1, 2],
        [1, 1, 3],
        [1, 2, 1],
        [1, 2, 2],
        [1, 2, 3],
        [1, 3, 1],
        [1, 3, 2],
        [1, 3, 3],
      ])
      expect(lits.run(`(for [x [1 2 3] y [1 2 3] z [1 2 3] &while (<= x y)] [x y z])`)).toEqual([
        [1, 1, 1],
        [1, 1, 2],
        [1, 1, 3],
        [1, 2, 1],
        [1, 2, 2],
        [1, 2, 3],
        [1, 3, 1],
        [1, 3, 2],
        [1, 3, 3],
        [2, 2, 1],
        [2, 2, 2],
        [2, 2, 3],
        [2, 3, 1],
        [2, 3, 2],
        [2, 3, 3],
        [3, 3, 1],
        [3, 3, 2],
        [3, 3, 3],
      ])
      expect(() => lits.run(`(for [x [0 1 2 3 4 5] &opt [y (* x 3)] &while (even? y)] y)`)).toThrow()
      expect(() => lits.run(`(for [x [0 1 2 3 4 5] &let [x 10]] y)`)).toThrow()
      expect(() => lits.run(`(for x [0 1 2 3 4 5] y)`)).toThrow()
      expect(() => lits.run(`(for [x [0 1 2 3 4 5]] x y)`)).toThrow()
      expect(() => lits.run(`(for [x [0 1 2 3 4 5] x [10 20]] x)`)).toThrow()
    })
  })
})
