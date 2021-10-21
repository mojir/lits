/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lispish } from '../../src'
import { UserDefinedError } from '../../src/errors'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
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
      expect(lispish.run(`(defs "a" 10) a`)).toBe(10)
      expect(lispish.run(`(defs "a" "b") (defs a "c") b`)).toBe(`c`)
      expect(lispish.run(`(defs (str "a" "1") 20) a1`)).toBe(20)
      expect(() => lispish.run(`(defs true false)`)).toThrow()
      expect(() => lispish.run(`(defs a)`)).toThrow()
      expect(() => lispish.run(`(defs a 10 10)`)).toThrow()
      expect(() => lispish.run(`(defs 1 10)`)).toThrow()
      expect(() => lispish.run(`(defs null 10)`)).toThrow()
      expect(() => lispish.run(`(defs false 10)`)).toThrow()
      expect(() => lispish.run(`(defs true 10)`)).toThrow()
      expect(() => lispish.run(`(defs [] 10)`)).toThrow()
      expect(() => lispish.run(`(defs (object) 10)`)).toThrow()
      expect(() => lispish.run(`(defs a 10)`)).toThrow()
    })
  })

  describe(`def`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(def a 10) a`)).toBe(10)
      expect(lispish.run(`(do (def a 10)) a`)).toBe(10)
      expect(() => lispish.run(`(def a 10) (def a 20) a`)).toThrow()
      expect(() => lispish.run(`(def true false)`)).toThrow()
      expect(() => lispish.run(`(def a)`)).toThrow()
      expect(() => lispish.run(`(def a 10 10)`)).toThrow()
      expect(() => lispish.run(`(def 1 10)`)).toThrow()
      expect(() => lispish.run(`(def null 10)`)).toThrow()
      expect(() => lispish.run(`(def false 10)`)).toThrow()
      expect(() => lispish.run(`(def true 10)`)).toThrow()
      expect(() => lispish.run(`(def [] 10)`)).toThrow()
      expect(() => lispish.run(`(def (object) 10)`)).toThrow()
      expect(() => lispish.run(`(def "a" 10)`)).toThrow()
    })

    test(`local variable`, () => {
      const program = `
        (def x "A")     ;Global variable x
        (write! x)       ;"A"
        (let [x "B"]    ;Local variable x
          (write! x)     ;"B"
        )
        (write! x)       ;"A" - global variable x
      `
      lispish.run(program)
      expect(logSpy).toHaveBeenNthCalledWith(1, `A`)
      expect(logSpy).toHaveBeenNthCalledWith(2, `B`)
      expect(logSpy).toHaveBeenNthCalledWith(3, `A`)
    })
  })

  describe(`if`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(if true "A" "B")`)).toBe(`A`)
      expect(lispish.run(`(if false "A" "B")`)).toBe(`B`)
      expect(lispish.run(`(if null "A" "B")`)).toBe(`B`)
      expect(lispish.run(`(if true "A")`)).toBe(`A`)
      expect(lispish.run(`(if false "A")`)).toBeNull()
      expect(lispish.run(`(if null "A")`)).toBeNull()
      expect(lispish.run(`(if "" "A" "B")`)).toBe(`B`)
      expect(lispish.run(`(if "x" "A" "B")`)).toBe(`A`)
      expect(lispish.run(`(if 0 "A" "B")`)).toBe(`B`)
      expect(lispish.run(`(if 1 "A" "B")`)).toBe(`A`)
      expect(lispish.run(`(if -1 "A" "B")`)).toBe(`A`)
      expect(lispish.run(`(if [] "A" "B")`)).toBe(`A`)
      expect(lispish.run(`(if (object) "A" "B")`)).toBe(`A`)
      expect(() => lispish.run(`(if)`)).toThrow()
      expect(() => lispish.run(`(if true)`)).toThrow()
      expect(() => lispish.run(`(if true "A" "B" "Q")`)).toThrow()
    })
    test(`That special form "if" only evaluate the correct path (true)`, () => {
      lispish.run(`(if true (write! "A") (write! "B"))`)
      expect(logSpy).toHaveBeenCalledWith(`A`)
      expect(logSpy).not.toHaveBeenCalledWith(`B`)
    })
    test(`That special form "if" only evaluate the correct path (false)`, () => {
      lispish.run(`(if false (write! "A") (write! "B"))`)
      expect(logSpy).not.toHaveBeenCalledWith(`A`)
      expect(logSpy).toHaveBeenCalledWith(`B`)
    })
  })

  describe(`if-not`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(if-not true "A" "B")`)).toBe(`B`)
      expect(lispish.run(`(if-not false "A" "B")`)).toBe(`A`)
      expect(lispish.run(`(if-not null "A" "B")`)).toBe(`A`)
      expect(lispish.run(`(if-not true "A")`)).toBeNull()
      expect(lispish.run(`(if-not false "A")`)).toBe(`A`)
      expect(lispish.run(`(if-not null "A")`)).toBe(`A`)
      expect(lispish.run(`(if-not "" "A" "B")`)).toBe(`A`)
      expect(lispish.run(`(if-not "x" "A" "B")`)).toBe(`B`)
      expect(lispish.run(`(if-not 0 "A" "B")`)).toBe(`A`)
      expect(lispish.run(`(if-not 1 "A" "B")`)).toBe(`B`)
      expect(lispish.run(`(if-not -1 "A" "B")`)).toBe(`B`)
      expect(lispish.run(`(if-not [] "A" "B")`)).toBe(`B`)
      expect(lispish.run(`(if-not (object) "A" "B")`)).toBe(`B`)
      expect(() => lispish.run(`(if-not)`)).toThrow()
      expect(() => lispish.run(`(if-not true)`)).toThrow()
      expect(() => lispish.run(`(if-not true "A" "B" "Q")`)).toThrow()
    })
    test(`That special form "if-not" only evaluate the correct path (true)`, () => {
      lispish.run(`(if-not true (write! "A") (write! "B"))`)
      expect(logSpy).toHaveBeenCalledWith(`B`)
      expect(logSpy).not.toHaveBeenCalledWith(`A`)
    })
    test(`That special form "if-not" only evaluate the correct path (false)`, () => {
      lispish.run(`(if-not false (write! "A") (write! "B"))`)
      expect(logSpy).not.toHaveBeenCalledWith(`B`)
      expect(logSpy).toHaveBeenCalledWith(`A`)
    })
  })

  describe(`if-let`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(if-let [a (> (count "Albert") 4)] a)`)).toBe(true)
      expect(lispish.run(`(if-let [a (> (count "Albert") 10)] a)`)).toBeNull()
      expect(lispish.run(`(if-let [a (> (count "Albert") 4)] "YES" "NO")`)).toBe(`YES`)
      expect(lispish.run(`(if-let [a (> (count "Albert") 10)] "YES" "NO")`)).toBe(`NO`)
      expect(() => lispish.run(`(if-let [a (> (count "Albert") 10)] "YES" a)`)).toThrow()
      expect(() => lispish.run(`(if-let [a (> (count "Albert") 10)])`)).toThrow()
      expect(() => lispish.run(`(if-let [a (> (count "Albert") 10) b 20] 1 2)`)).toThrow()
    })
  })

  describe(`when-let`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(when-let [a (> (count "Albert") 4)] a)`)).toBe(true)
      expect(lispish.run(`(when-let [a (> (count "Albert") 10)] a)`)).toBeNull()
      expect(lispish.run(`(when-let [a (> (count "Albert") 10)])`)).toBeNull()
      expect(lispish.run(`(when-let [a (> (count "Albert") 10)] 10 20)`)).toBeNull()
      expect(lispish.run(`(when-let [a (> (count "Albert") 4)] 10 20)`)).toBe(20)
      expect(() => lispish.run(`(when-let [a (> (count "Albert") 10) b 20] 1)`)).toThrow()
    })
  })

  describe(`let`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(let [a "A"] a)`)).toBe(`A`)
      expect(lispish.run(`(let [a "A" b "B"] a b)`)).toBe(`B`)
      expect(lispish.run(`(let [a "A" b "B"] a b)`)).toBe(`B`)
      expect(lispish.run(`(let [a (+ 10 20) b "B"] b a)`)).toBe(30)
      expect(lispish.run(`(let [a (fn [] 1)] (a))`)).toBe(1)
      expect(() => lispish.run(`(let)`)).toThrow()
      expect(() => lispish.run(`(let ())`)).toThrow()
      expect(() => lispish.run(`(let [)))`)).toThrow()
      expect(() => lispish.run(`(let [let [b "B"] b])`)).toThrow()
      expect(() => lispish.run(`(let [a "A") b) a`)).toThrow()
      expect(() => lispish.run(`(let (a "A"]`)).toThrow()
      expect(() => lispish.run(`(let (a "A") a)`)).toThrow()
    })
    test(`local and global variables`, () => {
      expect(() =>
        lispish.run(`
          (let (
            (a "A")
            (b a)     ;Cannot access local variable a here. This is what let* whould be for
          )
            b
          )
        `),
      ).toThrow()
      expect(
        lispish.run(`
          (def a "X")
          (let [
            a "A"
            b a     ;a is the global variable
          ]
            b
          )
        `),
      ).toBe(`X`)
    })
  })

  describe(`and`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(and)`)).toBe(true)
      expect(lispish.run(`(and 0)`)).toBe(0)
      expect(lispish.run(`(and 0 1)`)).toBe(0)
      expect(lispish.run(`(and 2 0)`)).toBe(0)
      expect(lispish.run(`(and 2 0 1)`)).toBe(0)
      expect(lispish.run(`(and 2 3 0)`)).toBe(0)
      expect(lispish.run(`(and 2 3 "")`)).toBe(``)
      expect(lispish.run(`(and 2 3 "x")`)).toBe(`x`)
      expect(lispish.run(`(and false 1)`)).toBe(false)
      expect(lispish.run(`(and 1 false)`)).toBe(false)
      expect(lispish.run(`(and 1 null)`)).toBe(null)
      expect(lispish.run(`(and 2 2 false)`)).toBe(false)
      expect(lispish.run(`(and 3 true 3)`)).toBe(3)
    })
    describe(`short circuit`, () => {
      test(`true, false`, () => {
        expect(lispish.run(`(and (write! true) (write! false))`)).toBe(false)
        expect(logSpy).toHaveBeenNthCalledWith(1, true)
        expect(logSpy).toHaveBeenNthCalledWith(2, false)
      })
      test(`true, 1`, () => {
        expect(lispish.run(`(and (write! true) (write! 1))`)).toBe(1)
        expect(logSpy).toHaveBeenNthCalledWith(1, true)
        expect(logSpy).toHaveBeenNthCalledWith(2, 1)
      })
      test(`false, true`, () => {
        expect(lispish.run(`(and (write! false) (write! true))`)).toBe(false)
        expect(logSpy).toHaveBeenCalledWith(false)
        expect(logSpy).not.toHaveBeenCalledWith(true)
      })
      test(`false, true`, () => {
        expect(lispish.run(`(and (write! false) (write! 0))`)).toBe(false)
        expect(logSpy).toHaveBeenCalledWith(false)
        expect(logSpy).not.toHaveBeenCalledWith(0)
      })
    })
  })

  describe(`or`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(or)`)).toBe(false)
      expect(lispish.run(`(or 0)`)).toBe(0)
      expect(lispish.run(`(or 0 1)`)).toBe(1)
      expect(lispish.run(`(or 2 0)`)).toBe(2)
      expect(lispish.run(`(or null 0 false)`)).toBe(false)
      expect(lispish.run(`(or null 0 1)`)).toBe(1)
    })
    describe(`short circuit`, () => {
      test(`true, false`, () => {
        expect(lispish.run(`(or (write! true) (write! false))`)).toBe(true)
        expect(logSpy).toHaveBeenCalledWith(true)
        expect(logSpy).not.toHaveBeenCalledWith(false)
      })
      test(`true, 1`, () => {
        expect(lispish.run(`(or (write! true) (write! 1))`)).toBe(true)
        expect(logSpy).toHaveBeenCalledWith(true)
        expect(logSpy).not.toHaveBeenCalledWith(1)
      })
      test(`false, true`, () => {
        expect(lispish.run(`(or (write! false) (write! true))`)).toBe(true)
        expect(logSpy).toHaveBeenNthCalledWith(1, false)
        expect(logSpy).toHaveBeenNthCalledWith(2, true)
      })
      test(`false, true`, () => {
        expect(lispish.run(`(or (write! false) (write! 0))`)).toBe(0)
        expect(logSpy).toHaveBeenNthCalledWith(1, false)
        expect(logSpy).toHaveBeenNthCalledWith(2, 0)
      })
    })
  })

  describe(`cond`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(cond)`)).toBeNull()
      expect(lispish.run(`(cond true 10 true 20)`)).toBe(10)
      expect(lispish.run(`(cond true 10)`)).toBe(10)
      expect(lispish.run(`(cond false 20 true (+ 5 5))`)).toBe(10)
      expect(
        lispish.run(
          `(cond (> 5 10) 20 (> 10 10) (do (write! "Hej") (+ 5 5)) (>= 10 10) (do "This will work" (+ 5 5 5)))`,
        ),
      ).toBe(15)
      expect(() => lispish.run(`(cond true 123)`)).not.toThrow()
      expect(() => lispish.run(`(cond (true 123))`)).toThrow()
    })
    test(`middle condition true`, () => {
      expect(
        lispish.run(
          `(cond (> 5 10) (write! 20) (>= 10 10) (+ 5 5) (write! (>= 10 10)) (do "This will work" (+ 5 5 5)))`,
        ),
      ).toBe(10)
      expect(logSpy).not.toHaveBeenCalled()
    })
  })

  describe(`defn`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(defn add [a b] (+ a b)) (add 1 2)`)).toBe(3)
      expect(lispish.run(`(defn add [a b &bind [x 10]] (+ a b x)) (add 1 2)`)).toBe(13)
      expect(() => lispish.run(`(defn add [] 10)`)).not.toThrow()
      expect(() => lispish.run(`(defn x [a a] 10)`)).toThrow()
      expect(() => lispish.run(`(defn true [] 10)`)).toThrow()
      expect(() => lispish.run(`(defn false [] 10)`)).toThrow()
      expect(() => lispish.run(`(defn null [] 10)`)).toThrow()
      expect(() => lispish.run(`(defn add ["s"] 10)`)).toThrow()
      expect(() => lispish.run(`(defn "add" [a b] (+ a b))`)).toThrow()
      expect(() => lispish.run(`(defn add 1 (+ a b))`)).toThrow()
      expect(() => lispish.run(`(defn add [a b])`)).toThrow()
    })
    test(`call defn function`, () => {
      expect(lispish.run(`(defn sumOneToN [n] (if (<= n 1) n (+ n (sumOneToN (- n 1))))) (sumOneToN 10)`)).toBe(55)
      expect(lispish.run(`(defn applyWithVal [fun val] (fun val)) (applyWithVal inc 10)`)).toBe(11)
      expect(lispish.run(`(defn applyWithVal [fun val] (fun val)) (applyWithVal inc 10)`)).toBe(11)
    })
  })

  describe(`defns`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(defns (str "a" "d" "d") [a b] (+ a b)) (add 1 2)`)).toBe(3)
      expect(() => lispish.run(`(defns "add" [] 10)`)).not.toThrow()
      expect(() => lispish.run(`(defns "x" [a a] 10)`)).toThrow()
      expect(() => lispish.run(`(defns true [] 10)`)).toThrow()
      expect(() => lispish.run(`(defns false [] 10)`)).toThrow()
      expect(() => lispish.run(`(defns null [] 10)`)).toThrow()
      expect(() => lispish.run(`(defns add ["s"] 10)`)).toThrow()
      expect(() => lispish.run(`(defns add 1 (+ a b))`)).toThrow()
      expect(() => lispish.run(`(defns add (a b))`)).toThrow()
    })
    test(`call defns function`, () => {
      expect(lispish.run(`(defns "sumOneToN" [n] (if (<= n 1) n (+ n (sumOneToN (- n 1))))) (sumOneToN 10)`)).toBe(55)
      expect(lispish.run(`(defns "applyWithVal" [fun val] (fun val)) (applyWithVal inc 10)`)).toBe(11)
    })
  })

  describe(`fn`, () => {
    test(`samples`, () => {
      lispish.run(`(fn [x] (+ x 1))`)
      lispish.run(`(fn [] 1)`)
      expect(() => lispish.run(`((fn [x] (+ y 1)) 10)`)).toThrow()
      expect(() => lispish.run(`(fn (false) 1)`)).toThrow()
      expect(() => lispish.run(`(fn (true) 1)`)).toThrow()
      expect(() => lispish.run(`(fn (null) 1)`)).toThrow()
      expect(() => lispish.run(`(fn)`)).toThrow()
      expect(() => lispish.run(`(fn [x])`)).toThrow()
      expect(() => lispish.run(`(fn "k")`)).toThrow()
      expect(() => lispish.run(`(fn k s)`)).toThrow()
      expect(() => lispish.run(`(fn add)`)).toThrow()
    })
  })

  describe(`try`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(try (/ 2 4) ((error) 1))`)).toBe(0.5)
      expect(lispish.run(`(try (throw "oops") ((error) 1))`)).toBe(1)
      expect(lispish.run(`(try (throw "oops") ((error) error))`)).toBeInstanceOf(Error)
      expect(() => lispish.run(`(try (/ 2 4) 1)`)).toThrow()
      expect(() => lispish.run(`(try (/ 2 4) (1))`)).toThrow()
      expect(() => lispish.run(`(try (/ 2 4) (("error") 1))`)).toThrow()
      expect(() => lispish.run(`(try (/ 2 4) ((error1 error2) 1))`)).toThrow()
      expect(() => lispish.run(`(try (/ 2 4) ((error) 1 2))`)).toThrow()
      expect(() => lispish.run(`(try (/ 2 4) ((error) 1 )2)`)).toThrow()
    })
  })

  describe(`throw`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(throw "An error")`)).toThrowError(UserDefinedError)
      expect(() => lispish.run(`(throw (subs "An error" 3))`)).toThrowError(UserDefinedError)
      expect(() => lispish.run(`(throw "An error" 10)`)).not.toThrowError(UserDefinedError)
      expect(() => lispish.run(`(throw "An error" 10)`)).toThrow()
      try {
        lispish.run(`(throw (subs "An error" 3))`)
        throw Error()
      } catch (error) {
        expect((error as UserDefinedError).message).toBe(`error`)
      }
    })
  })

  describe(`when`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(when true (write! 10) (write! 20))`)).toBe(20)
      expect(lispish.run(`(when "Charles" (write! 10) (write! 20))`)).toBe(20)
      expect(lispish.run(`(when false)`)).toBeNull()
      expect(lispish.run(`(when true)`)).toBeNull()
      expect(() => lispish.run(`(when)`)).toThrow()
    })
  })

  describe(`when-not`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(when-not true (write! 10) (write! 20))`)).toBeNull()
      expect(lispish.run(`(when-not (> 10 20) (write! 10) (write! 20))`)).toBe(20)
      expect(lispish.run(`(when-not false)`)).toBeNull()
      expect(lispish.run(`(when-not true)`)).toBeNull()
      expect(() => lispish.run(`(when-not)`)).toThrow()
    })
  })

  describe(`when-first`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(when-first [x [1 2 3]] (write! 10) (write! 20) x)`)).toBe(1)
      expect(lispish.run(`(when-first [x []] (write! 10) (write! 20) x)`)).toBeNull()
      expect(lispish.run(`(when-first [x "Albert"] (write! 10) (write! 20) x)`)).toBe(`A`)
      expect(lispish.run(`(when-first [x ""] (write! 10) (write! 20) x)`)).toBeNull()
      expect(lispish.run(`(when-first [x [0]])`)).toBeNull()
      expect(() => lispish.run(`(when-first [x null] x)`)).toThrow()
      expect(() => lispish.run(`(when-first [x null a 2] x)`)).toThrow()
      expect(() => lispish.run(`(when-first [] x)`)).toThrow()
      expect(() => lispish.run(`(when-first x 10)`)).toThrow()
    })
  })

  describe(`do`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(do [1 2 3] "[1]" (+ 1 2))`)).toBe(3)
      expect(lispish.run(`(do (object "a" 1) "a")`)).toBe(`a`)
      expect(lispish.run(`(do)`)).toBeNull()
    })
  })

  describe(`recur`, () => {
    test(`should work with defn`, () => {
      lispish.run(`(defn foo [n] (write! n) (when (not (zero? n)) (recur (dec n)))) (foo 3)`)
      expect(logSpy).toHaveBeenNthCalledWith(1, 3)
      expect(logSpy).toHaveBeenNthCalledWith(2, 2)
      expect(logSpy).toHaveBeenNthCalledWith(3, 1)
      expect(logSpy).toHaveBeenNthCalledWith(4, 0)
    })
    test(`recur must be called with the right number of parameters`, () => {
      expect(() => lispish.run(`(defn foo [n &opt m] (write! n m) (when (not (zero? n)) (recur))) (foo 3)`)).toThrow()
      expect(() =>
        lispish.run(`(defn foo [n &opt m] (write! n m) (when (not (zero? n)) (recur (dec n)))) (foo 3)`),
      ).not.toThrow()
      expect(() =>
        lispish.run(`(defn foo [n &opt m] (write! n m) (when (not (zero? n)) (recur (dec n) 1))) (foo 3)`),
      ).not.toThrow()
      expect(() =>
        lispish.run(`(defn foo [n &opt m] (write! n m) (when (not (zero? n)) (recur (dec n) 1 2))) (foo 3)`),
      ).toThrow()
      expect(() => lispish.run(`((fn [n &opt m] (write! n m) (when (not (zero? n)) (recur))) 3)`)).toThrow()
      expect(() => lispish.run(`((fn [n &opt m] (write! n m) (when (not (zero? n)) (recur (dec n)))) 3)`)).not.toThrow()
      expect(() =>
        lispish.run(`((fn [n &opt m] (write! n m) (when (not (zero? n)) (recur (dec n) 1))) 3)`),
      ).not.toThrow()
      expect(() => lispish.run(`((fn [n &opt m] (write! n m) (when (not (zero? n)) (recur (dec n) 1 2))) 3)`)).toThrow()
    })
  })

  describe(`loop`, () => {
    test(`should work with recur`, () => {
      lispish.run(`(loop [n 3] (write! n) (when (not (zero? n)) (recur (dec n))))`)
      expect(logSpy).toHaveBeenNthCalledWith(1, 3)
      expect(logSpy).toHaveBeenNthCalledWith(2, 2)
      expect(logSpy).toHaveBeenNthCalledWith(3, 1)
      expect(logSpy).toHaveBeenNthCalledWith(4, 0)
    })
    test(`recur must be called with right number of parameters`, () => {
      expect(() => lispish.run(`(loop [n 3] (write! n) (when (not (zero? n)) (recur (dec n) 2)))`)).toThrow()
      expect(() => lispish.run(`(loop [n 3] (write! n) (when (not (zero? n)) (recur)))`)).toThrow()
    })
    test(`throw should work`, () => {
      expect(() => lispish.run(`(loop [n 3] (write! n) (when (not (zero? n)) (throw (dec n))))`)).toThrow()
    })
  })
  describe(`time!`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(time! (+ 1 2)`)).toBe(3)
      expect(lastLog).toMatch(/Elapsed time: \d+ ms/)
    })
  })
})
