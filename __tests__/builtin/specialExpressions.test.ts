/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lispish } from '../../src'
import { ReturnFromSignal, ReturnSignal, UserDefinedError } from '../../src/errors'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`specialExpressions`, () => {
  let oldLog: () => void
  let logSpy: jest.Mock<any, any>
  beforeEach(() => {
    oldLog = console.log
    logSpy = jest.fn()
    console.log = logSpy
  })
  afterEach(() => {
    console.log = oldLog
  })

  describe(`defs`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(defs "a" 10) a`)).toBe(10)
      expect(lispish.run(`(defs "a" "b") (defs a "c") b`)).toBe(`c`)
      expect(lispish.run(`(defs (concat "a" "1") 20) a1`)).toBe(20)
      expect(() => lispish.run(`(defs true false)`)).toThrow()
      expect(() => lispish.run(`(defs a)`)).toThrow()
      expect(() => lispish.run(`(defs a 10 10)`)).toThrow()
      expect(() => lispish.run(`(defs 1 10)`)).toThrow()
      expect(() => lispish.run(`(defs null 10)`)).toThrow()
      expect(() => lispish.run(`(defs undefined 10)`)).toThrow()
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
      expect(() => lispish.run(`(def undefined 10)`)).toThrow()
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
      expect(lispish.run(`(if undefined "A" "B")`)).toBe(`B`)
      expect(lispish.run(`(if "" "A" "B")`)).toBe(`B`)
      expect(lispish.run(`(if "x" "A" "B")`)).toBe(`A`)
      expect(lispish.run(`(if 0 "A" "B")`)).toBe(`B`)
      expect(lispish.run(`(if 1 "A" "B")`)).toBe(`A`)
      expect(lispish.run(`(if -1 "A" "B")`)).toBe(`A`)
      expect(lispish.run(`(if [] "A" "B")`)).toBe(`A`)
      expect(lispish.run(`(if (object) "A" "B")`)).toBe(`A`)
      expect(() => lispish.run(`(if)`)).toThrow()
      expect(() => lispish.run(`(if true)`)).toThrow()
      expect(() => lispish.run(`(if true "A")`)).toThrow()
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

  describe(`let`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(let [a "A"] a)`)).toBe(`A`)
      expect(lispish.run(`(let [a "A" b "B"] a b)`)).toBe(`B`)
      expect(lispish.run(`(let [a "A" b "B"] a b)`)).toBe(`B`)
      expect(lispish.run(`(let [a (+ 10 20) b "B"] b a)`)).toBe(30)
      expect(() => lispish.run(`(let)`)).toThrow()
      expect(() => lispish.run(`(let ())`)).toThrow()
      expect(() => lispish.run(`(let [)))`)).toThrow()
      expect(() => lispish.run(`(let [let [b "B"] b])`)).toThrow()
      expect(() => lispish.run(`(let [a "A") b) a`)).toThrow()
      expect(() => lispish.run(`(let (a "A"]`)).toThrow()
      expect(() => lispish.run(`(let (a "A") a)`)).toThrow()
      expect(() => lispish.run(`(let [a (fn [] 1)] a)`)).toThrow()
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
      expect(lispish.run(`(and 1 undefined)`)).toBe(undefined)
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
      expect(lispish.run(`(or null 0 false undefined)`)).toBe(undefined)
      expect(lispish.run(`(or null 0 1 undefined)`)).toBe(1)
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
      expect(lispish.run(`(cond)`)).toBeUndefined()
      expect(lispish.run(`(cond (true 10) (true 20))`)).toBe(10)
      expect(lispish.run(`(cond (true 10))`)).toBe(10)
      expect(lispish.run(`(cond (false 20) (true (+ 5 5)) )`)).toBe(10)
      expect(
        lispish.run(`(cond ((> 5 10) 20) ((> 10 10) (write! "Hej") (+ 5 5)) ((>= 10 10) "This will work" (+ 5 5 5)))`),
      ).toBe(15)
      expect(() => lispish.run(`(cond (true 123) )`)).not.toThrow()
      expect(() => lispish.run(`(cond true 123) )`)).toThrow()
    })
    test(`middle condition true`, () => {
      expect(
        lispish.run(
          `(cond ((> 5 10) (write! 20)) ((>= 10 10) (+ 5 5)) ((write! (>= 10 10)) "This will work" (+ 5 5 5)))`,
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
      expect(() => lispish.run(`(defn undefined [] 10)`)).toThrow()
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
      expect(lispish.run(`(defns (concat "a" "d" "d") [a b] (+ a b)) (add 1 2)`)).toBe(3)
      expect(() => lispish.run(`(defns "add" [] 10)`)).not.toThrow()
      expect(() => lispish.run(`(defns "x" [a a] 10)`)).toThrow()
      expect(() => lispish.run(`(defns true [] 10)`)).toThrow()
      expect(() => lispish.run(`(defns false [] 10)`)).toThrow()
      expect(() => lispish.run(`(defns null [] 10)`)).toThrow()
      expect(() => lispish.run(`(defns undefined [] 10)`)).toThrow()
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
      expect(() => lispish.run(`(fn (undefined) 1)`)).toThrow()
      expect(() => lispish.run(`(fn)`)).toThrow()
      expect(() => lispish.run(`(fn [x])`)).toThrow()
      expect(() => lispish.run(`(fn "k")`)).toThrow()
      expect(() => lispish.run(`(fn k s)`)).toThrow()
      expect(() => lispish.run(`(fn add)`)).toThrow()
    })
  })

  describe(`return-from`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(return-from x (* 2 4))`)).toThrowError(ReturnFromSignal)
      expect(() => lispish.run(`(return-from x)`)).not.toThrowError(ReturnFromSignal)
      expect(() => lispish.run(`(return-from x)`)).toThrow()
      expect(() => lispish.run(`(return-from)`)).not.toThrowError(ReturnFromSignal)
      expect(() => lispish.run(`(return-from)`)).toThrow()
      expect(() => lispish.run(`(return-from "x" 10)`)).not.toThrowError(ReturnFromSignal)
      expect(() => lispish.run(`(return-from "x" 10)`)).toThrow()
      try {
        lispish.run(`(return-from x (* 2 4))`)
        throw `Not expecting this`
      } catch (e) {
        expect(e).toBeInstanceOf(ReturnFromSignal)
        expect((e as ReturnFromSignal).name).toBe(`ReturnFromSignal`)
        expect((e as ReturnFromSignal).value).toBe(8)
        expect((e as ReturnFromSignal).blockName).toBe(`x`)
      }
    })
    test(`in action`, () => {
      const program = `
      (defn x [] (write! "Hej") (return-from x "Kalle") (write! "san"))
      (x)
      `
      expect(lispish.run(program)).toBe(`Kalle`)
    })
    test(`nested block 1`, () => {
      const program = `
        (block x
          (block y
            (return-from y undefined)
          )
          2
        )
      `
      expect(lispish.run(program)).toBe(2)
    })

    test(`nested block 2`, () => {
      const program = `
        (block x
          (block y
            (return-from x undefined)
          )
          2
        )
      `
      expect(lispish.run(program)).toBeUndefined()
    })
    test(`return from fn`, () => {
      const program = `
        ((fn []
          (return "A")
          "B"
        ))
      `
      expect(lispish.run(program)).toBe(`A`)
    })
  })

  describe(`return`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(return (* 2 4))`)).toThrowError(ReturnSignal)
      expect(() => lispish.run(`(return)`)).not.toThrowError(ReturnFromSignal)
      expect(() => lispish.run(`(return)`)).toThrow()
      try {
        lispish.run(`(return (* 2 4))`)
        throw `Not expecting this`
      } catch (e) {
        expect(e).toBeInstanceOf(ReturnSignal)
        expect((e as ReturnSignal).name).toBe(`ReturnSignal`)
        expect((e as ReturnSignal).value).toBe(8)
      }
    })
    test(`in action`, () => {
      const program = `
      (defn x [] (write! "Hej") (return "Kalle") (write! "san"))
      (x)
      `
      expect(lispish.run(program)).toBe(`Kalle`)
    })
  })
  describe(`block`, () => {
    test(`samples`, () => {
      expect(
        lispish.run(`(block x
        (write! "hej")
        (return-from x 10)
        (write! "XXX")
      )`),
      ).toBe(10)
      expect(
        lispish.run(`(block x
        (write! "hej")
        (write! "XXX")
      )`),
      ).toBe(`XXX`)
      expect(() =>
        lispish.run(`(block x
        (write! "hej")
        (return-from "x" 10)
        (write! "XXX")
      )`),
      ).toThrow()
      expect(() =>
        lispish.run(`(block "x"
        (write! "hej")
        (return-from x 10)
        (write! "XXX")
      )`),
      ).toThrow()
      expect(() =>
        lispish.run(`(block x
        (write! hej)
        (return-from x 10)
        (write! "XXX")
      )`),
      ).toThrow()
    })
    test(`asd`, () => {
      expect(() =>
        lispish.run(`(block x
        (write! hej)
        (return-from x 10)
        (write! "XXX")
      )`),
      ).toThrow()
    })
  })

  describe(`try`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(try (/ 2 4) ((error) 1))`)).toBe(0.5)
      expect(lispish.run(`(try (/ 2 0) ((error) 1))`)).toBe(1)
      expect(lispish.run(`(try (/ 2 0) ((error) error))`)).toBeInstanceOf(Error)
      expect(() => lispish.run(`(try (/ 2 4) 1)`)).toThrow()
      expect(() => lispish.run(`(try (/ 2 4) (1))`)).toThrow()
      expect(() => lispish.run(`(try (/ 2 4) (("error") 1))`)).toThrow()
      expect(() => lispish.run(`(try (/ 2 4) ((error1 error2) 1))`)).toThrow()
      expect(() => lispish.run(`(try (/ 2 4) ((error) 1 2))`)).toThrow()
      expect(() => lispish.run(`(try (/ 2 4) ((error) 1 )2)`)).toThrow()
    })
    test(`return-from should not trigger catchBlock`, () => {
      const program = `
        (block b
          (try
            (do
              (write! "One")
              (return-from b "Two")
              (write! "Three")
            )
            ((error) "Four")
          )
        )
      `
      expect(lispish.run(program)).toBe(`Two`)
    })
    test(`return-from should not trigger catchBlock`, () => {
      const program = `
        (defn fun []
          (try
            (do
              (write! "One")
              (return "Two")
              (write! "Three")
            )
            ((error) "Four")
          )
        )
        (fun)
      `
      expect(lispish.run(program)).toBe(`Two`)
    })
  })

  describe(`throw`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(throw "An error")`)).toThrowError(UserDefinedError)
      expect(() => lispish.run(`(throw (substring "An error" 3))`)).toThrowError(UserDefinedError)
      expect(() => lispish.run(`(throw "An error" 10)`)).not.toThrowError(UserDefinedError)
      expect(() => lispish.run(`(throw "An error" 10)`)).toThrow()
      try {
        lispish.run(`(throw (substring "An error" 3))`)
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
      expect(lispish.run(`(when (> 10 20) (write! 10) (write! 20))`)).toBe(undefined)
      expect(lispish.run(`(when false)`)).toBe(undefined)
      expect(lispish.run(`(when true)`)).toBe(undefined)
      expect(() => lispish.run(`(when)`)).toThrow()
    })
  })

  describe(`unless`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(unless true (write! 10) (write! 20))`)).toBeUndefined()
      expect(lispish.run(`(unless (> 10 20) (write! 10) (write! 20))`)).toBe(20)
      expect(lispish.run(`(unless null (write! 10) (write! 20))`)).toBe(20)
      expect(lispish.run(`(unless false)`)).toBe(undefined)
      expect(lispish.run(`(unless true)`)).toBe(undefined)
      expect(() => lispish.run(`(unless)`)).toThrow()
    })
  })

  describe(`do`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(do [1 2 3] "[1]" (+ 1 2))`)).toBe(3)
      expect(lispish.run(`(do (object "a" 1) "a")`)).toBe(`a`)
      expect(lispish.run(`(do)`)).toBeUndefined()
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
})
