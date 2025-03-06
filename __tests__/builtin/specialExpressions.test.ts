/* eslint-disable no-console */
import type { Mock } from 'vitest'
import { afterEach, beforeEach, describe, expect, it, vitest } from 'vitest'
import { Lits } from '../../src'
import { UserDefinedError } from '../../src/errors'
import { getLitsVariants, getUndefinedSymbolNames } from '../testUtils'
import { findUnresolvedIdentifiers } from '../../src/analyze/findUnresolvedIdentifiers'
import { createContextStack } from '../../src/evaluator/ContextStack'
import { builtin } from '../../src/builtin'

const lits = getLitsVariants()

describe('specialExpressions', () => {
  let oldLog: () => void
  let logSpy: Mock<any>
  beforeEach(() => {
    oldLog = console.log
    logSpy = vitest.fn()
    console.log = (...args: unknown[]) => {
      logSpy(...args)
    }
  })
  afterEach(() => {
    console.log = oldLog
  })
  it('error message', () => {
    const litsNoDebug = new Lits({ polish: true })
    let failed = false
    try {
      litsNoDebug.run('(throw (subs "An error" 3))')
      failed = true
    }
    catch (error) {
      expect((error as UserDefinedError).message).toBe('error')
    }
    if (failed)
      throw new Error('Should have thrown an error')

    const litsDebug = new Lits({ debug: true, polish: true })
    try {
      failed = false
      litsDebug.run('(throw (subs "An error" 3))')
      failed = true
    }
    catch (error) {
      expect((error as UserDefinedError).message).toBe(
        'error\n(throw (subs "An error" 3))\n^                          ',
      )
    }
    if (failed)
      throw new Error('Should have thrown an error')
  })

  describe('def', () => {
    it('samples', () => {
      expect(lits.run('(def a 10) a')).toBe(10)
      expect(lits.run('(do (def a 10)) a')).toBe(10)
      expect(() => lits.run('(def a 10) (def a 20) a')).toThrow()
      expect(() => lits.run('(def true false)')).toThrow()
      expect(() => lits.run('(def a)')).toThrow()
      expect(() => lits.run('(def a 10 10)')).toThrow()
      expect(() => lits.run('(def 1 10)')).toThrow()
      expect(() => lits.run('(def nil 10)')).toThrow()
      expect(() => lits.run('(def false 10)')).toThrow()
      expect(() => lits.run('(def true 10)')).toThrow()
      expect(() => lits.run('(def [] 10)')).toThrow()
      expect(() => lits.run('(def (object) 10)')).toThrow()
      expect(() => lits.run('(def :a 10)')).toThrow()
    })

    it('local variable', () => {
      const program = `
      (def x :A)     ;Global variable x
      (write! x)       ;:A
      (do
        (let [x :B])    ;Local variable x
        (write! x)     ;:B
      )
        
      (write! x)       ;:A - global variable x
      `
      lits.run(program)
      // expect(logSpy).toHaveBeenNthCalledWith(1, 'A')
      expect(logSpy).toHaveBeenNthCalledWith(2, 'B')
      expect(logSpy).toHaveBeenNthCalledWith(3, 'A')
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(def foo (+ a b))'))).toEqual(new Set(['a', 'b']))
        expect(getUndefinedSymbolNames(lits.analyze('(def foo (+ a b)) foo'))).toEqual(new Set(['a', 'b']))
      })
    })
  })

  describe('if', () => {
    it('samples', () => {
      expect(lits.run('(if true :A :B)')).toBe('A')
      expect(lits.run('(if false :A :B)')).toBe('B')
      expect(lits.run('(if nil :A :B)')).toBe('B')
      expect(lits.run('(if true :A)')).toBe('A')
      expect(lits.run('(if false :A)')).toBeNull()
      expect(lits.run('(if nil :A)')).toBeNull()
      expect(lits.run('(if "" :A :B)')).toBe('B')
      expect(lits.run('(if :x :A :B)')).toBe('A')
      expect(lits.run('(if 0 :A :B)')).toBe('B')
      expect(lits.run('(if 1 :A :B)')).toBe('A')
      expect(lits.run('(if -1 :A :B)')).toBe('A')
      expect(lits.run('(if [] :A :B)')).toBe('A')
      expect(lits.run('(if (object) :A :B)')).toBe('A')
      expect(() => lits.run('(if)')).toThrow()
      expect(() => lits.run('(if true)')).toThrow()
      expect(() => lits.run('(if true :A :B :Q)')).toThrow()
    })
    it('that special form \'if\' only evaluate the correct path (true)', () => {
      lits.run('(if true (write! :A) (write! :B))')
      expect(logSpy).toHaveBeenCalledWith('A')
      expect(logSpy).not.toHaveBeenCalledWith('B')
    })
    it('that special form \'if\' only evaluate the correct path (false)', () => {
      lits.run('(if false (write! :A) (write! :B))')
      expect(logSpy).not.toHaveBeenCalledWith('A')
      expect(logSpy).toHaveBeenCalledWith('B')
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(if (> a b) a b)'))).toEqual(new Set(['a', 'b']))
        expect(getUndefinedSymbolNames(lits.analyze('(if (> a b) c d)'))).toEqual(new Set(['a', 'b', 'c', 'd']))
      })
    })
  })

  describe('unless', () => {
    it('samples', () => {
      expect(lits.run('(unless true :A :B)')).toBe('B')
      expect(lits.run('(unless false :A :B)')).toBe('A')
      expect(lits.run('(unless nil :A :B)')).toBe('A')
      expect(lits.run('(unless true :A)')).toBeNull()
      expect(lits.run('(unless false :A)')).toBe('A')
      expect(lits.run('(unless nil :A)')).toBe('A')
      expect(lits.run('(unless "" :A :B)')).toBe('A')
      expect(lits.run('(unless :x :A :B)')).toBe('B')
      expect(lits.run('(unless 0 :A :B)')).toBe('A')
      expect(lits.run('(unless 1 :A :B)')).toBe('B')
      expect(lits.run('(unless -1 :A :B)')).toBe('B')
      expect(lits.run('(unless [] :A :B)')).toBe('B')
      expect(lits.run('(unless (object) :A :B)')).toBe('B')
      expect(() => lits.run('(unless)')).toThrow()
      expect(() => lits.run('(unless true)')).toThrow()
      expect(() => lits.run('(unless true :A :B :Q)')).toThrow()
    })
    it('that special form \'unless\' only evaluate the correct path (true)', () => {
      lits.run('(unless true (write! :A) (write! :B))')
      expect(logSpy).toHaveBeenCalledWith('B')
      expect(logSpy).not.toHaveBeenCalledWith('A')
    })
    it('that special form \'unless\' only evaluate the correct path (false)', () => {
      lits.run('(unless false (write! :A) (write! :B))')
      expect(logSpy).not.toHaveBeenCalledWith('B')
      expect(logSpy).toHaveBeenCalledWith('A')
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(unless (> a b) a b)'))).toEqual(new Set(['a', 'b']))
        expect(getUndefinedSymbolNames(lits.analyze('(unless (> a b) c d)'))).toEqual(new Set(['a', 'b', 'c', 'd']))
      })
    })
  })

  describe('let', () => {
    it('samples', () => {
      expect(lits.run('(let [a :A]) a')).toBe('A')
      expect(lits.run('(let [a :A]) (do (let [a :B])) a')).toBe('A')
      expect(lits.run('(let [a :A b :B]) a b')).toBe('B')
      expect(lits.run('(let [a :A b :B]) a b')).toBe('B')
      expect(lits.run('(let [a (+ 10 20) b :B]) b a')).toBe(30)
      expect(lits.run('(let [a (fn [] 1)]) (a)')).toBe(1)
      expect(() => lits.run('(do (let [a 10]) (+ a 2)) a')).toThrow()
      expect(() => lits.run('(let)')).toThrow()
      expect(() => lits.run('(let ())')).toThrow()
      expect(() => lits.run('(let [)))')).toThrow()
      expect(() => lits.run('(let [let [b :B] b])')).toThrow()
      expect(() => lits.run('(let [a :A) b) a')).toThrow()
      expect(() => lits.run('(let (a :A]')).toThrow()
      expect(() => lits.run('(let (a :A) a)')).toThrow()
    })
    it('variables depend on each other', () => {
      const program = `
      (let
        [
          year 2000
          month 1
          day 1
          leapYear
            (&&
              (zero? (mod year 4))
              (||
                (! (zero? (mod year 100)))
                (zero? (mod year 400))))])
      leapYear
      `
      expect(lits.run(program)).toBe(true)
    })
    it('local and global variables', () => {
      expect(() =>
        lits.run(`
          (let [
            (a :A)
            (b a)     ;Cannot access local variable a here. This is what let* would be for
          ])
          b
        `),
      ).toThrow()
      expect(
        lits.run(`
          (def a :X)
          (let [
            b a     ;a is the global variable
          ])
          b
        `),
      ).toBe('X')
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(let [a (> (count name) 4)]) a b'))).toEqual(
          new Set(['name', 'b']),
        )
        expect(getUndefinedSymbolNames(lits.analyze('(let [data 1, data2 (+ data 1)]) data2'))).toEqual(new Set([]))
      })
    })
  })

  describe('&&', () => {
    it('samples', () => {
      expect(lits.run('(&&)')).toBe(true)
      expect(lits.run('(&& 0)')).toBe(0)
      expect(lits.run('(&& 0 1)')).toBe(0)
      expect(lits.run('(&& 2 0)')).toBe(0)
      expect(lits.run('(&& 2 0 1)')).toBe(0)
      expect(lits.run('(&& 2 3 0)')).toBe(0)
      expect(lits.run('(&& 2 3 "")')).toBe('')
      expect(lits.run('(&& 2 3 :x)')).toBe('x')
      expect(lits.run('(&& false 1)')).toBe(false)
      expect(lits.run('(&& 1 false)')).toBe(false)
      expect(lits.run('(&& 1 nil)')).toBe(null)
      expect(lits.run('(&& 2 2 false)')).toBe(false)
      expect(lits.run('(&& 3 true 3)')).toBe(3)
    })
    describe('short circuit', () => {
      it('true, false', () => {
        expect(lits.run('(&& (write! true) (write! false))')).toBe(false)
        expect(logSpy).toHaveBeenNthCalledWith(1, true)
        expect(logSpy).toHaveBeenNthCalledWith(2, false)
      })
      it('true, 1', () => {
        expect(lits.run('(&& (write! true) (write! 1))')).toBe(1)
        expect(logSpy).toHaveBeenNthCalledWith(1, true)
        expect(logSpy).toHaveBeenNthCalledWith(2, 1)
      })
      it('false, true', () => {
        expect(lits.run('(&& (write! false) (write! true))')).toBe(false)
        expect(logSpy).toHaveBeenCalledWith(false)
        expect(logSpy).not.toHaveBeenCalledWith(true)
      })
      it('false, 0', () => {
        expect(lits.run('(&& (write! false) (write! 0))')).toBe(false)
        expect(logSpy).toHaveBeenCalledWith(false)
        expect(logSpy).not.toHaveBeenCalledWith(0)
      })
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(&& false b)'))).toEqual(new Set(['b']))
      })
    })
  })

  describe('||', () => {
    it('samples', () => {
      expect(lits.run('(||)')).toBe(false)
      expect(lits.run('(|| 0)')).toBe(0)
      expect(lits.run('(|| 0 1)')).toBe(1)
      expect(lits.run('(|| 2 0)')).toBe(2)
      expect(lits.run('(|| nil 0 false)')).toBe(false)
      expect(lits.run('(|| nil 0 1)')).toBe(1)
    })
    describe('short circuit', () => {
      it('true, false', () => {
        expect(lits.run('(|| (write! true) (write! false))')).toBe(true)
        expect(logSpy).toHaveBeenCalledWith(true)
        expect(logSpy).not.toHaveBeenCalledWith(false)
      })
      it('true, 1', () => {
        expect(lits.run('(|| (write! true) (write! 1))')).toBe(true)
        expect(logSpy).toHaveBeenCalledWith(true)
        expect(logSpy).not.toHaveBeenCalledWith(1)
      })
      it('false, true', () => {
        expect(lits.run('(|| (write! false) (write! true))')).toBe(true)
        expect(logSpy).toHaveBeenNthCalledWith(1, false)
        expect(logSpy).toHaveBeenNthCalledWith(2, true)
      })
      it('false, 0', () => {
        expect(lits.run('(|| (write! false) (write! 0))')).toBe(0)
        expect(logSpy).toHaveBeenNthCalledWith(1, false)
        expect(logSpy).toHaveBeenNthCalledWith(2, 0)
      })
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(|| true b (+ c d))'))).toEqual(new Set(['b', 'c', 'd']))
      })
    })
  })

  describe('cond', () => {
    it('samples', () => {
      expect(lits.run('(cond)')).toBeNull()
      expect(lits.run('(cond true 10 true 20)')).toBe(10)
      expect(lits.run('(cond true 10)')).toBe(10)
      expect(lits.run('(cond false 20 true (+ 5 5))')).toBe(10)
      expect(
        lits.run('(cond (> 5 10) 20 (> 10 10) (do (write! "Hej") (+ 5 5)) (>= 10 10) (do "This will work" (+ 5 5 5)))'),
      ).toBe(15)
      expect(() => lits.run('(cond true 123)')).not.toThrow()
      expect(() => lits.run('(cond (true 123))')).toThrow()
    })
    it('middle condition true', () => {
      expect(
        lits.run('(cond (> 5 10) (write! 20) (>= 10 10) (+ 5 5) (write! (>= 10 10)) (do "This will work" (+ 5 5 5)))'),
      ).toBe(10)
      expect(logSpy).not.toHaveBeenCalled()
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(cond true a false b (> a 1) c :else d)'))).toEqual(
          new Set(['a', 'b', 'c', 'd']),
        )
      })
    })
  })

  describe('switch', () => {
    it('samples', () => {
      expect(lits.run('(switch 1)')).toBeNull()
      expect(lits.run('(def x "-") (switch x "-" (+ 5 5) 2 20)')).toBe(10)
      expect(lits.run('(switch true true 10)')).toBe(10)
      expect(lits.run('(switch true false 20 true (+ 5 5))')).toBe(10)
      expect(
        lits.run('(switch 2, 0 20, 1 (do (write! "Hej") (+ 5 5)), 2 (do "This will work" (+ 5 5 5)))'),
      ).toBe(15)
      expect(() => lits.run('(switch)')).toThrow()
      expect(() => lits.run('(switch true 123)')).toThrow()
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(switch foo true a false b (> a 1) c :else d)'))).toEqual(
          new Set(['foo', 'a', 'b', 'c', 'd']),
        )
      })
    })
  })

  describe('defn', () => {
    it('samples', () => {
      expect(lits.run('(defn add [a b] (+ a b)) (add 1 2)')).toBe(3)
      expect(lits.run('(defn add [a b &let [x 10]] (+ a b x)) (add 1 2)')).toBe(13)
      expect(() => lits.run('(defn add [a b &rest &let [x 10]] (+ a b x)) (add 1 2)')).toThrow()
      expect(() => lits.run('(defn add [] 10)')).not.toThrow()
      expect(() => lits.run('(defn true [] 10)')).toThrow()
      expect(() => lits.run('(defn false [] 10)')).toThrow()
      expect(() => lits.run('(defn nil [] 10)')).toThrow()
      expect(() => lits.run('(defn add [:s] 10)')).toThrow()
      expect(() => lits.run('(defn "add" [a b] (+ a b))')).toThrow()
      expect(() => lits.run('(defn add 1 (+ a b))')).toThrow()
      expect(() => lits.run('(defn add [a b])')).toThrow()
      expect(() => lits.run('(defn add a b)')).toThrow()
    })
    it('arity', () => {
      expect(lits.run('(defn add ([a b] (+ a b))) (add 1 2)')).toBe(3)
      expect(lits.run('(defn add ([a b] (+ a b)) ([a] 10)) (+ (add 1 2) (add 1))')).toBe(13)
      expect(() => lits.run('(defn add ([a b] (-- a b)) ([a] 10)) (+ (add 1 2) (add 1))')).toThrow()
      expect(() => lits.run('(defn add ([a b] (+ a b)) ([a] 10)] (+ (add 1 2) (add 1))')).toThrow()
      expect(() => lits.run('(defn add ([a b] (+ a b)] ([a] 10)) (+ (add 1 2) (add 1))')).toThrow()
      expect(() => lits.run('(defn add ([a b] (+ a b)) ([a b] 10))')).toThrow()
      expect(() => lits.run('(defn add ([a b] (+ a b)) ([a b &rest rest] 10))')).toThrow()
      expect(() => lits.run('(defn add ([a b &rest rest] (+ a b)) ([a b] 10))')).toThrow()
      expect(() => lits.run('(defn add ([a b &rest rest] (+ a b)) ([a b c &rest rest] 10))')).toThrow()
    })
    it('call defn function', () => {
      expect(lits.run('(defn \'sumOneToN\' [n] (if (<= n 1) n (+ n (\'sumOneToN\' (- n 1))))) (sumOneToN 10)')).toBe(55)
      expect(lits.run('(defn applyWithVal [fun val] (fun val)) (applyWithVal inc 10)')).toBe(11)
      expect(lits.run('(defn applyWithVal [fun val] (fun val)) (applyWithVal inc 10)')).toBe(11)
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(defn foo [a] (if (== a 1) 1 (+ a (foo (dec a)))))'))).toEqual(
          new Set(),
        )
        expect(
          getUndefinedSymbolNames(lits.analyze('(defn foo [a &let [x y, y z]] (if (== a 1) 1 (+ a (foo (dec a)))))')),
        ).toEqual(new Set(['y', 'z']))
        expect(getUndefinedSymbolNames(lits.analyze('(defn foo [a b] (str a b c))'))).toEqual(new Set(['c']))
        expect(getUndefinedSymbolNames(lits.analyze('(defn foo [a b] (str a b c)) (foo x y)'))).toEqual(
          new Set(['c', 'x', 'y']),
        )
        expect(getUndefinedSymbolNames(lits.analyze('(defn add ([a b &rest rest] (+ a b)) ([a] 10))'))).toEqual(new Set())
      })
    })
  })

  describe('fn', () => {
    it('samples', () => {
      lits.run('(fn [x] (+ x 1))')
      lits.run('(fn [] 1)')
      expect(() => lits.run('((fn [x] (+ y 1)) 10)')).toThrow()
      expect(() => lits.run('(fn (false) 1)')).toThrow()
      expect(() => lits.run('(fn (true) 1)')).toThrow()
      expect(() => lits.run('(fn (nil) 1)')).toThrow()
      expect(() => lits.run('(fn)')).toThrow()
      expect(() => lits.run('(fn [x])')).toThrow()
      expect(() => lits.run('(fn :k)')).toThrow()
      expect(() => lits.run('(fn k s)')).toThrow()
      expect(() => lits.run('(fn add)')).toThrow()
    })

    it('shorthand lambda', () => {
      expect(lits.run('(#(+ %1 %2 %3) 2 4 6)')).toBe(12)
      expect(lits.run('(#(if %1 %2 %3) 2 4 6)')).toBe(4)
      expect(lits.run('(#(if %1 %2 %3) 0 4 6)')).toBe(6)
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(fn [a b] (str a b c))'))).toEqual(new Set(['c']))
        expect(getUndefinedSymbolNames(lits.analyze('(def foo (fn [a b] (str a b c))) (foo 1 x)'))).toEqual(
          new Set(['c', 'x']),
        )
        expect(getUndefinedSymbolNames(lits.analyze('(fn ([a b &rest rest] (+ a b)) ([a] 10))'))).toEqual(new Set())
        expect(getUndefinedSymbolNames(lits.analyze('(#(if %1 %2 %3) 0 4 6)'))).toEqual(new Set())
      })
    })
  })

  describe('try', () => {
    it('samples', () => {
      expect(lits.run('(try (/ 2 4) (catch error 1))')).toBe(0.5)
      expect(lits.run('(try (throw "oops") (catch error 1))')).toBe(1)
      expect(lits.run('(try (throw "oops") (catch error error))')).toBeInstanceOf(Error)
      expect(() => lits.run('(try (/ 2 4) 1)')).toThrow()
      expect(() => lits.run('(try (/ 2 4) (1))')).toThrow()
      expect(() => lits.run('(try (/ 2 4) (catch "error" 1))')).toThrow()
      expect(() => lits.run('(try (/ 2 4) (ratch error 1))')).toThrow()
      expect(() => lits.run('(try (/ 2 4) (catch error1 error2 1))')).toThrow()
      expect(() => lits.run('(try (/ 2 4) (catch error 1 2))')).toThrow()
      expect(() => lits.run('(try (/ 2 4) (catch error 1 )2)')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(try (/ a b) (catch error (str error x)))'))).toEqual(
          new Set(['a', 'b', 'x']),
        )
      })
    })
  })

  describe('throw', () => {
    it('samples', () => {
      expect(() => lits.run('(throw "An error")')).toThrowError(UserDefinedError)
      expect(() => lits.run('(throw (subs "An error" 3))')).toThrowError(UserDefinedError)
      expect(() => lits.run('(throw "An error" 10)')).not.toThrowError(UserDefinedError)
      expect(() => lits.run('(throw "An error" 10)')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(throw (/ a b))'))).toEqual(new Set(['a', 'b']))
      })
    })
  })

  describe('comment', () => {
    it('samples', () => {
      expect(lits.run('(comment [1 2 3] "[1]" (+ 1 2))')).toBeNull()
      expect(lits.run('(comment (object :a 1) :a)')).toBeNull()
      expect(lits.run('(comment)')).toBeNull()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(comment [x "Albert"] x)'))).toEqual(new Set())
        expect(getUndefinedSymbolNames(lits.analyze('(comment (+ a b c))'))).toEqual(new Set())
      })
    })
  })

  describe('do', () => {
    it('samples', () => {
      expect(lits.run('(do [1 2 3] "[1]" (+ 1 2))')).toBe(3)
      expect(lits.run('(do (object :a 1) :a)')).toBe('a')
      expect(lits.run('(do)')).toBeNull()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(do [a 2 3] "[1]" (+ 1 b))'))).toEqual(new Set(['a', 'b']))
      })
    })
  })

  describe('recur', () => {
    it('should work with defn', () => {
      lits.run('(defn foo [n] (write! n) (if (! (zero? n)) (recur (dec n)))) (foo 3)')
      expect(logSpy).toHaveBeenNthCalledWith(1, 3)
      expect(logSpy).toHaveBeenNthCalledWith(2, 2)
      expect(logSpy).toHaveBeenNthCalledWith(3, 1)
      expect(logSpy).toHaveBeenNthCalledWith(4, 0)
    })
    it('recur must be called with the right number of parameters', () => {
      expect(() => lits.run('(defn foo [n] (write! n) (if (! (zero? n)) (recur))) (foo 3)')).toThrow()
      expect(() => lits.run('(defn foo [n] (write! n) (if (! (zero? n)) (recur (dec n)))) (foo 3)')).not.toThrow()
      expect(() => lits.run('(defn foo [n] (write! n) (if (! (zero? n)) (recur (dec n) 1))) (foo 3)')).toThrow()
      expect(() => lits.run('(defn foo [n] (write! n) (if (! (zero? n)) (recur (dec n) 1 2))) (foo 3)')).toThrow()
      expect(() => lits.run('((fn [n] (write! n) (if (! (zero? n)) (recur))) 3)')).toThrow()
      expect(() => lits.run('((fn [n] (write! n) (if (! (zero? n)) (recur (dec n)))) 3)')).not.toThrow()
      expect(() => lits.run('((fn [n] (write! n) (if (! (zero? n)) (recur (dec n) 1))) 3)')).toThrow()
      expect(() => lits.run('((fn [n] (write! n) (if (! (zero? n)) (recur (dec n) 1 2))) 3)')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        const lits2 = new Lits({ polish: true })

        expect(
          findUnresolvedIdentifiers(lits2.parse(lits2.tokenize('((fn [n] (write! n) (if (! (zero? n)) (recur (dec n)))) 3)')).b, createContextStack(), builtin),
        ).toEqual(new Set())
        expect(
          findUnresolvedIdentifiers(lits2.parse(lits2.tokenize('((fn [n] (write! n) (if (! (zero? n)) (recur (- n a)))) 3)')).b, createContextStack(), builtin),
        ).toEqual(new Set([{ symbol: 'a' }]))
      })
    })
  })

  describe('loop', () => {
    it('should work with recur', () => {
      lits.run('(loop [n 3] (write! n) (if (! (zero? n)) (recur (dec n))))')
      expect(logSpy).toHaveBeenNthCalledWith(1, 3)
      expect(logSpy).toHaveBeenNthCalledWith(2, 2)
      expect(logSpy).toHaveBeenNthCalledWith(3, 1)
      expect(logSpy).toHaveBeenNthCalledWith(4, 0)
    })
    it('recur must be called with right number of parameters', () => {
      expect(() => lits.run('(loop [n 3] (write! n) (if (! (zero? n)) (recur (dec n) 2)))')).toThrow()
      expect(() => lits.run('(loop [n 3] (write! n) (if (! (zero? n)) (recur)))')).toThrow()
    })
    it('throw should work', () => {
      expect(() => lits.run('(loop [n 3] (write! n) (if (! (zero? n)) (throw (dec n))))')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(
          getUndefinedSymbolNames(lits.analyze('(loop [n 3] (write! n) (if (! (zero? n)) (recur (dec n))))')),
        ).toEqual(new Set())
        expect(
          getUndefinedSymbolNames(lits.analyze('(loop [n 3] (write! x n) (if (! (zero? n)) (recur (dec n))))')),
        ).toEqual(new Set(['x']))
        expect(
          getUndefinedSymbolNames(
            lits.analyze('(loop [n (+ 3 y)] (write! x n) (if (! (zero? n)) (recur (dec n))))'),
          ),
        ).toEqual(new Set(['x', 'y']))
      })
    })
  })

  describe('doseq', () => {
    it('samples', () => {
      expect(lits.run('(doseq [x []] x)')).toBeNull()
      expect(lits.run('(doseq [x [1 2 3] y []] x)')).toBeNull()
      expect(lits.run('(doseq [x [] y [1 2 3]] x)')).toBeNull()

      expect(lits.run('(doseq [x "Al" y [1 2]] (repeat x y))')).toBeNull()
      expect(lits.run('(doseq [x {:a 10 :b 20} y [1 2]] (repeat x y))')).toBeNull()

      expect(lits.run('(doseq [x [1 2] y [1 10]] (* x y))')).toBeNull()
      expect(lits.run('(doseq [x [1 2] &let [z (* x x x)]] z)')).toBeNull()
      expect(lits.run('(doseq [x [1 2] y [x (* 2 x)]] (* x y))')).toBeNull()

      expect(lits.run('(doseq [x [0 1 2 3 4 5] &let [y (* x 3)] &when (even? y)] y)')).toBeNull()
      expect(lits.run('(doseq [x [0 1 2 3 4 5] &let [y (* x 3)] &while (even? y)] y)')).toBeNull()

      expect(lits.run('(doseq [x [1 2 3] y [1 2 3] &while (<= x y) z [1 2 3]] [x y z])')).toBeNull()
      expect(lits.run('(doseq [x [1 2 3] y [1 2 3] z [1 2 3] &while (<= x y)] [x y z])')).toBeNull()
      expect(() => lits.run('(doseq [x [0 1 2 3 4 5] &rest [y (* x 3)] &while (even? y)] y)')).toThrow()
      expect(() => lits.run('(doseq [x [0 1 2 3 4 5] &let [x 10]] y)')).toThrow()
      expect(() => lits.run('(doseq x [0 1 2 3 4 5] y)')).toThrow()
      expect(() => lits.run('(doseq [x [0 1 2 3 4 5]] x y)')).toThrow()
      expect(() => lits.run('(doseq [x [0 1 2 3 4 5] x [10 20]] x)')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(
          getUndefinedSymbolNames(lits.analyze('(doseq [x [0 1 2 3 4 5] &let [y (* x 3)] &when (even? y)] y)')),
        ).toEqual(new Set())
        expect(
          getUndefinedSymbolNames(lits.analyze('(doseq [x [0 1 2 3 4 5] &let [y (* x 3)] &while (even? y)] y)')),
        ).toEqual(new Set())
        expect(
          getUndefinedSymbolNames(lits.analyze('(doseq [x [0 1 2 3 4 a] &let [y (* x b)] &while (even? c)] d)')),
        ).toEqual(new Set(['a', 'b', 'c', 'd']))
      })
    })
  })

  describe('for', () => {
    it('samples', () => {
      expect(lits.run('(for [x []] x)')).toEqual([])
      expect(lits.run('(for [x [1 2 3] y []] x)')).toEqual([])
      expect(lits.run('(for [x [] y [1 2 3]] x)')).toEqual([])

      expect(lits.run('(for [x "Al" y [1 2]] (repeat x y))')).toEqual([['A'], ['A', 'A'], ['l'], ['l', 'l']])
      expect(lits.run('(for [x {:a 10 :b 20} y [1 2]] (repeat x y))')).toEqual([
        [['a', 10]],
        [
          ['a', 10],
          ['a', 10],
        ],
        [['b', 20]],
        [
          ['b', 20],
          ['b', 20],
        ],
      ])

      expect(lits.run('(for [x [1 2] y [1 10]] (* x y))')).toEqual([1, 10, 2, 20])
      expect(lits.run('(for [x [1 2] &let [z (* x x x)]] z)')).toEqual([1, 8])
      expect(lits.run('(for [x [1 2] y [x (* 2 x)]] (* x y))')).toEqual([1, 2, 4, 8])

      expect(lits.run('(for [x [0 1 2 3 4 5] &let [y (* x 3)] &when (even? y)] y)')).toEqual([0, 6, 12])
      expect(lits.run('(for [x [0 1 2 3 4 5] &let [y (* x 3)] &while (even? y)] y)')).toEqual([0])

      expect(lits.run('(for [x [1 2 3] y [1 2 3] &while (<= x y) z [1 2 3]] [x y z])')).toEqual([
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
      expect(lits.run('(for [x [1 2 3] y [1 2 3] z [1 2 3] &while (<= x y)] [x y z])')).toEqual([
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
      expect(() => lits.run('(for [x [0 1 2 3 4 5] &rest [y (* x 3)] &while (even? y)] y)')).toThrow()
      expect(() => lits.run('(for [x [0 1 2 3 4 5] &let [x 10]] y)')).toThrow()
      expect(() => lits.run('(for x [0 1 2 3 4 5] y)')).toThrow()
      expect(() => lits.run('(for [x [0 1 2 3 4 5]] x y)')).toThrow()
      expect(() => lits.run('(for [x [0 1 2 3 4 5] x [10 20]] x)')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(
          getUndefinedSymbolNames(lits.analyze('(for [x [0 1 2 3 4 5] &let [y (* x 3)] &when (even? y)] y)')),
        ).toEqual(new Set())
        expect(
          getUndefinedSymbolNames(lits.analyze('(for [x [0 1 2 3 4 5] &let [y (* x 3)] &while (even? y)] y)')),
        ).toEqual(new Set())
        expect(
          getUndefinedSymbolNames(lits.analyze('(for [x [0 1 2 3 4 a] &let [y (* x b)] &when (even? c)] d)')),
        ).toEqual(new Set(['a', 'b', 'c', 'd']))
      })
    })
  })

  describe('defined?', () => {
    it('samples', () => {
      expect(lits.run('(defined? foo)')).toBe(false)
      expect(lits.run('(def foo :foo) (defined? foo)')).toBe(true)
      expect(lits.run('(defined? +)')).toBe(true)
      expect(lits.run('(def foo nil) (defined? foo)')).toBe(true)
      expect(lits.run('(defined? if)')).toBe(true)

      expect(() => lits.run('(defined?)')).toThrow()
      expect(() => lits.run('(defined? foo bar)')).toThrow()
    })
  })

  describe('unresolvedIdentifiers', () => {
    it('samples', () => {
      expect(getUndefinedSymbolNames(lits.analyze('(defined? x)'))).toEqual(new Set(['x']))
    })
  })

  describe('??', () => {
    it('samples', () => {
      expect(lits.run('(?? foo)')).toBe(null)
      expect(lits.run('(?? foo 0)')).toBe(0)
      expect(lits.run('(?? foo 0)')).toBe(0)
      expect(lits.run('(?? 0 1)')).toBe(0)
      expect(lits.run('(?? "")')).toBe('')
      expect(lits.run('(?? nil)')).toBe(null)
      expect(lits.run('(?? nil 0)')).toBe(0)
      expect(lits.run('(?? false)')).toBe(false)
      expect(lits.run('(def foo :foo) (?? foo)')).toBe('foo')

      expect(() => lits.run('(??)')).toThrow()
      expect(() => lits.run('(?? foo bar)')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(?? x)'))).toEqual(new Set(['x']))
        expect(getUndefinedSymbolNames(lits.analyze('(?? x y)'))).toEqual(new Set(['x', 'y']))
      })
    })
  })
})
