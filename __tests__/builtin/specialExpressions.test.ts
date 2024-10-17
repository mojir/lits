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
  let logSpy: Mock<any, any>
  let lastLog: unknown
  beforeEach(() => {
    oldLog = console.log
    logSpy = vitest.fn()
    console.log = (...args: unknown[]) => {
      logSpy(...args)
      lastLog = args[0]
    }
  })
  afterEach(() => {
    console.log = oldLog
  })
  it('error message', () => {
    const litsNoDebug = new Lits()
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

    const litsDebug = new Lits({ debug: true })
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

  describe('defs', () => {
    it('samples', () => {
      expect(lits.run('(defs :a 10) a')).toBe(10)
      expect(lits.run('(defs :a :b) (defs a :c) b')).toBe('c')
      expect(lits.run('(defs (str :a :1) 20) a1')).toBe(20)
      expect(() => lits.run('(defs true false)')).toThrow()
      expect(() => lits.run('(defs a)')).toThrow()
      expect(() => lits.run('(defs a 10 10)')).toThrow()
      expect(() => lits.run('(defs 1 10)')).toThrow()
      expect(() => lits.run('(defs nil 10)')).toThrow()
      expect(() => lits.run('(defs false 10)')).toThrow()
      expect(() => lits.run('(defs true 10)')).toThrow()
      expect(() => lits.run('(defs [] 10)')).toThrow()
      expect(() => lits.run('(defs (object) 10)')).toThrow()
      expect(() => lits.run('(defs a 10)')).toThrow()
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(defs "foo" (+ a b))'))).toEqual(new Set(['a', 'b']))
        expect(getUndefinedSymbolNames(lits.analyze('(defs "foo" (+ a b)) foo'))).toEqual(new Set(['a', 'b']))
      })
    })
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
        (let [x :B]    ;Local variable x
          (write! x)     ;:B
        )
        (write! x)       ;:A - global variable x
      `
      lits.run(program)
      expect(logSpy).toHaveBeenNthCalledWith(1, 'A')
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

  describe('if-not', () => {
    it('samples', () => {
      expect(lits.run('(if-not true :A :B)')).toBe('B')
      expect(lits.run('(if-not false :A :B)')).toBe('A')
      expect(lits.run('(if-not nil :A :B)')).toBe('A')
      expect(lits.run('(if-not true :A)')).toBeNull()
      expect(lits.run('(if-not false :A)')).toBe('A')
      expect(lits.run('(if-not nil :A)')).toBe('A')
      expect(lits.run('(if-not "" :A :B)')).toBe('A')
      expect(lits.run('(if-not :x :A :B)')).toBe('B')
      expect(lits.run('(if-not 0 :A :B)')).toBe('A')
      expect(lits.run('(if-not 1 :A :B)')).toBe('B')
      expect(lits.run('(if-not -1 :A :B)')).toBe('B')
      expect(lits.run('(if-not [] :A :B)')).toBe('B')
      expect(lits.run('(if-not (object) :A :B)')).toBe('B')
      expect(() => lits.run('(if-not)')).toThrow()
      expect(() => lits.run('(if-not true)')).toThrow()
      expect(() => lits.run('(if-not true :A :B :Q)')).toThrow()
    })
    it('that special form \'if-not\' only evaluate the correct path (true)', () => {
      lits.run('(if-not true (write! :A) (write! :B))')
      expect(logSpy).toHaveBeenCalledWith('B')
      expect(logSpy).not.toHaveBeenCalledWith('A')
    })
    it('that special form \'if-not\' only evaluate the correct path (false)', () => {
      lits.run('(if-not false (write! :A) (write! :B))')
      expect(logSpy).not.toHaveBeenCalledWith('B')
      expect(logSpy).toHaveBeenCalledWith('A')
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(if-not (> a b) a b)'))).toEqual(new Set(['a', 'b']))
        expect(getUndefinedSymbolNames(lits.analyze('(if-not (> a b) c d)'))).toEqual(new Set(['a', 'b', 'c', 'd']))
      })
    })
  })

  describe('if-let', () => {
    it('samples', () => {
      expect(lits.run('(if-let [a (> (count "Albert") 4)] a)')).toBe(true)
      expect(lits.run('(if-let [a (> (count "Albert") 10)] a)')).toBeNull()
      expect(lits.run('(if-let [a (> (count "Albert") 4)] "YES" "NO")')).toBe('YES')
      expect(lits.run('(if-let [a (> (count "Albert") 10)] "YES" "NO")')).toBe('NO')
      expect(() => lits.run('(if-let [a (> (count "Albert") 10)] "YES" a)')).toThrow()
      expect(() => lits.run('(if-let [a (> (count "Albert") 10)])')).toThrow()
      expect(() => lits.run('(if-let [a (> (count "Albert") 10) b 20] 1 2)')).toThrow()
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(if-let [a (> (count name) 4)] a b)'))).toEqual(
          new Set(['name', 'b']),
        )
        expect(getUndefinedSymbolNames(lits.analyze('(if-let [name (str name :_)] name)'))).toEqual(new Set(['name']))
      })
    })
  })

  describe('when-let', () => {
    it('samples', () => {
      expect(lits.run('(when-let [a (> (count "Albert") 4)] a)')).toBe(true)
      expect(lits.run('(when-let [a (> (count "Albert") 10)] a)')).toBeNull()
      expect(lits.run('(when-let [a (> (count "Albert") 10)])')).toBeNull()
      expect(lits.run('(when-let [a (> (count "Albert") 10)] 10 20)')).toBeNull()
      expect(lits.run('(when-let [a (> (count "Albert") 4)] 10 20)')).toBe(20)
      expect(() => lits.run('(when-let [a (> (count "Albert") 10) b 20] 1)')).toThrow()
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(when-let [a (> (count name) 4)] a b c)'))).toEqual(
          new Set(['name', 'b', 'c']),
        )
        expect(getUndefinedSymbolNames(lits.analyze('(when-let [name (str name :_)] name)'))).toEqual(new Set(['name']))
      })
    })
  })

  describe('let', () => {
    it('samples', () => {
      expect(lits.run('(let [a :A] a)')).toBe('A')
      expect(lits.run('(let [a :A b :B] a b)')).toBe('B')
      expect(lits.run('(let [a :A b :B] a b)')).toBe('B')
      expect(lits.run('(let [a (+ 10 20) b :B] b a)')).toBe(30)
      expect(lits.run('(let [a (fn [] 1)] (a))')).toBe(1)
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
    it('local and global variables', () => {
      expect(() =>
        lits.run(`
          (let (
            (a :A)
            (b a)     ;Cannot access local variable a here. This is what let* would be for
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
      ).toBe('A')
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(let [a (> (count name) 4)] a b)'))).toEqual(
          new Set(['name', 'b']),
        )
        expect(getUndefinedSymbolNames(lits.analyze('(let [data 1, data2 (+ data 1)] data2)'))).toEqual(new Set([]))
      })
    })
  })

  describe('and', () => {
    it('samples', () => {
      expect(lits.run('(and)')).toBe(true)
      expect(lits.run('(and 0)')).toBe(0)
      expect(lits.run('(and 0 1)')).toBe(0)
      expect(lits.run('(and 2 0)')).toBe(0)
      expect(lits.run('(and 2 0 1)')).toBe(0)
      expect(lits.run('(and 2 3 0)')).toBe(0)
      expect(lits.run('(and 2 3 "")')).toBe('')
      expect(lits.run('(and 2 3 :x)')).toBe('x')
      expect(lits.run('(and false 1)')).toBe(false)
      expect(lits.run('(and 1 false)')).toBe(false)
      expect(lits.run('(and 1 nil)')).toBe(null)
      expect(lits.run('(and 2 2 false)')).toBe(false)
      expect(lits.run('(and 3 true 3)')).toBe(3)
    })
    describe('short circuit', () => {
      it('true, false', () => {
        expect(lits.run('(and (write! true) (write! false))')).toBe(false)
        expect(logSpy).toHaveBeenNthCalledWith(1, true)
        expect(logSpy).toHaveBeenNthCalledWith(2, false)
      })
      it('true, 1', () => {
        expect(lits.run('(and (write! true) (write! 1))')).toBe(1)
        expect(logSpy).toHaveBeenNthCalledWith(1, true)
        expect(logSpy).toHaveBeenNthCalledWith(2, 1)
      })
      it('false, true', () => {
        expect(lits.run('(and (write! false) (write! true))')).toBe(false)
        expect(logSpy).toHaveBeenCalledWith(false)
        expect(logSpy).not.toHaveBeenCalledWith(true)
      })
      it('false, 0', () => {
        expect(lits.run('(and (write! false) (write! 0))')).toBe(false)
        expect(logSpy).toHaveBeenCalledWith(false)
        expect(logSpy).not.toHaveBeenCalledWith(0)
      })
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(and false b)'))).toEqual(new Set(['b']))
      })
    })
  })

  describe('or', () => {
    it('samples', () => {
      expect(lits.run('(or)')).toBe(false)
      expect(lits.run('(or 0)')).toBe(0)
      expect(lits.run('(or 0 1)')).toBe(1)
      expect(lits.run('(or 2 0)')).toBe(2)
      expect(lits.run('(or nil 0 false)')).toBe(false)
      expect(lits.run('(or nil 0 1)')).toBe(1)
    })
    describe('short circuit', () => {
      it('true, false', () => {
        expect(lits.run('(or (write! true) (write! false))')).toBe(true)
        expect(logSpy).toHaveBeenCalledWith(true)
        expect(logSpy).not.toHaveBeenCalledWith(false)
      })
      it('true, 1', () => {
        expect(lits.run('(or (write! true) (write! 1))')).toBe(true)
        expect(logSpy).toHaveBeenCalledWith(true)
        expect(logSpy).not.toHaveBeenCalledWith(1)
      })
      it('false, true', () => {
        expect(lits.run('(or (write! false) (write! true))')).toBe(true)
        expect(logSpy).toHaveBeenNthCalledWith(1, false)
        expect(logSpy).toHaveBeenNthCalledWith(2, true)
      })
      it('false, 0', () => {
        expect(lits.run('(or (write! false) (write! 0))')).toBe(0)
        expect(logSpy).toHaveBeenNthCalledWith(1, false)
        expect(logSpy).toHaveBeenNthCalledWith(2, 0)
      })
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(or true b (+ c d))'))).toEqual(new Set(['b', 'c', 'd']))
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

  describe('defn', () => {
    it('samples', () => {
      expect(lits.run('(defn add [a b] (+ a b)) (add 1 2)')).toBe(3)
      expect(lits.run('(defn add [a b &let [x 10]] (+ a b x)) (add 1 2)')).toBe(13)
      expect(() => lits.run('(defn add [a b & &let [x 10]] (+ a b x)) (add 1 2)')).toThrow()
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
      expect(() => lits.run('(defn add ([a b] (++ a b)) ([a] 10)) (+ (add 1 2) (add 1))')).toThrow()
      expect(() => lits.run('(defn add ([a b] (+ a b)) ([a] 10)] (+ (add 1 2) (add 1))')).toThrow()
      expect(() => lits.run('(defn add ([a b] (+ a b)] ([a] 10)) (+ (add 1 2) (add 1))')).toThrow()
      expect(() => lits.run('(defn add ([a b] (+ a b)) ([a b] 10))')).toThrow()
      expect(() => lits.run('(defn add ([a b] (+ a b)) ([a b & rest] 10))')).toThrow()
      expect(() => lits.run('(defn add ([a b & rest] (+ a b)) ([a b] 10))')).toThrow()
      expect(() => lits.run('(defn add ([a b & rest] (+ a b)) ([a b c & rest] 10))')).toThrow()
    })
    it('call defn function', () => {
      expect(lits.run('(defn sumOneToN [n] (if (<= n 1) n (+ n (sumOneToN (- n 1))))) (sumOneToN 10)')).toBe(55)
      expect(lits.run('(defn applyWithVal [fun val] (fun val)) (applyWithVal inc 10)')).toBe(11)
      expect(lits.run('(defn applyWithVal [fun val] (fun val)) (applyWithVal inc 10)')).toBe(11)
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(defn foo [a] (if (= a 1) 1 (+ a (foo (dec a)))))'))).toEqual(
          new Set(),
        )
        expect(
          getUndefinedSymbolNames(lits.analyze('(defn foo [a &let [x y, y z]] (if (= a 1) 1 (+ a (foo (dec a)))))')),
        ).toEqual(new Set(['y', 'z']))
        expect(getUndefinedSymbolNames(lits.analyze('(defn foo [a b] (str a b c))'))).toEqual(new Set(['c']))
        expect(getUndefinedSymbolNames(lits.analyze('(defn foo [a b] (str a b c)) (foo x y)'))).toEqual(
          new Set(['c', 'x', 'y']),
        )
        expect(getUndefinedSymbolNames(lits.analyze('(defn add ([a b & rest] (+ a b)) ([a] 10))'))).toEqual(new Set())
      })
    })
  })

  describe('defns', () => {
    it('samples', () => {
      expect(lits.run('(defns (str :a :d :d) [a b] (+ a b)) (add 1 2)')).toBe(3)
      expect(() => lits.run('(defns "add" [] 10)')).not.toThrow()
      expect(() => lits.run('(defns true [] 10)')).toThrow()
      expect(() => lits.run('(defns false [] 10)')).toThrow()
      expect(() => lits.run('(defns nil [] 10)')).toThrow()
      expect(() => lits.run('(defns add [:s] 10)')).toThrow()
      expect(() => lits.run('(defns add 1 (+ a b))')).toThrow()
      expect(() => lits.run('(defns add (a b))')).toThrow()
    })
    it('call defns function', () => {
      expect(lits.run('(defns "sumOneToN" [n] (if (<= n 1) n (+ n (sumOneToN (- n 1))))) (sumOneToN 10)')).toBe(55)
      expect(lits.run('(defns "applyWithVal" [fun val] (fun val)) (applyWithVal inc 10)')).toBe(11)
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(defns :foo [a] (if (= a 1) 1 (+ a (foo (dec a)))))'))).toEqual(
          new Set([]),
        )
        expect(getUndefinedSymbolNames(lits.analyze('(defns :foo [a b] (str a b c))'))).toEqual(new Set(['c']))
        expect(getUndefinedSymbolNames(lits.analyze('(defns :foo [a b] (str a b c)) (foo x y)'))).toEqual(
          new Set(['c', 'x', 'y']),
        )
        expect(getUndefinedSymbolNames(lits.analyze('(defns :add ([a b & rest] (+ a b)) ([a] 10))'))).toEqual(new Set())
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
        expect(getUndefinedSymbolNames(lits.analyze('(fn ([a b & rest] (+ a b)) ([a] 10))'))).toEqual(new Set())
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

  describe('when', () => {
    it('samples', () => {
      expect(lits.run('(when true (write! 10) (write! 20))')).toBe(20)
      expect(lits.run('(when "Charles" (write! 10) (write! 20))')).toBe(20)
      expect(lits.run('(when false)')).toBeNull()
      expect(lits.run('(when true)')).toBeNull()
      expect(() => lits.run('(when)')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(when (/ a b) f g h)'))).toEqual(
          new Set(['a', 'b', 'f', 'g', 'h']),
        )
      })
    })
  })

  describe('when-not', () => {
    it('samples', () => {
      expect(lits.run('(when-not true (write! 10) (write! 20))')).toBeNull()
      expect(lits.run('(when-not (> 10 20) (write! 10) (write! 20))')).toBe(20)
      expect(lits.run('(when-not false)')).toBeNull()
      expect(lits.run('(when-not true)')).toBeNull()
      expect(() => lits.run('(when-not)')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(when-not (/ a b) f g h)'))).toEqual(
          new Set(['a', 'b', 'f', 'g', 'h']),
        )
      })
    })
  })

  describe('when-first', () => {
    it('samples', () => {
      expect(lits.run('(when-first [x [1 2 3]] (write! 10) (write! 20) x)')).toBe(1)
      expect(lits.run('(when-first [x []] (write! 10) (write! 20) x)')).toBeNull()
      expect(lits.run('(when-first [x "Albert"] (write! 10) (write! 20) x)')).toBe('A')
      expect(lits.run('(when-first [x ""] (write! 10) (write! 20) x)')).toBeNull()
      expect(lits.run('(when-first [x [0]])')).toBeNull()
      expect(() => lits.run('(when-first [x nil] x)')).toThrow()
      expect(() => lits.run('(when-first [x nil a 2] x)')).toThrow()
      expect(() => lits.run('(when-first [] x)')).toThrow()
      expect(() => lits.run('(when-first x 10)')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(when-first [x "Albert"] x)'))).toEqual(new Set())
        expect(getUndefinedSymbolNames(lits.analyze('(when-first [x b] x y)'))).toEqual(new Set(['b', 'y']))
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
      lits.run('(defn foo [n] (write! n) (when (not (zero? n)) (recur (dec n)))) (foo 3)')
      expect(logSpy).toHaveBeenNthCalledWith(1, 3)
      expect(logSpy).toHaveBeenNthCalledWith(2, 2)
      expect(logSpy).toHaveBeenNthCalledWith(3, 1)
      expect(logSpy).toHaveBeenNthCalledWith(4, 0)
    })
    it('recur must be called with the right number of parameters', () => {
      expect(() => lits.run('(defn foo [n] (write! n) (when (not (zero? n)) (recur))) (foo 3)')).toThrow()
      expect(() => lits.run('(defn foo [n] (write! n) (when (not (zero? n)) (recur (dec n)))) (foo 3)')).not.toThrow()
      expect(() => lits.run('(defn foo [n] (write! n) (when (not (zero? n)) (recur (dec n) 1))) (foo 3)')).toThrow()
      expect(() => lits.run('(defn foo [n] (write! n) (when (not (zero? n)) (recur (dec n) 1 2))) (foo 3)')).toThrow()
      expect(() => lits.run('((fn [n] (write! n) (when (not (zero? n)) (recur))) 3)')).toThrow()
      expect(() => lits.run('((fn [n] (write! n) (when (not (zero? n)) (recur (dec n)))) 3)')).not.toThrow()
      expect(() => lits.run('((fn [n] (write! n) (when (not (zero? n)) (recur (dec n) 1))) 3)')).toThrow()
      expect(() => lits.run('((fn [n] (write! n) (when (not (zero? n)) (recur (dec n) 1 2))) 3)')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        const lits2 = new Lits()

        expect(
          findUnresolvedIdentifiers(lits2.parse(lits2.tokenize('((fn [n] (write! n) (when (not (zero? n)) (recur (dec n)))) 3)')).b, createContextStack(), builtin),
        ).toEqual(new Set())
        expect(
          findUnresolvedIdentifiers(lits2.parse(lits2.tokenize('((fn [n] (write! n) (when (not (zero? n)) (recur (- n a)))) 3)')).b, createContextStack(), builtin),
        ).toEqual(new Set([{ symbol: 'a' }]))
      })
    })
  })

  describe('loop', () => {
    it('should work with recur', () => {
      lits.run('(loop [n 3] (write! n) (when (not (zero? n)) (recur (dec n))))')
      expect(logSpy).toHaveBeenNthCalledWith(1, 3)
      expect(logSpy).toHaveBeenNthCalledWith(2, 2)
      expect(logSpy).toHaveBeenNthCalledWith(3, 1)
      expect(logSpy).toHaveBeenNthCalledWith(4, 0)
    })
    it('recur must be called with right number of parameters', () => {
      expect(() => lits.run('(loop [n 3] (write! n) (when (not (zero? n)) (recur (dec n) 2)))')).toThrow()
      expect(() => lits.run('(loop [n 3] (write! n) (when (not (zero? n)) (recur)))')).toThrow()
    })
    it('throw should work', () => {
      expect(() => lits.run('(loop [n 3] (write! n) (when (not (zero? n)) (throw (dec n))))')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(
          getUndefinedSymbolNames(lits.analyze('(loop [n 3] (write! n) (when (not (zero? n)) (recur (dec n))))')),
        ).toEqual(new Set())
        expect(
          getUndefinedSymbolNames(lits.analyze('(loop [n 3] (write! x n) (when (not (zero? n)) (recur (dec n))))')),
        ).toEqual(new Set(['x']))
        expect(
          getUndefinedSymbolNames(
            lits.analyze('(loop [n (+ 3 y)] (write! x n) (when (not (zero? n)) (recur (dec n))))'),
          ),
        ).toEqual(new Set(['x', 'y']))
      })
    })
  })
  describe('time!', () => {
    it('samples', () => {
      expect(lits.run('(time! (+ 1 2))')).toBe(3)
      expect(lastLog).toMatch(/Elapsed time: \d+ ms/)
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('(time! (foo x z))'))).toEqual(new Set(['foo', 'x', 'z']))
      })
    })
  })

  describe('doseq', () => {
    it('samples', () => {
      expect(lits.run('(doseq [x []] x)')).toBeNull()
      expect(lits.run('(doseq [x [1 2 3] y []] x)')).toBeNull()
      expect(lits.run('(doseq [x [] y [1 2 3]] x)')).toBeNull()

      expect(lits.run('(doseq [x "Al" y [1 2]] (repeat y x))')).toBeNull()
      expect(lits.run('(doseq [x {:a 10 :b 20} y [1 2]] (repeat y x))')).toBeNull()

      expect(lits.run('(doseq [x [1 2] y [1 10]] (* x y))')).toBeNull()
      expect(lits.run('(doseq [x [1 2] &let [z (* x x x)]] z)')).toBeNull()
      expect(lits.run('(doseq [x [1 2] y [x (* 2 x)]] (* x y))')).toBeNull()

      expect(lits.run('(doseq [x [0 1 2 3 4 5] &let [y (* x 3)] &when (even? y)] y)')).toBeNull()
      expect(lits.run('(doseq [x [0 1 2 3 4 5] &let [y (* x 3)] &while (even? y)] y)')).toBeNull()

      expect(lits.run('(doseq [x [1 2 3] y [1 2 3] &while (<= x y) z [1 2 3]] [x y z])')).toBeNull()
      expect(lits.run('(doseq [x [1 2 3] y [1 2 3] z [1 2 3] &while (<= x y)] [x y z])')).toBeNull()
      expect(() => lits.run('(doseq [x [0 1 2 3 4 5] & [y (* x 3)] &while (even? y)] y)')).toThrow()
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

      expect(lits.run('(for [x "Al" y [1 2]] (repeat y x))')).toEqual([['A'], ['A', 'A'], ['l'], ['l', 'l']])
      expect(lits.run('(for [x {:a 10 :b 20} y [1 2]] (repeat y x))')).toEqual([
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
      expect(() => lits.run('(for [x [0 1 2 3 4 5] & [y (* x 3)] &while (even? y)] y)')).toThrow()
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

  describe('declared?', () => {
    it('samples', () => {
      expect(lits.run('(declared? foo)')).toBe(false)
      expect(lits.run('(def foo :foo) (declared? foo)')).toBe(true)
      expect(lits.run('(declared? +)')).toBe(true)
      expect(lits.run('(def foo nil) (declared? foo)')).toBe(true)
      expect(lits.run('(declared? if)')).toBe(true)

      expect(() => lits.run('(declared?)')).toThrow()
      expect(() => lits.run('(declared? foo bar)')).toThrow()
    })
  })

  describe('unresolvedIdentifiers', () => {
    it('samples', () => {
      expect(getUndefinedSymbolNames(lits.analyze('(declared? x)'))).toEqual(new Set(['x']))
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
