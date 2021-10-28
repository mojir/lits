/* eslint-disable no-console */
import { Lispish } from '../../../src'
import { AssertionError } from '../../../src/errors'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`misc functions`, () => {
  let oldLog: () => void
  let oldWarn: () => void
  let lastLog: unknown
  let logSpy: (...args: unknown[]) => void
  beforeEach(() => {
    oldLog = console.log
    oldWarn = console.warn
    logSpy = jest.fn()
    console.log = (...args) => {
      logSpy(...args)
      lastLog = args[0]
    }
    console.warn = (...args) => {
      lastLog = args[0]
    }
  })
  afterEach(() => {
    console.log = oldLog
    console.warn = oldWarn
  })
  describe(`inst-ms`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(inst-ms 1)`)).toThrow()
      expect(() => lispish.run(`(inst-ms :x)`)).toThrow()
      expect(lispish.run(`(inst-ms)`)).toBeLessThanOrEqual(Date.now())
    })
  })

  describe(`not=`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(not=)`)).toThrow()
      expect(lispish.run(`(not= 1)`)).toBe(true)
      expect(lispish.run(`(not= 1 1)`)).toBe(false)
      expect(lispish.run(`(not= 1 2)`)).toBe(true)
      expect(lispish.run(`(not= 1 2 1)`)).toBe(false)
      expect(lispish.run(`(not= 1 2 3)`)).toBe(true)
      expect(lispish.run(`(not= :1)`)).toBe(true)
      expect(lispish.run(`(not= :1 :1)`)).toBe(false)
      expect(lispish.run(`(not= :1 :2)`)).toBe(true)
      expect(lispish.run(`(not= :1 :2 :1)`)).toBe(false)
      expect(lispish.run(`(not= :1 :2 3)`)).toBe(true)
      expect(lispish.run(`(not= nil 0)`)).toBe(true)
      expect(lispish.run(`(not= 1 true 3)`)).toBe(true)
      expect(lispish.run(`(not= 1 false 3)`)).toBe(true)
    })
  })

  describe(`equal?`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(not=)`)).toThrow()
      expect(lispish.run(`(equal? 1 1)`)).toBe(true)
      expect(lispish.run(`(equal? 1 2)`)).toBe(false)
      expect(lispish.run(`(equal? :1 :1)`)).toBe(true)
      expect(lispish.run(`(equal? :1 :2)`)).toBe(false)
      expect(lispish.run(`(equal? nil 0)`)).toBe(false)
      expect(lispish.run(`(equal? [1 2 {:a 10 :b [nil]}] [1 2 {:b [nil] :a 10}])`)).toBe(true)
      expect(lispish.run(`(equal? [1 2 {:a 10 :b [nil]}] [1 2 {:b [0] :a 10}])`)).toBe(false)
    })
  })

  describe(`=`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(=)`)).toThrow()
      expect(lispish.run(`(= 1)`)).toBe(true)
      expect(lispish.run(`(= 1 1)`)).toBe(true)
      expect(lispish.run(`(= 1 2)`)).toBe(false)
      expect(lispish.run(`(= 1 2 1)`)).toBe(false)
      expect(lispish.run(`(= 1 2 3)`)).toBe(false)
      expect(lispish.run(`(= :1)`)).toBe(true)
      expect(lispish.run(`(= :1 :1)`)).toBe(true)
      expect(lispish.run(`(= :1 :2)`)).toBe(false)
      expect(lispish.run(`(= :1 :2 :1)`)).toBe(false)
      expect(lispish.run(`(= :1 :2 :3)`)).toBe(false)
      expect(lispish.run(`(= :2 :2 :2)`)).toBe(true)
      expect(lispish.run(`(= 1 :2 3)`)).toBe(false)
      expect(lispish.run(`(= 1 nil 3)`)).toBe(false)
      expect(lispish.run(`(= 1 true 3)`)).toBe(false)
      expect(lispish.run(`(= 1 false 3)`)).toBe(false)
      expect(lispish.run(`(= nil nil)`)).toBe(true)
      expect(lispish.run(`(= true true)`)).toBe(true)
      expect(lispish.run(`(= false false)`)).toBe(true)
    })

    test(`Object equality`, () => {
      const program = `
        (def obj1 (object :x 10))
        (def obj2 (object :x 10))
        [(= obj1 obj1) (= obj1 obj2)]
      `
      expect(lispish.run(program)).toEqual([true, false])
    })

    test(`Array equality`, () => {
      const program = `
        (def array1 [1 2 3])
        (def array2 [1 2 3])
        [(= array1 array1) (= array1 array2)]
      `
      expect(lispish.run(program)).toEqual([true, false])
    })
  })

  describe(`>`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(>)`)).toThrow()
      expect(lispish.run(`(> 1)`)).toBe(true)
      expect(lispish.run(`(> 1 2)`)).toBe(false)
      expect(lispish.run(`(> 1 1)`)).toBe(false)
      expect(lispish.run(`(> 2 1)`)).toBe(true)
      expect(lispish.run(`(> 2 1 2)`)).toBe(false)
      expect(lispish.run(`(> 2 1 0)`)).toBe(true)
      expect(lispish.run(`(> "albert" "ALBERT")`)).toBe(true)
      expect(lispish.run(`(> "ALBERT" "albert")`)).toBe(false)
      expect(lispish.run(`(> "albert" "alber")`)).toBe(true)
      expect(lispish.run(`(> "albert" "albert")`)).toBe(false)
      expect(lispish.run(`(> "alber" "albert")`)).toBe(false)

      expect(lispish.run(`(> :1)`)).toBe(true)
      expect(lispish.run(`(> :1 :2)`)).toBe(false)
      expect(lispish.run(`(> :1 :1)`)).toBe(false)
      expect(lispish.run(`(> :2 :1)`)).toBe(true)
      expect(lispish.run(`(> :2 :1 :2)`)).toBe(false)
      expect(lispish.run(`(> :2 :1 0)`)).toBe(true)
    })
  })

  describe(`<`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(<)`)).toThrow()
      expect(lispish.run(`(< 1)`)).toBe(true)
      expect(lispish.run(`(< 1 2)`)).toBe(true)
      expect(lispish.run(`(< 1 1)`)).toBe(false)
      expect(lispish.run(`(< 2 1)`)).toBe(false)
      expect(lispish.run(`(< 1 2 1)`)).toBe(false)
      expect(lispish.run(`(< 0 1 2)`)).toBe(true)
      expect(lispish.run(`(< "albert" "ALBERT")`)).toBe(false)
      expect(lispish.run(`(< "ALBERT" "albert")`)).toBe(true)
      expect(lispish.run(`(< "albert" "alber")`)).toBe(false)
      expect(lispish.run(`(< "albert" "albert")`)).toBe(false)
      expect(lispish.run(`(< "alber" "albert")`)).toBe(true)

      expect(lispish.run(`(< :1)`)).toBe(true)
      expect(lispish.run(`(< :1 :2)`)).toBe(true)
      expect(lispish.run(`(< :1 :1)`)).toBe(false)
      expect(lispish.run(`(< :2 :1)`)).toBe(false)
      expect(lispish.run(`(< :1 :2 :1)`)).toBe(false)
      expect(lispish.run(`(< 0 :1 :2)`)).toBe(true)
    })
  })

  describe(`>=`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(>=)`)).toThrow()
      expect(lispish.run(`(>= 1)`)).toBe(true)
      expect(lispish.run(`(>= 1 2)`)).toBe(false)
      expect(lispish.run(`(>= 1 1)`)).toBe(true)
      expect(lispish.run(`(>= 2 1)`)).toBe(true)
      expect(lispish.run(`(>= 2 1 2)`)).toBe(false)
      expect(lispish.run(`(>= 2 1 1)`)).toBe(true)
      expect(lispish.run(`(>= "albert" "ALBERT")`)).toBe(true)
      expect(lispish.run(`(>= "ALBERT" "albert")`)).toBe(false)
      expect(lispish.run(`(>= "albert" "alber")`)).toBe(true)
      expect(lispish.run(`(>= "albert" "albert")`)).toBe(true)
      expect(lispish.run(`(>= "alber" "albert")`)).toBe(false)

      expect(lispish.run(`(>= :1)`)).toBe(true)
      expect(lispish.run(`(>= :1 :2)`)).toBe(false)
      expect(lispish.run(`(>= :1 :1)`)).toBe(true)
      expect(lispish.run(`(>= :2 :1)`)).toBe(true)
      expect(lispish.run(`(>= :2 :1 :2)`)).toBe(false)
      expect(lispish.run(`(>= :2 :1 :1)`)).toBe(true)
    })
  })

  describe(`<=`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(<=)`)).toThrow()
      expect(lispish.run(`(<= 1)`)).toBe(true)
      expect(lispish.run(`(<= 1 2)`)).toBe(true)
      expect(lispish.run(`(<= 1 1)`)).toBe(true)
      expect(lispish.run(`(<= 2 1)`)).toBe(false)
      expect(lispish.run(`(<= 1 2 1)`)).toBe(false)
      expect(lispish.run(`(<= 1 2 2)`)).toBe(true)
      expect(lispish.run(`(<= "albert" "ALBERT")`)).toBe(false)
      expect(lispish.run(`(<= "ALBERT" "albert")`)).toBe(true)
      expect(lispish.run(`(<= "albert" "alber")`)).toBe(false)
      expect(lispish.run(`(<= "albert" "albert")`)).toBe(true)
      expect(lispish.run(`(<= "alber" "albert")`)).toBe(true)

      expect(lispish.run(`(<= :1)`)).toBe(true)
      expect(lispish.run(`(<= :1 :2)`)).toBe(true)
      expect(lispish.run(`(<= :1 :1)`)).toBe(true)
      expect(lispish.run(`(<= :2 :1)`)).toBe(false)
      expect(lispish.run(`(<= :1 :2 :1)`)).toBe(false)
      expect(lispish.run(`(<= :1 :2 :2)`)).toBe(true)
    })
  })

  describe(`not`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(not)`)).toThrow()
      expect(lispish.run(`(not 0)`)).toBe(true)
      expect(lispish.run(`(not "")`)).toBe(true)
      expect(lispish.run(`(not :0)`)).toBe(false)
      expect(lispish.run(`(not 1)`)).toBe(false)
      expect(lispish.run(`(not -1)`)).toBe(false)
      expect(lispish.run(`(not [])`)).toBe(false)
      expect(lispish.run(`(not false)`)).toBe(true)
      expect(lispish.run(`(not true)`)).toBe(false)
      expect(lispish.run(`(not nil)`)).toBe(true)
      expect(() => lispish.run(`(not 0 1)`)).toThrow()
    })
  })

  describe(`write!`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(write!)`)).toBe(null)
      expect(lispish.run(`(write! 1)`)).toBe(1)
      expect(lispish.run(`(write! :1)`)).toBe(`1`)
      expect(lispish.run(`(write! 100 [] :1)`)).toBe(`1`)
      expect(lispish.run(`(write! [])`)).toEqual([])
      expect(lispish.run(`(write! (object))`)).toEqual({})
      expect(lispish.run(`(write! nil)`)).toBe(null)
      expect(lispish.run(`(write! true)`)).toBe(true)
      expect(lispish.run(`(write! false)`)).toBe(false)
    })
    test(`that it does console.log`, () => {
      lispish.run(`(write! 1)`)
      expect(logSpy).toHaveBeenCalledWith(1)
    })
  })

  describe(`get-path`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(get-path [1 2 3] "[1]")`)).toBe(2)
      expect(lispish.run(`(get-path (object :a 1) :a)`)).toBe(1)
      expect(lispish.run(`(get-path (object :a (object :b [1 2 3])) "a.b[1]")`)).toBe(2)
      expect(lispish.run(`(get-path O "a.b[1]")`, { values: { O: { a: { b: [1, 2, 3] } } } })).toBe(2)
      expect(lispish.run(`(get-path O "a.c[1]")`, { values: { O: { a: { b: [1, 2, 3] } } } })).toBe(null)
      expect(lispish.run(`(get-path O "")`, { values: { O: { a: { b: [1, 2, 3] } } } })).toEqual({
        a: { b: [1, 2, 3] },
      })
      expect(() => lispish.run(`(get-path O)`, { values: { O: { a: { b: [1, 2, 3] } } } })).toThrow()
      expect(() => lispish.run(`(get-path)`, { values: { O: { a: { b: [1, 2, 3] } } } })).toThrow()
      expect(() => lispish.run(`(get-path O :a :b)`, { values: { O: { a: { b: [1, 2, 3] } } } })).toThrow()
      expect(() => lispish.run(`(get-path (regexp "abc" :a)`)).toThrow()
    })
  })

  describe(`debug!`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(debug!)`)).toBe(null)
      expect(lispish.run(`(debug! +)`)).toBeTruthy()
      expect(() => lispish.run(`(debug! "" 0)`)).toThrow()
    })
    test(`multiple contexts`, () => {
      const context = lispish.context(`(def x 10) (defn foo [] "foo") (def bar (fn [] "bar")) (def plus +)`)
      lispish.run(`((fn [z] (debug!) (+ z 1)) 10)`, { values: { y: 20 }, contexts: [context] })
      expect(lastLog).toMatchSnapshot()
    })
    test(`debug value`, () => {
      lispish.run(`(debug! #(> %1 2))`)
      expect(lastLog).toMatchSnapshot()
    })
  })

  describe(`boolean`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(boolean 0)`)).toBe(false)
      expect(lispish.run(`(boolean 1)`)).toBe(true)
      expect(lispish.run(`(boolean "Albert")`)).toBe(true)
      expect(lispish.run(`(boolean "")`)).toBe(false)
      expect(lispish.run(`(boolean true)`)).toBe(true)
      expect(lispish.run(`(boolean false)`)).toBe(false)
      expect(lispish.run(`(boolean nil)`)).toBe(false)
      expect(lispish.run(`(boolean [])`)).toBe(true)
      expect(lispish.run(`(boolean {})`)).toBe(true)
      expect(() => lispish.run(`(boolean)`)).toThrow()
      expect(() => lispish.run(`(boolean 2 3)`)).toThrow()
    })
  })

  describe(`compare`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(compare 0 1)`)).toBe(-1)
      expect(lispish.run(`(compare 3 1)`)).toBe(1)
      expect(lispish.run(`(compare nil nil)`)).toBe(0)
      expect(lispish.run(`(compare true true)`)).toBe(0)
      expect(lispish.run(`(compare true false)`)).toBe(1)
      expect(lispish.run(`(compare false true)`)).toBe(-1)
      expect(lispish.run(`(compare [] [])`)).toBe(0)
      expect(lispish.run(`(compare [1 2 3] [2 3])`)).toBe(1)
      expect(lispish.run(`(compare [1 2] [1 2 3])`)).toBe(-1)
      expect(lispish.run(`(compare [1 2 3] [1 2 4])`)).toBe(-1)
      expect(lispish.run(`(compare :A :a)`)).toBe(-1)
      expect(lispish.run(`(compare :A :A)`)).toBe(0)
      expect(lispish.run(`(compare (regexp :A) (regexp :a))`)).toBe(-1)
      expect(lispish.run(`(compare (regexp :A) (regexp :A))`)).toBe(0)
      expect(lispish.run(`(compare (regexp :a) (regexp :A))`)).toBe(1)
      expect(lispish.run(`(compare (regexp :a) :A)`)).toBe(1)
      expect(lispish.run(`(compare {:a 1} {:a 2})`)).toBe(0)
      expect(lispish.run(`(compare {:a 1 :b 2} {:a 2})`)).toBe(1)
      expect(lispish.run(`(compare {:a 1 :b 2} {:a 2})`)).toBe(1)
      expect(lispish.run(`(compare + {:a 2})`)).toBe(1)
      expect(lispish.run(`(compare + -)`)).toBe(0)
    })
  })
  describe(`assert`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(assert false)`)).toThrowError(AssertionError)
      expect(() => lispish.run(`(assert false "Expected true")`)).toThrowError(AssertionError)
      expect(() => lispish.run(`(assert nil)`)).toThrowError(AssertionError)
      expect(() => lispish.run(`(assert 0)`)).toThrowError(AssertionError)
      expect(() => lispish.run(`(assert "")`)).toThrowError(AssertionError)
      expect(lispish.run(`(assert [])`)).toEqual([])
      expect(lispish.run(`(assert true)`)).toBe(true)
      expect(lispish.run(`(assert 1)`)).toBe(1)
      expect(lispish.run(`(assert :0)`)).toBe(`0`)
    })
  })

  describe(`lispish-version`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(lispish-version)`)).toMatch(/^\d+\.\d+\.\d+/)
      expect(() => lispish.run(`(lispish-version 1)`)).toThrow()
    })
  })

  describe(`equal?`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(equal? {:a 10 :b 20} {:b 20 :a 10})`)).toBe(true)
      expect(lispish.run(`(equal? [1 true nil] [1 true nil])`)).toBe(true)
      expect(lispish.run(`(equal? {:a 10 :b [1 2 {:b 20}]} {:b [1 2 {:b 20}] :a 10})`)).toBe(true)
      expect(lispish.run(`(equal? {:a 10 :b [1 2 {:b 20}]} {:b [1 2 {:b 21}] :a 10})`)).toBe(false)
      expect(lispish.run(`(equal? [1, 2, 3] [1, 2, 3, 4])`)).toBe(false)
      expect(lispish.run(`(equal? {:a 10} {:a 10, :b 20})`)).toBe(false)
    })
  })
})
