import { describe, expect, it } from 'vitest'
import { Lits } from '..'
import { createContextStack } from '../evaluator/ContextStack'
import { isUnknownRecord } from '../typeGuards'
import { FUNCTION_SYMBOL } from '../utils/symbols'
import { calculateOutcomes } from './calculateOutcomes'

const lits = new Lits()

type TestSample = [string, null | unknown[]]
describe('calculateOutcomes.', () => {
  describe('calculateAndOutcomes.', () => {
    testSamples([
      ['(and)', [true]],
      ['(and true false)', [false]],
      ['(and false true)', [false]],
      ['(and 1 2)', [2]],
      ['(and (rand!) 2)', null],
      ['(and (if (> (rand!) 0.5) 1 2) 3)', [3]],
      ['(and (if (> (rand!) 0.5) 0 1) 2)', [0, 2]],
    ])
  })

  describe('calculateCondOutcomes.', () => {
    testSamples([
      ['(cond 1 :1, 2 :2, 3 :3)', ['1']],
      ['(cond (> (rand!) 0.5) :1, 2 :2, 3 :3)', ['1', '2', '3', null]],
      [`(def x (if (> (rand!) 0.5) 1 2))
        (cond
          (= x 1) 1
          (= x 2) 2
          (= x 3) 3)`, [1, 2]],
      ['(cond (> (rand!) 0.5) :1, (> (rand!) 0.5) :2)', ['1', '2', null]],
      ['(cond)', [null]],
    ])
  })

  describe('calculateDeclaredOutcomes.', () => {
    testSamples([
      ['(declared? foo)', [true, false]],
      ['(def foo nil) (declared? foo)', [true]],
    ])
  })

  describe('calculateDefOutcomes.', () => {
    testSamples([
      ['(def foo :bar)', [null]],
      ['(def foo :bar) foo', ['bar']],
      ['(def foo (if (> (rand!) 0.5) 1 2)) foo', [1, 2]],
    ])
  })

  describe('calculateDefsOutcomes.', () => {
    testSamples([
      ['(defs :foo :bar)', [null]],
      ['(defs :foo :bar) foo', ['bar']],
      ['(defs (str :f :o :o) (if (> (rand!) 0.5) 1 2)) foo', [1, 2]],
      ['(def a 10) (+ a 10)', [20]],
    ])
  })

  describe('calculateDoOutcomes.', () => {
    testSamples([
      [`(do 
          (def a (if (> (rand!) 0.5) 1 2))
          (def b (if (> (rand!) 0.5) 10 20))
          (+ a b)
      )`, [11, 21, 12, 22]],
    ])
  })

  describe('calculateForOutcomes.', () => {
    testSamples([
      [`(for [x [1 2] y [1 2]]
          (str x y))`, [['11', '12', '21', '22']]],
      [`(for [x [(if (> (rand!) 0.5) 1 2) 2] y [1 2]]
          (str x y))`, null],
      [`(for [x [1 2] y [1 2] &when (> x (if (> (rand!) 0.5) 1 2))]
        (str x y))`, null],
      [`(for [x [1 2] y [1 2] &while (> x (if (> (rand!) 0.5) 1 2))]
        (str x y))`, null],
      [`(for [x [1 2] y [1 2] &let [z (if (> (rand!) 0.5) 1 2)]]
        (str x y))`, null],
      [`(for [x [1 2] y [1 2]]
          (str x y (if (> (rand!) 0.5) 1 2)))`, null],
    ])
  })

  describe('calculateDoSeqOutcomes.', () => {
    testSamples([
      [`(doseq [x [1 2] y [1 2]]
          (str x y))`, [null]],
      [`(doseq [x [(if (> (rand!) 0.5) 1 2) 2] y [1 2]]
          (str x y))`, null],
      [`(doseq [x [1 2] y [1 2]]
          (str x y (if (> (rand!) 0.5) 1 2)))`, null],
    ])
  })

  describe('calculateIfOutcomes.', () => {
    testSamples([
      ['(if true "heads")', ['heads']],
      ['(if false "heads")', [null]],
      ['(if true "heads" "tails")', ['heads']],
      ['(if false "heads" "tails")', ['tails']],
      ['(if foo "heads" "tails")', ['heads', 'tails']],
      ['(if (> (rand!) 0.5) "heads" "tails")', ['heads', 'tails']],
      [`(if (> (rand!) 0.5)
          (if (> (rand!) 0.5) 1 2)
          (if (> (rand!) 0.5) 3 4))`, [1, 2, 3, 4]],
    ])
  })

  describe('calculateIfNotOutcomes.', () => {
    testSamples([
      ['(if-not true "heads" "tails")', ['tails']],
      ['(if-not false "heads" "tails")', ['heads']],
      ['(if-not true "heads")', [null]],
      ['(if-not false "heads")', ['heads']],
      ['(if-not foo "heads" "tails")', ['heads', 'tails']],
      ['(if-not (> (rand!) 0.5) "heads" "tails")', ['heads', 'tails']],
      [`(if-not (> (rand!) 0.5)
          (if-not (> (rand!) 0.5) 1 2)
          (if-not (> (rand!) 0.5) 3 4))`, [1, 2, 3, 4]],
    ])
  })

  describe('calculateIfLetOutcomes.', () => {
    testSamples([
      ['(if-let [x true] x 0)', [true]],
      ['(if-let [x false] x 0)', [0]],
      ['(if-let [x foo] x 0)', null],
      ['(if-let [x foo] 1 0)', [1, 0]],
    ])
  })

  describe('calculateLetOutcomes.', () => {
    testSamples([
      ['(let [] :foo)', ['foo']],
      ['(let [foo :bar] foo)', ['bar']],
      ['(let [foo baz] foo)', null],
      ['(let [foo baz] :foo)', ['foo']],
      ['(let [foo (if (> (rand!) 0.5) 1 2)] foo)', [1, 2]],
      ['(let [foo (if (> (rand!) 0.5) 1 2)] (if (> (rand!) 0.5) foo 3))', [1, 3, 2]],
      [`(let [foo (if (> (rand!) 0.5) 1 2)]
          foo
        )`, [1, 2]],
      [`(let [foo (if (> (rand!) 0.5) 1 2)]
          (if (not foo) 1 2)
        )`, [2]],
      [`(let [foo (if-not (> (rand!) 0.5) 1 2)]
          (if-not (not foo) 1 2)
      )`, [1]],
    ])
  })

  describe('calculateLoopOutcomes.', () => {
    testSamples([
      [`(loop [n 3]
          (when
            (not (zero? n))
            (recur (dec n))))`, [null]],
      [`(loop [n 2]
          (if (< n 100)
            (recur
              (* n n))
            n))`, [256]],
    ])
  })

  describe('calculateOrOutcomes.', () => {
    testSamples([
      ['(or)', [false]],
      ['(or true false)', [true]],
      ['(or false true)', [true]],
      ['(or 1 2)', [1]],
      ['(or (rand!) 2)', null],
      ['(or (if (> (rand!) 0.5) 1 2) 3)', [1, 2]],
      ['(or (if (> (rand!) 0.5) 0 1) 2)', [2, 1]],
    ])
  })

  describe('calculateQqOutcomes.', () => {
    testSamples([
      ['(?? foo)', null],
      ['(def foo true) (?? foo)', [true]],
      ['(def foo true) (?? foo (rand!))', null],
      ['(def foo true) (?? foo 0)', [true]],
      ['(def foo nil) (?? foo 0)', [0]],
    ])
  })

  describe('calculateTryOutcomes.', () => {
    testSamples([
      // ['(throw :A)', [Error]],
      // ['(try 1 (catch e 2))', [1]],
      ['(try (throw :A) (catch e :X))', ['X']],
      // ['(try (throw :A) (catch e e))', [Error]],
      // [`(try
      //     (if
      //       (> (rand!) 0.5)
      //       (throw (if (> (rand!) 0.5) :A :B))
      //       :A)
      //     (catch e e))`, [Error, Error, 'A']],
    ])
  })

  describe('calculateTimeOutcomes.', () => {
    testSamples([
      ['(time! :A)', ['A']],
    ])
  })

  describe('calculateRecurOutcomes.', () => {
    testSamples([
      [`(defn foo [n]
          (if (< n 100)
            (recur
              (* n n))
            n))
        (foo 3)`, [6561]],
    ])
  })

  describe('calculateWhenFirstOutcomes.', () => {
    testSamples([
      ['(when-first [x [true false]] x 1)', [1]],
      ['(when-first [x [true false]] 1 x)', [true]],
      ['(when-first [x [(if (> (rand!) 0.5) 1 2) false]] 0 x)', [1, 2]],
      ['(when-first [x [(rand!) false]] 0 x)', null],
    ])
  })

  describe('calculateWhenOutcomes.', () => {
    testSamples([
      ['(when true 1)', [1]],
      ['(when false 1)', [null]],
      ['(when foo 1)', [1, null]],
      ['(when foo (if (> (rand!) 0.5) 1 2))', [1, 2, null]],
    ])
  })

  describe('calculateWhenNotOutcomes.', () => {
    testSamples([
      ['(when-not false 0 1)', [1]],
      ['(when-not true 0 1)', [null]],
      ['(when-not foo 0 1)', [1, null]],
      ['(when-not foo 0 (if (> (rand!) 0.5) 1 2))', [1, 2, null]],
    ])
  })

  describe('calculateWhenLetOutcomes.', () => {
    testSamples([
      ['(when-let [x true] 0 x)', [true]],
      ['(when-let [x false] 0 x)', [null]],
      ['(when-let [x foo] 0 x)', null],
      ['(when-let [x foo] 0 1)', [1, null]],

    ])
  })

  describe('calculate function outcomes.', () => {
    describe('calculateDefnOutcomes.', () => {
      testSamples([
        ['(defn foo [] :bar)', [null]],
        [`(defn foo [] :bar)
          (foo)`, ['bar']],
        [`(defn foo [] :bar)
          (if foo 1 2)`, [1]],
        [`(defn foo
            ([] (if (> (rand!) 0.5) "No parameters" "0 parameters"))
            ([x] "One parameter")
            ([x y] "Two parameters")
            ([x y z] "Three parameters")
            ([x y z zz & rest] "Four or more parameters"))
          (foo)`, ['No parameters', '0 parameters']],
        [`(defn foo
            ([] (if (> (rand!) 0.5) "No parameters" "0 parameters"))
            ([x] "One parameter")
            ([x y] "Two parameters")
            ([x y z] "Three parameters")
            ([x y z zz & rest] "Four or more parameters"))
          (foo 1 2)`, ['Two parameters']],
        [`(defn foo
            ([] (if (> (rand!) 0.5) "No parameters" "0 parameters"))
            ([x] "One parameter")
            ([x y] "Two parameters")
            ([x y z] "Three parameters")
            ([x y z zz & rest] (if (> (rand!) 0.5) "Four or more parameters" "Many parameters")))
          (foo 1 2 3 4 5 6)`, ['Four or more parameters', 'Many parameters']],
      ])
    })

    describe('calculateDefnsOutcomes.', () => {
      testSamples([
        ['(defns :foo [] :bar)', [null]],
        [`(defns :foo [] :bar)
        (foo)`, ['bar']],
        [`(defns :foo
          ([] (if (> (rand!) 0.5) "No parameters" "0 parameters"))
          ([x] "One parameter")
          ([x y] "Two parameters")
          ([x y z] "Three parameters")
          ([x y z zz & rest] "Four or more parameters"))
        (foo)`, ['No parameters', '0 parameters']],
        [`(defns :foo
          ([] (if (> (rand!) 0.5) "No parameters" "0 parameters"))
          ([x] "One parameter")
          ([x y] "Two parameters")
          ([x y z] "Three parameters")
          ([x y z zz & rest] "Four or more parameters"))
        (foo 1 2)`, ['Two parameters']],
        [`(defns (str :f :o :o)
          ([] (if (> (rand!) 0.5) "No parameters" "0 parameters"))
          ([x] "One parameter")
          ([x y] "Two parameters")
          ([x y z] "Three parameters")
          ([x y z zz & rest] (if (> (rand!) 0.5) "Four or more parameters" "Many parameters")))
        (foo 1 2 3 4 5 6)`, ['Four or more parameters', 'Many parameters']],
      ])
    })

    describe('calculateFnOutcomes.', () => {
      testSamples([
        ['(fn [] :bar)', [Function]],
        ['((fn [] :bar))', ['bar']],
        [`((fn
          ([] (if (> (rand!) 0.5) "No parameters" "0 parameters"))
          ([x] "One parameter")
          ([x y] "Two parameters")
          ([x y z] "Three parameters")
          ([x y z zz & rest] "Four or more parameters")))`, ['No parameters', '0 parameters']],
        [`((fn
          ([] (if (> (rand!) 0.5) "No parameters" "0 parameters"))
          ([x] "One parameter")
          ([x y] "Two parameters")
          ([x y z] "Three parameters")
          ([x y z zz & rest] "Four or more parameters")) 1 2)`, ['Two parameters']],
        [`((fn
          ([] (if (> (rand!) 0.5) "No parameters" "0 parameters"))
          ([x] "One parameter")
          ([x y] "Two parameters")
          ([x y z] "Three parameters")
          ([x y z zz & rest] (if (> (rand!) 0.5) "Four or more parameters" "Many parameters"))) 1 2 3 4 5 6)`, ['Four or more parameters', 'Many parameters']],
      ])
    })
  })

  describe('misc.', () => {
    testSamples([
      // We cannot compute outcomes, because the function is recursive
      [`(defn factorial [x]
          (if (= x 1)
            1
            (* x (factorial (dec x)))
          )
        )

        (factorial 5)`, [120]],
      [`(def l [7 39 45 0 23 1 50 100 12 -5])
      (defn numberComparer [a b]
        (cond
          (< a b) -1
          (> a b) 1
          :else 0
        )
      )
      
      (sort numberComparer l)`, [[
        -5,
        0,
        1,
        7,
        12,
        23,
        39,
        45,
        50,
        100,
      ]]],
      [`(let [foo (if (> (rand!) 0.5) 1 2)]
          (if (not foo) 1 2)
        )`, [2]],
      // We cannot compute outcomes, because the expression is missing an end parenthesis
      ['(ifs false "heads" "tails")', null],
    ])
  })
})

function testSamples(samples: TestSample[]) {
  samples.forEach(([program, expectedOutcomes]) => {
    it(`${program} -> ${JSON.stringify(expectedOutcomes)}`, () => {
      const tokens = lits.tokenize(program)
      const ast = lits.parse(tokens)
      const contextStack = createContextStack()
      if (expectedOutcomes === null) {
        expect(calculateOutcomes(contextStack.clone(), ast.b)).toBeNull()
      }

      else {
        const outcomes = calculateOutcomes(contextStack, ast.b)!.map((outcome) => {
          return isUnknownRecord(outcome) && outcome[FUNCTION_SYMBOL] === true
            ? Function
            : outcome instanceof Error
              ? Error
              : outcome
        })
        expect(outcomes).toEqual(expectedOutcomes)
      }
    })
  })
}
